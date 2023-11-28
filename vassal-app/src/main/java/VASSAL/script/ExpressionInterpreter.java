/*
 *
 * Copyright (c) 2008-2023 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.script;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.configure.PropertyExpression;
import VASSAL.counters.Attachment;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Mat;
import VASSAL.counters.MatCargo;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.ReportState;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
import VASSAL.script.expression.BeanShellExpression;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.swing.DialogCloser;

import bsh.BeanShellExpressionValidator;
import bsh.EvalError;
import bsh.NameSpace;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.JDialog;
import javax.swing.SwingUtilities;

import java.awt.Point;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * A BeanShell Interpreter customised to evaluate a single Vassal
 * expression containing Vassal property references.
 * All traits with the same expression will share the same Interpreter
 *
 * Each ExpressionInterpreter has 2 levels of NameSpace:
 * 1. Top level is a single global NameSpace that contains utility methods
 *    available to all ExpressionInterpreters. It is the parent of all
 *    level 2 NameSpaces.
 * 2. Level 2 is a NameSpace for each unique expression that contains the
 *    parsed expression. All expressions in all traits that are the same
 *    will use the one Expression NameSpace.
 *
 */
public class ExpressionInterpreter extends AbstractInterpreter implements Loopable {

  private static final long serialVersionUID = 1L;
  private static final Logger logger = LoggerFactory.getLogger(ExpressionInterpreter.class);

  protected static final String INIT_SCRIPT = "/VASSAL/script/init_expression.bsh"; // NON-NLS
  protected static final String THIS = "_interp"; // NON-NLS
  protected static final String SOURCE = "_source"; // NON-NLS
  protected static final String MAGIC1 = "_xyzzy"; // NON-NLS
  protected static final String MAGIC2 = "_plugh"; // NON-NLS
  protected static final String MAGIC3 = "_plover"; // NON-NLS
  protected static final String ERROR_PREFIX = " inline evaluation of: ``_xyzzy=_plugh();''";

  // Top-level static NameSpace shared between all ExpressionInterpreters
  // Loaded with utility methods available to all interpreters
  protected static NameSpace topLevelNameSpace;

  protected NameSpace expressionNameSpace;

  //protected NameSpace localNameSpace;

  protected String expression;
  protected List<String> variables;
  protected List<String> stringVariables;

  // source is not persistent; it should be set during evaluate() only
  protected PropertySource source;
  protected AuditTrail currentAudit;
  protected Auditable currentOwner;
  @Override
  public String getComponentTypeName() {
    return Resources.getString("Editor.ExpressionInterpreter.component_type");
  }

  @Override
  public String getComponentName() {
    return Resources.getString("Editor.ExpressionInterpreter.component_type");
  }

  protected static String strip(String expr) {
    final String s = expr.trim();
    if (s.startsWith("{") && s.endsWith("}")) {
      return s.substring(1, s.length() - 1);
    }
    return expr;
  }

  /**
   * Private constructor to build an ExpressionInterpreter. Interpreters
   * can only be created by createInterpreter.
   *
   * @param expr Expression
   * @throws ExpressionException Invalid Expression details
   */
  public ExpressionInterpreter(String expr) throws ExpressionException {
    super();

    expression = expr == null ? "" : strip(expr);

    // Install the Vassal Class loader so that bsh can find Vassal classes
    this.setClassLoader(this.getClass().getClassLoader());

    // Initialise the top-level name space if this is the first
    // expression to be created
    if (topLevelNameSpace == null) {
      initialiseStatic();
    }

    // Create the Expression level namespace as a child of the
    // top level namespace
    expressionNameSpace = new NameSpace(topLevelNameSpace, "expression"); // NON-NLS

    // Get a list of any variables used in the expression. These are
    // property names that will need to be evaluated at expression
    // evaluation time.
    // stringVariables is a list of the property names that call String functions so we
    // know must be String type. These will be passed in to the evaluating expression as
    // parameters to force their type to be known and allow String functions to be called on them.
    final BeanShellExpressionValidator validator = new BeanShellExpressionValidator(expression);
    variables = validator.getVariables();
    stringVariables = validator.getStringVariables();

    // Build a method enclosing the expression. This saves the results
    // of the expression parsing, improving performance. Force return
    // value to a String as this is what Vassal is expecting.
    // Pass the values of any String property names used in the expression as arguments
    setNameSpace(expressionNameSpace);
    if (expression.length() > 0) {
      try {
        final StringBuilder argList = new StringBuilder();
        for (final String variable : stringVariables) {
          if (argList.length() > 0) {
            argList.append(',');
          }
          argList.append("String ").append(variable); // NON-NLS
        }
        final String ex = "String " + MAGIC2 + "(" + argList.toString() + ") { " + MAGIC3 + "=" + expression + "; return " + MAGIC3 + ".toString();}";
        eval(ex); // NON-NLS
      }
      catch (EvalError e) {
        throw new ExpressionException(getExpression());
      }
    }

    // Add a link to this Interpreter into the new NameSpace for callbacks from
    // BeanShell back to us
    setVar(THIS, this);

  }

  /**
   * Initialise the static elements of this class. Create a Top Level
   * NameSpace using the Vassal class loader, load useful classes and
   * read and process the init_expression.bsh file to load scripted
   * methods available to expressions.
   */
  protected void initialiseStatic() {
    topLevelNameSpace = new NameSpace(null, getClassManager(), "topLevel"); // NON-NLS
    setNameSpace(topLevelNameSpace);
    getNameSpace().importClass("VASSAL.build.module.properties.PropertySource");
    getNameSpace().importClass("VASSAL.script.ExpressionInterpreter");

    // Read the Expression initialisation script into the top level namespace
    final URL ini = getClass().getResource(INIT_SCRIPT);

    try (InputStream is = ini.openStream();
         InputStreamReader isr = new InputStreamReader(is, StandardCharsets.UTF_8);
         BufferedReader in = new BufferedReader(isr)) {
      try {
        eval(in);
      }
      catch (EvalError e) {
        logger.error("Error trying to read init script: " + ini); // NON-NLS
        WarningDialog.show(e, "");
      }
    }
    catch (IOException e) {
      logger.error("Error trying to read init script: " + ini); // NON-NLS
      WarningDialog.show(e, "");
    }
  }

  /**
   * Return the current expression
   *
   * @return expression
   */
  public String getExpression() {
    return expression;
  }

  /**
   * Evaluate the expression, setting the value of any undefined
   * values to the matching Vassal property value. Primitives must
   * be wrapped.
   *
   * @return result
   */
  public String evaluate(PropertySource ps) throws ExpressionException {
    return evaluate(ps, false);
  }

  public String evaluate(PropertySource ps, boolean localized) throws ExpressionException {
    return evaluate(ps, localized, null, null);
  }

  public String evaluate(PropertySource ps, boolean localized, Auditable owner, AuditTrail audit) throws ExpressionException {
    return evaluate(ps, null, localized, owner, audit);
  }

  public String evaluate(PropertySource ps, java.util.Map<String, String> properties, boolean localized, Auditable owner, AuditTrail audit) throws ExpressionException {

    if (getExpression().length() == 0) {
      return "";
    }

    String result;
    try {
      RecursionLimiter.startExecution(this);

      // Default to the GameModule to satisfy properties if no
      // GamePiece supplied.
      source = ps == null ? GameModule.getGameModule() : ps;
      currentAudit = audit;
      currentOwner = owner;

      setNameSpace(expressionNameSpace);

      // Bind each undeclared variable with the value of the
      // corresponding Vassal property. Allow for old-style $variable$ references
      for (final String var : variables) {
        String name = var;
        final String origName = name;
        if (name.length() > 2 && name.startsWith("$") && name.endsWith("$")) {
          name = name.substring(1, name.length() - 1);
        }
        // Check for a propoerty in the passed property Map, then check the source if not found
        Object prop = properties == null ? null : properties.get(name);
        if (prop == null) {
          prop = (source == null) ? "" : localized ? source.getLocalizedProperty(name) : source.getProperty(name);
        }
        final String value = prop == null ? "" : prop.toString();
        if (audit != null) {
          audit.addMessage(" " + origName + "=" + (value == null ? "" : value));
        }
        if (value == null) {
          setVar(var, "");
        }
        else if (BeanShell.TRUE.equals(value)) {
          setVar(var, true);
        }
        else if (BeanShell.FALSE.equals(value)) {
          setVar(var, false);
        }
        else if (!StringUtils.containsOnly(value, "+-.0123456789")) { // NON-NLS
          setVar(var, value);
        }
        // Special case where the 'Store Integers with leading zeros as Strings' option is turned on AND
        // the string is 2 or more numerical digits commencing with 0, then store it as a String so that
        // the leading zeros are preserved. It is up to the Designer to convert this to an integer later
        // using Integer.parseInt(x) if they need to do arithmetic on it.
        else if (GlobalOptions.getInstance() != null && GlobalOptions.getInstance().isStoreLeadingZeroIntegersAsStrings()
          && value.length() > 1 && value.startsWith("0")
          && StringUtils.containsOnly(value, "0123456789")) {
          setVar(var, value);
        }
        else {
          try {
            setVar(var, Integer.parseInt(value));
          }
          catch (NumberFormatException ex1) {
            // A very large integer (e.g. a PieceUID) will fail to convert to an Integer.
            // Don't let it convert to a Float, store it as a String instead
            try {
              NumberUtils.createBigInteger(value); // Will fail if non-integer
              setVar(var, value);
            }
            catch (NumberFormatException ex3) {
              try {
                setVar(var, Float.parseFloat(value));
              }
              catch (NumberFormatException ex2) {
                setVar(var, value);
              }
            }
          }
        }
      }

      final StringBuilder argList = new StringBuilder();
      for (final String var : stringVariables) {
        String name = var;
        final String origName = name;
        if (name.length() > 2 && name.startsWith("$") && name.endsWith("$")) {
          name = name.substring(1, name.length() - 1);
        }
        // Check for a property in the passed property Map, then check the source if not found
        Object prop = properties == null ? null : properties.get(name);
        if (prop == null) {
          prop = (source == null) ? "" : localized ? source.getLocalizedProperty(name) : source.getProperty(name);
        }
        final String value = prop == null ? "" : prop.toString();
        if (audit != null) {
          audit.addMessage(" " + origName + "=" + (value == null ? "" : value));
        }
        if (argList.length() > 0) {
          argList.append(',');
        }
        argList.append('"').append(value == null ? "" : value.replace("\"", "\\\"")).append('"');
      }

      // Re-evaluate the pre-parsed expression now that the undefined variables have
      // been bound to their Vassal property values.

      setVar(THIS, this);
      setVar(SOURCE, source);

      try {
        eval(MAGIC1 + "=" + MAGIC2 + "(" + argList.toString() + ")");
        final Object magic1 = get(MAGIC1);
        result = (magic1 == null) ? "" : magic1.toString();
      }
      catch (EvalError e) {
        final String s = e.getRawMessage();
        final String search = MAGIC2 + "();'' ";
        final int pos = s.indexOf(search);
        String m = s.substring(pos + search.length());
        if (m.startsWith(ERROR_PREFIX)) {
          m = m.substring(ERROR_PREFIX.length() + 1);
        }
        if (audit != null) {
          audit.addMessage(Resources.getString("Audit.error_trail", m));
        }
        throw new ExpressionException(getExpression(), s.substring(pos + search.length()), owner, audit);
      }
    }
    catch (RecursionLimitException e) {
      result = "";
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.possible_infinite_expression_loop"), getExpression(), e));
    }
    finally {
      RecursionLimiter.endExecution();
      source = null;  // prevent source from being retained
    }

    return result;
  }

  public String evaluate() throws ExpressionException {
    return getExpression().length() == 0 ? "" : evaluate(GameModule.getGameModule());
  }

  /**
   * Convert a String value into a wrapped primitive object if possible.
   * Note this is a non-static copy of BeanShell.wrap(). Callbacks from
   * beanshell (e.g. getProperty) fail if an attempt is made to call a static method.
   *
   * @param value Value to wrap
   * @return wrapped value
   */
  public Object wrap(String value) {
    if (value == null) {
      return "";
    }
    else if (BeanShell.TRUE.equals(value)) {
      return Boolean.TRUE;
    }
    else if (BeanShell.FALSE.equals(value)) {
      return Boolean.FALSE;
    }
    else {
      try {
        return Integer.valueOf(value);
      }
      catch (NumberFormatException e) {
        return value;
      }
    }
  }

  /**
   * Wrap a possibly already wrapped value.
   *
   * @param value A value that may already be wrapped (due to auto-boxing)
   * @return      Wrapped value
   */
  public Object wrap(Object value) {
    return value instanceof String ? wrap((String) value) : value;
  }

  /*****************************************************************
   * Callbacks from BeanShell Expressions to Vassal
   **/

  public Object getProperty(Object name) {
    final Object value = source.getProperty(name.toString());
    return value == null ? "" : wrap(value.toString());
  }

  public Object getLocalizedProperty(Object name) {
    final Object value = source.getLocalizedProperty(name.toString());
    return value == null ? "" : wrap(value.toString());
  }

  /**
   * getProperty minus the wrap
   */
  public Object getString(Object name) {
    final Object value = source.getProperty(name.toString());
    return value == null ? "" : value.toString();
  }

  public Object getZoneProperty(Object propertyName, Object zoneName) {
    if (source instanceof GamePiece) {
      final Map map = ((GamePiece) source).getMap();
      if (map != null) {
        final Zone zone = map.findZone(zoneName.toString());
        if (zone != null) {
          return wrap((String) zone.getProperty(propertyName.toString()));
        }
      }
    }
    return wrap("");
  }

  public Object getZoneProperty(Object propertyName, Object zoneName, Object mapName) {
    final Map map = findVassalMap(mapName.toString());
    if (map != null) {
      final Zone zone = map.findZone(zoneName.toString());
      if (zone != null) {
        return wrap((String) zone.getProperty(propertyName.toString()));
      }
    }
    return wrap("");
  }

  public Object getMapProperty(Object propertyName, Object mapName) {
    final Map map = findVassalMap(mapName.toString());
    return map == null ? wrap("") : wrap((String) map.getProperty(propertyName.toString()));
  }

  /**
   * Callback for Beanshell getAttachProperty functions
   *
   * @param attachment              Attachment name
   * @param property                Property name
   * @param indexOrNameOrexpression An index number, a piece Basic Name or an Expression
   * @param ps                      Source Piece
   * @return                        Property value
   */
  public Object getAttachmentProperty(Object attachment, Object property, Object indexOrNameOrexpression, PropertySource ps) {
    final String attach = attachment.toString();
    final String prop = property.toString();

    if (attach.isEmpty() || prop.isEmpty()) return "";

    int index = -1;
    String pieceName = "";
    PieceFilter filter = null;

    if (indexOrNameOrexpression instanceof Integer) {
      index = (Integer) indexOrNameOrexpression;
    }
    else {
      final String s = indexOrNameOrexpression.toString();
      if (NumberUtils.isDigits(s)) {
        index = Integer.parseInt(s);
      }
      else if (s.startsWith("{")) {
        filter = createFilter(s, ps);
      }
      else {
        pieceName = s;
      }
    }

    return getAttachmentProperty(attach, prop, index, pieceName, filter, ps);
  }

  /**
   * Internal implementation of all types of getAttachment Property
   * NOTE: Only one of (index>0), !pieceName.isEmprty() or filter != null should be true
   *
   * @param attachment  Name of attachment
   * @param property    Name of Property
   * @param index       If > 0, then return the property from the index'th attached piece (starting at 1)
   * @param pieceName   If not empty, then return the property from the first attached piece with this Basic Name
   * @param filter      If not null, then return the property from the first piece that matches the expression
   * @param ps          Source Piece
   * @return            Property value
   */
  private Object getAttachmentProperty(String attachment, String property, Integer index, String pieceName, PieceFilter filter, PropertySource ps) {
    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      final GamePiece p = Decorator.getOutermost((Decorator) ps);
      for (final GamePiece gp : Decorator.getDecorators(p, Attachment.class)) {
        final Attachment attach = (Attachment) gp;
        if (attachment.equals(attach.getAttachName())) {

          // Found the named attachment, handle the index case
          if (index > 0) {
            final GamePiece target = attach.getAttachedPieceAt(index - 1);
            if (target == null) return "";
            return wrap(target.getProperty(property));
          }

          //Search through the attachments, looking for a match on name or expression
          for (final GamePiece target : attach.getAttachList()) {
            if (!pieceName.isEmpty() && pieceName.equals(target.getProperty(BasicPiece.BASIC_NAME))) {
              return wrap(target.getProperty(property));
            }
            if (filter != null && filter.accept(target)) {
              return wrap(target.getProperty(property));
            }
          }
        }
      }
    }
    return "";
  }

  private PropertySource translatePiece(PropertySource ps) {
    // This allows ReportState to sum properties properly
    if (ps instanceof ReportState.OldAndNewPieceProperties) {
      ps = ((ReportState.OldAndNewPieceProperties) ps).getNewPiece();
    }
    return ps;
  }

  /**
   * Convert an arbitrary property value to an integer and return the value,
   * or return 0 if not an integer.
   * @param prop  Property value
   * @return      converted integer value
   */
  private static int IntPropValue(Object prop) {
    if (prop != null) {
      if (prop instanceof Integer) {
        return (Integer) prop;
      }
      final String s1 = prop.toString();
      return NumberUtils.toInt(s1, 0);
    }
    return 0;
  }

  /**
   * SumStack(property) function
   * Total the value of the named property in all counters in the
   * same stack as the specified piece.
   *
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object sumStack(Object property, PropertySource ps) {
    return sumStack(property, "", ps);
  }

  /**
   * Create a PieceFilter from an expression.
   *
   * @param expression  Beanshell Expression
   * @param ps          Property Source
   * @return            Generated filter
   */
  private PieceFilter createFilter(String expression, PropertySource ps, String comment) {
    if (expression == null) return null;
    final String matchString = replaceDollarVariables(expression, ps);
    return matchString == null || matchString.isEmpty() ? null : new PropertyExpression(unescape(matchString)).getFilter(ps, currentOwner, AuditTrail.create(ps, expression, comment));
  }

  private PieceFilter createFilter(String expression, PropertySource ps) {
    return createFilter(expression, ps, "");
  }

  private PieceFilter createFilter(Object expression, PropertySource ps, String comment) {
    return createFilter(expression.toString(), ps, comment);
  }

  private PieceFilter createFilter(Object expression, PropertySource ps) {
    return createFilter(expression, ps, "");
  }


  /**
   * SumStack(property, expression) function
   * Total the value of the named property in all counters in the
   * same stack as the specified piece that meet the specified expression
   *
   * @param property   Property Name
   * @param expression Expression
   * @param ps         GamePiece
   * @return total
   */
  public Object sumStack(Object property, Object expression, PropertySource ps) {
    return sumStack(property.toString(), createFilter(expression, ps), ps);
  }

  private Object sumStack(String property, PieceFilter filter, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);
    if (ps instanceof GamePiece) {
      final GamePiece piece = (GamePiece) ps;
      final Stack s = ((GamePiece) ps).getParent();
      if (s == null) {
        result = updateTotal(result, piece, property, filter, true);
      }
      else {
        for (final GamePiece gamePiece : s.asList()) {
          result = updateTotal(result, gamePiece, property, filter, true);
        }
      }
    }
    return result;
  }

  /**
   * CountStack(property) and CountStack(expression) function count the number of pieces in
   * the same stack which have any non-blank value for the
   * specified property.
   *
   * @param propertyOrExpression Property Name or Expression
   * @param ps                   GamePiece
   * @return total
   */
  public Object countStack(Object propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.toString().trim().startsWith("{")) {
      return countStack("", propertyOrExpression, ps);
    }
    else {
      return countStack(propertyOrExpression.toString(), (PieceFilter) null, ps);
    }
  }

  public Object countStack(Object property, Object expression, PropertySource ps) {
    return countStack(property.toString(), createFilter(expression, ps), ps);
  }


  private Object countStack(String property, PieceFilter filter, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      final GamePiece piece = (GamePiece) ps;
      final Stack s = ((GamePiece) ps).getParent();
      if (s == null) {
        result = updateTotal(result, piece, property, filter, false);
      }
      else {
        if (filter == null && (property == null || property.isEmpty())) {
          result = s.nVisible(); // Blank property with no filter returns number of visible-to-me pieces in the stack
        }
        else {
          for (final GamePiece gamePiece: s.asList()) {
            result = updateTotal(result, gamePiece, property, filter, false);
          }
        }
      }
    }
    return result;
  }

  /**
   * MaxAttachment(attachment, property) function
   * Highest value of the named property among all pieces attached, or 0 if no pieces are attached
   *
   * @param attachment Attachment Name
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object maxAttachment(Object attachment, Object property, PropertySource ps) {
    int result = Integer.MIN_VALUE;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      final GamePiece p = Decorator.getOutermost((Decorator)ps);
      for (final GamePiece decorator : Decorator.getDecorators(p, Attachment.class)) {
        final Attachment a = (Attachment) decorator;
        if (a.getAttachName().equals(attachment)) {
          for (final GamePiece target : a.getAttachList()) {
            final Object prop = target.getProperty(property);
            final int value = IntPropValue(prop);
            if (value > result) {
              result = value;
            }
          }
        }
      }
    }

    return (result == Integer.MIN_VALUE) ? 0 : result;
  }


  /**
   * MinAttachment(attachment, property) function
   * Lowest value of the named property among all pieces attached, or 0 if no pieces are attached
   *
   * @param attachment Attachment Name
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object minAttachment(Object attachment, Object property, PropertySource ps) {
    int result = Integer.MAX_VALUE;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      final GamePiece p = Decorator.getOutermost((Decorator)ps);
      for (final GamePiece decorator : Decorator.getDecorators(p, Attachment.class)) {
        final Attachment a = (Attachment) decorator;
        if (a.getAttachName().equals(attachment)) {
          for (final GamePiece target : a.getAttachList()) {
            final Object prop = target.getProperty(property);
            final int value = IntPropValue(prop);
            if (value < result) {
              result = value;
            }
          }
        }
      }
    }

    return (result == Integer.MAX_VALUE) ? 0 : result;
  }


  /**
   * SumAttach Beanshell function implementation
   */
  public Object sumAttachment(Object attachment, Object property, PropertySource ps) {
    return sumAttachment(attachment, property, "", ps);
  }

  public Object sumAttachment(Object attachment, Object property, Object expression, PropertySource ps) {
    return sumAttachment(attachment.toString(), property.toString(), createFilter(expression, ps), ps);
  }

  private Object sumAttachment(String attachment, String property, PieceFilter filter, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      final GamePiece p = Decorator.getOutermost((Decorator)ps);
      for (final GamePiece decorator : Decorator.getDecorators(p, Attachment.class)) {
        final Attachment a = (Attachment) decorator;
        if (a.getAttachName().equals(attachment)) {
          for (final GamePiece target : a.getAttachList()) {
            result = updateTotal(result, target, property, filter, true);
          }
        }
      }
    }
    return result;
  }

  /**
   * CountAttach Beanshell function implementation
   */

  public Object countAttachment(Object attachment, Object propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.toString().trim().startsWith("{")) {
      return countAttachment(attachment, "", propertyOrExpression, ps);
    }
    else {
      return countAttachment(attachment, propertyOrExpression.toString(), "", ps);
    }
  }

  public Object countAttachment(Object attachment, Object property, Object expression, PropertySource ps) {
    return countAttachment(attachment.toString(), property.toString(), createFilter(expression, ps), ps);
  }

  private Object countAttachment(String attachment, String property, PieceFilter filter, PropertySource ps) {
    int result = 0;
    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      for (final GamePiece target : Attachment.getAttachList((GamePiece) ps, attachment)) {
        result = updateTotal(result, target, property, filter, false);
      }
    }

    return result;
  }
  /**
   * SumMat(property) function
   * Total the value of the named property in all counters
   * among the Mat-and-MatCargo grouping of the current piece
   *
   * @param propertyOrExpression Property Name
   * @param ps                   GamePiece
   * @return total
   */
  public Object sumMat(Object propertyOrExpression, PropertySource ps) {
    final String s = propertyOrExpression.toString();
    if (s.startsWith("{")) {
      return sumMat("", propertyOrExpression, ps);
    }
    else {
      return sumMat(propertyOrExpression.toString(), (PieceFilter) null, ps);
    }
  }

  public Object sumMat(Object property, Object expression, PropertySource ps) {
    final PieceFilter filter = createFilter(expression, ps);
    return sumMat(property.toString(), filter, ps);
  }

  private Object sumMat(String property, PieceFilter filter, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      GamePiece gp = (GamePiece) ps;
      GamePiece mat;
      if (gp instanceof Decorator) {
        gp  = Decorator.getOutermost(gp);
        mat = Decorator.getDecorator(gp, Mat.class);

        if (mat == null) {
          final MatCargo cargo = (MatCargo) Decorator.getDecorator(gp, MatCargo.class);
          if (cargo != null) {
            mat = cargo.getMat();
          }
        }
      }
      else {
        mat = null;
      }

      if (mat != null) {
        mat = Decorator.getOutermost(mat);
        final Mat actualMat = (Mat) Decorator.getDecorator(mat, Mat.class);
        result = updateTotal(result, actualMat, property, filter, true);

        for (final GamePiece cargo : actualMat.getContents()) {
          result = updateTotal(result, cargo, property, filter, true);
        }
      }
      else {
        result = updateTotal(result, (GamePiece) ps, property, filter, true);
      }
    }
    return result;
  }
  /**
   * CountMat(property) function
   * Return the total number of counters with a non-blank value for the specified property
   * among the Mat-and-MatCargo grouping of the current piece
   *
   * @param propertyOrExpression Property Name
   * @param ps                   GamePiece
   * @return total
   */
  public Object countMat(Object propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.toString().trim().startsWith("{")) {
      return countMat("", propertyOrExpression, ps);
    }
    else {
      return countMat(propertyOrExpression.toString(), (PieceFilter) null, ps);
    }
  }

  public Object countMat(PropertySource ps) {
    return countMat("", null, ps);
  }

  public Object countMat(Object property, Object expression, PropertySource ps) {
    final PieceFilter filter = createFilter(expression, ps);
    return countMat(property.toString(), filter, ps);
  }

  private Object countMat(String property, PieceFilter filter, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      GamePiece gp = (GamePiece) ps;
      GamePiece mat;
      if (gp instanceof Decorator) {
        gp  = Decorator.getOutermost(gp);
        mat = Decorator.getDecorator(gp, Mat.class);

        if (mat == null) {
          final MatCargo cargo = (MatCargo) Decorator.getDecorator(gp, MatCargo.class);
          if (cargo != null) {
            mat = cargo.getMat();
          }
        }
      }
      else {
        mat = null;
      }

      if (mat != null) {
        mat = Decorator.getOutermost(mat);
        final Mat actualMat = (Mat) Decorator.getDecorator(mat, Mat.class);
        result = updateTotal(result, actualMat, property, filter, false);


        for (final GamePiece cargo : actualMat.getContents()) {
          result = updateTotal(result, cargo, property, filter, false);
        }
      }
      else {
        result = updateTotal(result, (GamePiece) ps, property, filter, false);
      }
    }

    return result;
  }

  /**
   * SumLocation(property) function
   * Total the value of the named property in all counters in the
   * same location as the specified piece.
   * <p>
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object sumLocation(Object property, PropertySource ps) {
    return sumLocation(property, "", ps);
  }

  /**
   * SumLocation(property, expression) function
   * Total the value of the named property in all counters in the
   * same location as the specified piece that meet the supplied expression.
   * <p>
   * @param property   Property Name
   * @param expression Expression
   * @param ps         GamePiece Source
   * @return total
   */
  public Object sumLocation(Object property, Object expression, PropertySource ps) {

    if (ps instanceof GamePiece) {
      final GamePiece p = (GamePiece) ps;
      final Map m = p.getMap();
      if (m != null) {
        final String here = m.locationName(p.getPosition());
        return sumLocation(property, here, m.getConfigureName(), expression, ps);
      }
    }

    return 0;
  }

  /**
   * SumMapLocation(property, location, map) function
   * Total the value of the named property in all counters in the
   * specified map and location
   * <p>
   * @param property   Property Name
   * @param location   Location Name
   * @param map        Map Name
   * @param ps         GamePiece source
   * @return total
   */
  public Object sumLocation(Object property, Object location, Object map, PropertySource ps) {
    return sumLocation(property, location, map, "", ps);
  }

  /**
   * SumMapLocation(property, location, map, expression) function
   * Total the value of the named property in all counters in the
   * specified map and location that match the supplied expression
   * <p>
   * @param property   Property Name
   * @param location   Location Name
   * @param map        Map Name
   * @param expression Expression
   * @param ps         GamePiece source
   * @return total
   */

  public Object sumLocation(Object property, Object location, Object map, Object expression, PropertySource ps) {

    ps = translatePiece(ps);

    final PieceFilter filter = createFilter(expression, ps, "SumLocation");
    final Map targetMap = findVassalMap(map.toString());

    return targetMap == null ? 0 : sumLocation(property.toString(), location.toString(), targetMap, filter);
  }

  /**
   * Lowest-level SumMapLocation function called by all other versions
   *
   * @param property      Property Name to sum
   * @param locationName  Location Name to match
   * @param mapName       Map to check
   * @param filter        Optional PieceFilter to check expression match
   * @return
   */
  private Object sumLocation(String property, String locationName, Map map, PieceFilter filter) {
    int result = 0;

    // Ask IndexManager for list of pieces on that map at that location. Stacks are not returned by the IM.
    for (final GamePiece piece : GameModule.getGameModule().getIndexManager().getPieces(map, BasicPiece.LOCATION_NAME, locationName)) {
      result = updateTotal(result, piece, property, filter, true);
    }
    return result;
  }

  /**
   * BeanShell CountLocation implementation
   * Return count of pieces in the same location as the current piece
   * @return  Piece Count
   */
  public Object countLocation(PropertySource ps) {
    return countLocation("", ps);
  }


  public Object countLocation(Object propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.toString().trim().startsWith("{")) {
      return countCurrentLocation("", propertyOrExpression, ps);
    }
    else {
      return countCurrentLocation(propertyOrExpression, "", ps);
    }
  }

  public Object countLocation(Object propertyOrLocation, Object expressionOrMap, PropertySource ps) {
    if (expressionOrMap != null && expressionOrMap.toString().trim().startsWith("{")) {
      return countCurrentLocation(propertyOrLocation, expressionOrMap,  ps);
    }
    else {
      return countMapLocation(propertyOrLocation, expressionOrMap, "", ps);
    }
  }

  private Object countCurrentLocation(Object property, Object expression, PropertySource ps) {
    if (ps instanceof GamePiece) {
      final GamePiece p = (GamePiece) ps;
      final Map m = p.getMap();
      if (m != null) {
        final String here = m.locationName(p.getPosition());
        return countMapLocation(here, m.getConfigureName(), property, expression, ps);
      }
    }
    return 0;
  }

  public Object countMapLocation(Object locationName, Object mapName, Object propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.toString().trim().startsWith("{")) {
      return countMapLocation(locationName, mapName, "", propertyOrExpression, ps);
    }
    else {
      return countMapLocation(locationName, mapName, propertyOrExpression, "", ps);
    }
  }

  public Object countMapLocation(Object locationName, Object mapName, Object property, Object expression, PropertySource ps) {
    ps = translatePiece(ps);

    final PieceFilter filter = createFilter(expression, ps);
    final Map targetMap = findVassalMap(mapName.toString());

    String propValue;
    if (property == null || property.toString().isEmpty()) {
      propValue = null;
    }
    else {
      propValue = property.toString();
    }

    return targetMap == null ? 0 : countLocation(locationName.toString(), targetMap, propValue, filter);
  }

  /**
   * Lowest-level CountLocation function called by all other versions
   * @param locationName Location Name to search for
   * @param map          Map to search on
   * @param propValue    null if no property was supplied, or property name if supplied
   * @param filter       Option filter implementing Property Match Expression
   * @return             Count of pieces
   */
  private Object countLocation(Object locationName, Map map, String property, PieceFilter filter) {
    int result = 0;

    // Ask IndexManager for list of pieces on that map at that location. Stacks are not returned by the IM.
    for (final GamePiece piece : GameModule.getGameModule().getIndexManager().getPieces(map, BasicPiece.LOCATION_NAME, locationName.toString())) {
      result = updateTotal(result, piece, property, filter, false);
    }

    return result;
  }


  /**
   * SumZone(property, location, map, expression) function
   * Total the value of the named property in all counters in the
   * specified map and zone that match the supplied expression
   * <p>
   * @param property   Property Name
   * @param zone       Zone Name
   * @param map        Map Name
   * @param expression Expression
   * @param ps         GamePiece source
   * @return total
   */

  public Object sumZone(Object property, Object zone, Object map, Object expression, PropertySource ps) {
    ps = translatePiece(ps);

    String mapName = map == null ? "" : map.toString();
    String zoneName = zone == null ? "" : zone.toString();
    final String expr = expression == null ? "" : expression.toString();

    // GamePiece versions of SumZone may not have provided a map or zone, use the ones where the source piece is.
    if (ps instanceof GamePiece && mapName.isEmpty()) {
      mapName = (String) ps.getProperty(BasicPiece.CURRENT_MAP);
      zoneName = (String) ps.getProperty(BasicPiece.CURRENT_ZONE);
    }

    final PieceFilter filter = createFilter(expr, ps);
    final Map targetMap = findVassalMap(mapName);

    return targetMap == null ? 0 : sumZone(property.toString(), zoneName, targetMap, filter);
  }

  /**
   * Lowest-level SumZone function called by all other versions
   *
   * @param property      Property Name to sum
   * @param zone          Zone Name to match
   * @param mapName       Map to check
   * @param filter        Optional PieceFilter to check expression match
   * @return
   */
  private Object sumZone(String property, String zoneName, Map map, PieceFilter filter) {
    int result = 0;

    // Ask IndexManager for list of pieces on that map at that zone. Stacks are not returned by the IM.
    for (final GamePiece piece : GameModule.getGameModule().getIndexManager().getPieces(map, BasicPiece.CURRENT_ZONE, zoneName)) {
      result = updateTotal(result, piece, property, filter, true);
    }
    return result;
  }

  /**
   * CountZone() function.
   * Return count of pieces in the same zone as the current piece
   * @return  Piece Count
   */
  public Object countZone(PropertySource ps) {
    return countCurrentZone("", "", ps);
  }


  public Object countZone(Object propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.toString().trim().startsWith("{")) {
      return countCurrentZone("", propertyOrExpression, ps);
    }
    else {
      return countCurrentZone(propertyOrExpression, "", ps);
    }
  }

  public Object countZone(Object propertyOrZone, Object expressionOrMap, PropertySource ps) {
    if (expressionOrMap != null && expressionOrMap.toString().trim().startsWith("{")) {
      return countCurrentZone(propertyOrZone, expressionOrMap,  ps);
    }
    else {
      return countZone(propertyOrZone, expressionOrMap, "", ps);
    }
  }

  public Object countCurrentZone(Object propertyName, Object expression, PropertySource ps) {
    if (ps instanceof GamePiece) {
      final String mapName = (String) ps.getProperty(BasicPiece.CURRENT_MAP);
      final String zoneName = (String) ps.getProperty(BasicPiece.CURRENT_ZONE);
      return countZone(zoneName, mapName, propertyName, expression, ps);
    }
    return 0;
  }


  public Object countZone(Object zoneName, Object mapName, Object propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.toString().trim().startsWith("{")) {
      return countZone(zoneName, mapName, "", propertyOrExpression, ps);
    }
    else {
      return countZone(zoneName, mapName, propertyOrExpression, "", ps);
    }
  }

  public Object countZone(Object zoneName, Object mapName, Object property, Object expression, PropertySource ps) {
    ps = translatePiece(ps);

    final PieceFilter filter = createFilter(expression, ps);
    final Map targetMap = findVassalMap(mapName.toString());

    String propValue;
    if (property == null || property.toString().isEmpty()) {
      propValue = null;
    }
    else {
      propValue = property.toString();
    }

    return targetMap == null ? 0 : countZone(zoneName.toString(), targetMap, propValue, filter);
  }


  /**
   * Lowest-level CountZone function called by all other versions
   * @param zoneName     Zone Name to search for
   * @param map          Map to search on
   * @param propValue    null if no property was supplied, or value of supplied property
   * @param filter       Option filter implementing Property Match Expression
   * @return             Count of pieces
   */
  private Object countZone(String zoneName, Map map, String property, PieceFilter filter) {
    int result = 0;

    // Ask IndexManager for list of pieces on that map at that location. Stacks are not returned by the IM.
    for (final GamePiece piece : GameModule.getGameModule().getIndexManager().getPieces(map, BasicPiece.CURRENT_ZONE, zoneName)) {
      result = updateTotal(result, piece, property, filter, false);
    }

    return result;
  }




  // 1 Argument form of SumMap
  public Object sumMap(Object property, PropertySource ps) {
    return sumMap(property, "", "", ps);
  }

  // 2 Argument form of SumMap
  public Object sumMap(Object property, Object mapOrExpression, PropertySource ps) {
    if (mapOrExpression != null && mapOrExpression.toString().trim().startsWith("{")) {
      return sumMap(property, "", mapOrExpression, ps);
    }
    else {
      return sumMap(property, mapOrExpression, "", ps);
    }
  }

  // 3 Argument form of SumMap
  public Object sumMap(Object property, Object map, Object expression, PropertySource ps) {
    ps = translatePiece(ps);

    final String propertyName = property == null ? "" : property.toString();
    if (propertyName.isEmpty()) {
      return 0;
    }

    String mapName = map == null ? "" : map.toString();

    // GamePiece versions of SumZone may not have provided a map or zone, use the ones where the source piece is.
    if (ps instanceof GamePiece && mapName.isEmpty()) {
      mapName = (String) ps.getProperty(BasicPiece.CURRENT_MAP);
    }

    final PieceFilter filter = createFilter(expression, ps);
    final Map targetMap = findVassalMap(mapName);

    return sumMap(propertyName, targetMap, filter);
  }

  /**
   * Lowest-level SumMap function called by all other versions
   * @param propertyName Property to sum
   * @param map          Map to search on
   * @param propValue    null if no property was supplied, or value of supplied property
   * @param filter       Option filter implementing Property Match Expression
   * @return             Count of pieces
   */
  private Object sumMap(String propertyName, Map map, PieceFilter filter) {
    int result = 0;

    if (map != null) {
      for (final GamePiece piece : map.getAllPieces()) {
        if (piece instanceof Stack) {
          for (final GamePiece p : ((Stack) piece).asList()) {
            result = updateTotal(result, p, propertyName, filter, true);
          }
        }
        else {
          result = updateTotal(result, piece, propertyName, filter, true);
        }
      }
    }

    return result;
  }


  // 0 Arg version of CountMap
  public Object countMap(PropertySource ps) {
    return countMap("", "", "", ps);
  }
  // 1 Arg version of CountMap
  public Object countMap(Object propertyOrExpressionOrMap, PropertySource ps) {
    if (propertyOrExpressionOrMap != null && propertyOrExpressionOrMap.toString().trim().startsWith("{")) {
      return countMap("", "", propertyOrExpressionOrMap, ps);
    }
    final Map map = findVassalMap((String) propertyOrExpressionOrMap);
    if (map == null) {
      return countMap("", propertyOrExpressionOrMap, "", ps);
    }
    else {
      return countMap(propertyOrExpressionOrMap, "", "", ps);
    }
  }

  // 2 Arg version of CountMap
  public Object countMap(Object propertyOrMap, Object expressionOrProperty, PropertySource ps) {
    Object map = null;
    Object property = null;
    Object expression = null;

    if (expressionOrProperty != null && expressionOrProperty.toString().trim().startsWith("{")) {
      // 2nd arg is expression, must be (map, expr) or (prop, expr)
      expression = expressionOrProperty;

      final Map targetMap = findVassalMap((String) propertyOrMap);
      if (targetMap == null) {
        property = propertyOrMap;
      }
      else {
        map = propertyOrMap;
      }
    }
    else {
      // 2nd arg not an expression, must be (map, property)
      map = propertyOrMap;
      property = expressionOrProperty;
    }

    return countMap(map, property, expression, ps);
  }


  // 3 Arg version of CountMap
  public Object countMap(Object map, Object property, Object expression, PropertySource ps) {

    final String mapName = map == null ? "" : map.toString();
    final String propertyName = property == null ? "" : property.toString();
    Map targetMap = null;

    // GamePiece versions of SumZone may not have provided a map or zone, use the ones where the source piece is.
    if (ps instanceof GamePiece && mapName.isEmpty()) {
      targetMap = ((GamePiece) ps).getMap();
    }
    else {
      targetMap = findVassalMap(mapName);
    }


    final PieceFilter filter = createFilter(expression, ps);

    return countMap(targetMap, propertyName, filter);
  }

  /**
   * Lowest-level CountMap function called by all other versions
   * @param propertyName Property to sum
   * @param map          Map to search on
   * @param propValue    null if no property was supplied, or value of supplied property
   * @param filter       Option filter implementing Property Match Expression
   * @return             Count of pieces
   */
  private Object countMap(Map map, String propertyName, PieceFilter filter) {
    int result = 0;

    if (map != null) {
      for (final GamePiece piece : map.getAllPieces()) {
        if (piece instanceof Stack) {
          for (final GamePiece p : ((Stack) piece).asList()) {
            result = updateTotal(result, p, propertyName, filter, false);
          }
        }
        else {
          result = updateTotal(result, piece, propertyName, filter, false);
        }
      }
    }

    return result;
  }

  /**
   * Implementation of Range functions
   */


  public Object rangeAttach(Object attachmentName, Object expression, boolean asPixels, PropertySource ps) {
    if (!(attachmentName instanceof String)) return 0;
    if (!(expression instanceof String)) return 0;
    final PieceFilter filter = createFilter((String) expression, ps);
    return rangeAttach((String) attachmentName, filter, asPixels, ps);
  }

  private Object rangeAttach(String attachmentName, PieceFilter filter, boolean asPixels,  PropertySource ps) {
    if (ps instanceof GamePiece) {
      final Map map = ((GamePiece) ps).getMap();
      final Point from = ((GamePiece) ps).getPosition();
      for (final GamePiece target : Attachment.getAttachList((GamePiece) ps, attachmentName)) {
        // Act on the first attached piece on the same Map that is not the source piece (in case of self-attachments)
        if (target != ps && (filter == null || filter.accept(target))) {
          final Map toMap = target.getMap();
          final Point to = target.getPosition();
          // Pieces must be on the same map
          if (map != null && map.equals(toMap)) {
            return range(from, to, map, asPixels);
          }
        }
      }
    }
    return 0;
  }

  public Object rangeInPixels(Object x, Object y, PropertySource ps) {
    return rangeXY(x, y, ps, true);
  }

  public Object rangeInCells(Object x, Object y, PropertySource ps) {
    return rangeXY(x, y, ps, false);
  }

  private Object rangeXY(Object x, Object y, PropertySource ps, boolean asPixels) {
    if (ps instanceof GamePiece) {
      final Map map = ((GamePiece) ps).getMap();
      final Point from = ((GamePiece) ps).getPosition();
      return range(from, new Point(IntPropValue(x), IntPropValue(y)), map, asPixels);
    }
    return 0;
  }

  public Object rangeInPixels(Object x1, Object y1, Object x2, Object y2, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, (String) ps.getProperty(BasicPiece.CURRENT_MAP), true);
  }

  public Object rangeInCells(Object x1, Object y1, Object x2, Object y2, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, (String) ps.getProperty(BasicPiece.CURRENT_MAP), false);
  }

  public Object rangeInPixels(Object x1, Object y1, Object x2, Object y2, Object mapName, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, mapName, true);
  }

  public Object rangeInCells(Object x1, Object y1, Object x2, Object y2, Object mapName, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, mapName, false);
  }

  private Object rangeMap(Object x1, Object y1, Object x2, Object y2, Object mapName, boolean asPixels) {
    final Point from = new Point(IntPropValue(x1), IntPropValue(y1));
    final Point to = new Point(IntPropValue(x2), IntPropValue(y2));
    final Map map = Map.getMapById(mapName.toString());
    return range(from, to, map, asPixels);
  }

  private int range(Point from, Point to, Map map, boolean asPixels) {

    if (from == null || to == null) {
      return 0;
    }

    // If range needed in pixels, or if the map/board/grid cannot be determined, just calculate it directly.
    if (asPixels || map == null || map.findBoard(from) == null || map.findBoard(from).getGrid() == null) {
      return (int) Math.round(from.distance(to));
    }

    // Otherwise ask the grid at the from point to calculate the range as it sees fit.
    // NOTE: the from and to points may not be on the same grid (in the case of a zoned grid) so range(from, to) is not necessarily equal to range(to, from)
    return map.findBoard(from).getGrid().range(from, to);
  }

  /**
   * Implementation of all flavours of SumRange and SumRangePx functions
   *
   * @param propertyName  Property name to sum
   * @param expression    Optional expression to select targets
   * @param minRange      minimum Range
   * @param maxRange      maximum range
   * @param asPixels      true to force range check to be pixels
   * @param ps            Source piece
   * @return              Sum of properties
   */
  public Object sumRange(Object propertyName, Object expression, Object minRange, Object maxRange, Boolean asPixels, PropertySource ps) {
    final String property = propertyName == null ? null : propertyName.toString();
    return sumOrCountRange(property, expression, minRange, maxRange, asPixels, true, ps);
  }

  /**
   * Implementation of all flavours of CountRange and CounrRangePx functions
   *
   * @param propertyOrExpression  Property name to sum OR Expression if no property supplied
   * @param expression            Optional expression to select targets
   * @param minRange              minimum Range
   * @param maxRange              maximum range
   * @param asPixels              true to force range check to be pixels
   * @param ps                    Source piece
   * @return                      Count of matching pieces
   */
  public Object countRange(Object propertyOrExpression, Object expression, Object minRange, Object maxRange, Boolean asPixels, PropertySource ps) {
    String prop = propertyOrExpression.toString();
    String expr = expression.toString();

    // Check if an Expression was supplied in the prop argument.
    if (prop.startsWith("{")) {
      expr = prop;
      prop = "";
    }

    return sumOrCountRange(prop, expr, minRange, maxRange, asPixels, false, ps);
  }

  /**
   * Internal function to process all ranged Sum and Count functions
   * @param propertyName  For Sum, name of property to be Summed. For count, Only count target piece if it has a non-blank value for this property.
   * @param expression    Selection expression
   * @param minRange      Minimum range to check
   * @param maxRange      Maximum range to check
   * @param asPixels      True to use the min/max range as pixels. False to use as cells.
   * @param doSum         True to Sum, false to Count
   * @param ps            The initiating piece
   * @return              Sum or Count
   */
  private Object sumOrCountRange(String propertyName, Object expression, Object minRange, Object maxRange, boolean asPixels, boolean doSum, PropertySource ps) {

    final PieceFilter filter = createFilter(expression, ps);

    if (!(ps instanceof GamePiece)) return 0;

    int result = 0;

    final GamePiece sourcePiece = (GamePiece) ps;
    if (sourcePiece == null || sourcePiece.getMap() == null) {
      return result;
    }

    int min = IntPropValue(minRange);
    if (min < 0) min = 0;
    int max = IntPropValue(maxRange);
    if (max < min) max = min;


    final Point position = sourcePiece.getPosition();

    // Ask the IndexManager for the pieces within the maximum range
    // NOTE: The IndexManager will NOT return the source piece in the list if the minRange is 0. We need to handle this separately.
    for (final GamePiece piece : GameModule.getGameModule().getIndexManager().getPieces(sourcePiece, max, asPixels)) {
      // Check the minimum range
      if (min > 0) {
        final int range = range(position, piece.getPosition(), sourcePiece.getMap(), asPixels);
        if (range < min) continue;
      }
      result = updateTotal(result, piece, propertyName, filter, doSum);
    }

    // If the range is 0, check our sourcepiece as well.
    if (min == 0) {
      result = updateTotal(result, sourcePiece, propertyName, filter, doSum);
    }

    return result;
  }

  /**
   * Update the total for a given piece, Check if it meets the requirements and return an updated total depending on the
   * type being processed.
   *
   * @param currentTotal  The current running total
   * @param piece         The piece to be tested
   * @param propertyName  If doSum is true, then the property to be summed, for doSum false, check it has a non-null value
   * @param filter        Filter the piece must pass
   * @param doSum         True if summing a property total, false if counting
   * @return              Updated total
   */
  private int updateTotal(int currentTotal, GamePiece piece, String propertyName, PieceFilter filter, boolean doSum) {
    int newTotal = currentTotal;

    // Check if this piece fails the filter. Null filter means no filter expression specified and we accept the piece.
    if (filter != null && !filter.accept(piece)) {
      // Failed, return the current total
      return newTotal;
    }

    // Find the value of the specified property in the piece being tested
    String value = "";
    if (propertyName != null && !propertyName.isEmpty()) {
      value = (String) piece.getProperty(propertyName);
      if (value == null) value = "";
    }

    if (doSum) {
      // Summing - add the integer value of the property value, which will add 0 if not integer value found for the property
      newTotal += IntPropValue(value);
    }
    else {
      // Counting - Add 1 if the property name was not supplied, or the value of the property is non-blank.
      if (propertyName == null || propertyName.isEmpty() || !value.isEmpty()) {
        newTotal++;
      }
    }

    return newTotal;
  }

  /*
   * Random Numbers
   *
   * - random(max)       - Return a Random integer between 1 and max inclusive (init_beanshell.bsh calls random(1, max))
   * - random(min, max)  - Return a Random integer between min and max inclusive
   * - isRandom(percent) - Return true percent% of the time
   * - isRandom()        - Equivalent to Random(50) (init_beanshell.bsh calls isRandom(50))
   */

  public Object random(Object src, Object minString, Object maxString) {
    final int min = parseInt(src, "Random", minString, 1); // NON-NLS
    int max = parseInt(src, "Random", maxString, 1); // NON-NLS
    if (max < min) {
      max = min;
    }
    if (min == max) {
      return min;
    }
    final int range = max - min + 1;
    return GameModule.getGameModule().getRNG().nextInt(range) + min;
  }

  public Object isRandom(Object src, Object percentString) {
    int percent = parseInt(src, "IsRandom", percentString, 50); // NON-NLS
    if (percent < 0)
      percent = 0;
    if (percent > 100)
      percent = 100;
    final int r = GameModule.getGameModule().getRNG().nextInt(100) + 1;
    return r <= percent;
  }

  private int parseInt(Object src, String function, Object value, int dflt) {
    int result = dflt;
    try {
      result = Integer.parseInt(value.toString());
    }
    catch (Exception e) {
      final String message = "Illegal number in call to Beanshell function " + function + ". " + ((src instanceof Decorator) ? "Piece= [" + ((Decorator) src).getProperty(BasicPiece.BASIC_NAME) + "]. " : ""); //NON-NLS
      final String data = "Data=[" + value.toString() + "]."; //NON-NLS
      ErrorDialog.dataWarning(new BadDataReport(message, data, e));
    }
    return result;
  }

  /*
   * Sum & Count
   *
   * Note that these methods are called from Beanshell and all arguments are passed
   * as Object types.
   *
   * Sum (property, match)      - Sum named property in units on all maps matching match
   * Sum (property, match, map) - Sum named property in units on named map matching match
   * Count (match)              - Count units on all maps matching match
   * Count (match, map)         - Count units on named map matching match

   */
  public Object sum(Object src, Object propertyName, Object propertyMatch) {
    return sum(src, propertyName, propertyMatch, null);
  }

  public Object sum(Object src, Object propertyName, Object propertyMatch, Object mapName) {
    int result = 0;
    List<Map> mapList = new ArrayList<>();

    if (! (src instanceof PropertySource)) return 0;
    if (! (propertyName instanceof String)) return 0;
    if (! (propertyMatch == null || propertyMatch instanceof String)) return 0;
    if (! (mapName == null || mapName instanceof String)) return 0;

    final PieceFilter filter = createFilter((String) propertyMatch, (PropertySource) src);

    if (src instanceof GamePiece) {
      mapList = getMapList(mapName, (GamePiece) src);
    }
    else if (mapName != null && !((String) mapName).isEmpty()) {
      mapList.add(findVassalMap((String) mapName));
    }
    else if (src instanceof GameModule) {
      mapList = Map.getMapList();
    }
    else if (src instanceof Map) {
      mapList.add((Map) src);
    }
    else if (src instanceof ReportState.OldAndNewPieceProperties) {
      mapList.add(((ReportState.OldAndNewPieceProperties) src).getNewPiece().getMap());
    }
    else {
      return 0;
    }

    for (final Map map : mapList) {
      if (map != null) {
        for (final GamePiece piece : map.getAllPieces()) {
          if (piece instanceof Stack) {
            for (final GamePiece p : ((Stack) piece).asList()) {
              result += getIntPropertyValue(p, filter, (String) propertyName);
            }
          }
          else {
            result += getIntPropertyValue(piece, filter, (String) propertyName);
          }
        }
      }
    }

    return result;
  }

  private int getIntPropertyValue(GamePiece piece, PieceFilter filter, String propertyName) {
    if (filter == null || filter.accept(piece)) {
      try {
        return Integer.parseInt((String) piece.getProperty(propertyName));
      }
      catch (Exception ignored) {

      }
    }
    return 0;
  }

  public Object count(Object src, Object propertyMatch) {
    return count(src, propertyMatch, null);
  }

  public Object count(Object src, Object propertyMatch, Object mapName) {

    int result = 0;
    List<Map> mapList = new ArrayList<>();

    if (! (src instanceof PropertySource)) return 0;
    if (! (propertyMatch == null || propertyMatch instanceof String)) return 0;
    if (! (mapName == null || mapName instanceof String)) return 0;

    final String matchString = replaceDollarVariables((String) propertyMatch, (PropertySource) src);
    final PieceFilter filter = matchString == null ? null : new PropertyExpression(unescape(matchString)).getFilter((PropertySource) src);

    if (src instanceof GamePiece) {
      mapList = getMapList(mapName, (GamePiece) src);
    }
    else if (mapName != null && !((String) mapName).isEmpty()) {
      mapList.add(findVassalMap((String) mapName));
    }
    else if (src instanceof GameModule) {
      mapList = Map.getMapList();
    }
    else if (src instanceof Map) {
      mapList.add((Map) src);
    }
    else if (src instanceof ReportState.OldAndNewPieceProperties) {
      mapList.add(((ReportState.OldAndNewPieceProperties) src).getNewPiece().getMap());
    }
    else {
      return 0;
    }

    for (final Map map : mapList) {
      if (map != null) {
        for (final GamePiece piece : map.getAllPieces()) {
          if (piece instanceof Stack) {
            for (final GamePiece p : ((Stack) piece).asList()) {
              if (filter == null || filter.accept(p)) {
                result++;
              }
            }
          }
          else {
            if (filter == null || filter.accept(piece)) {
              result++;
            }
          }
        }
      }
    }

    return result;
  }

  private String unescape(String expr) {
    return expr.replace("\\\"", "\"");
  }

  /**
   * Return a list of Maps of interest
   * @param mapName Map Name to search for. If null, then return all maps
   * @param sourcePiece A piece to use for a shortcut check for current map
   * @return List of all maps, or specific map requested
   */
  private List<Map> getMapList(Object mapName, GamePiece sourcePiece) {

    if (mapName == null) {
      return Map.getMapList();
    }

    // Shortcut - See if the parent piece for our source piece is the map we want (most likely)
    if (sourcePiece != null && sourcePiece.getMap() != null && sourcePiece.getMap().getMapName().equals(mapName)) {
      return List.of(sourcePiece.getMap());
    }

    // Otherwise, search all maps for the one we want
    else {
      for (final Map map : Map.getMapList()) {
        if (map.getMapName().equals(mapName)) {
          return List.of(map);
        }
      }
    }

    return new ArrayList<>();
  }

  private Map findVassalMap(String mapName) {
    for (final Map map : Map.getMapList()) {
      if (map.getMapName().equals(mapName)) {
        return map;
      }
    }
    return null;
  }

  /**
   * Utility function to replace $xxx$ variables with values from a GamePiece
   *
   * @param expression Expression possibly containing $$ variables
   * @param src A GamePiece to use as a source for the $$ variable values
   *
   * @return Updated expression
   */
  private String replaceDollarVariables(String expression, GamePiece src) {
    return replaceDollarVariables(expression, (PropertySource) src);
  }

  private String replaceDollarVariables(String expression, PropertySource src) {
    if (expression == null || !expression.contains("$")) {
      return expression;
    }

    final StringBuilder buffer = new StringBuilder();
    int state = 0;
    final StringBuilder propertyName = new StringBuilder();

    for (int i = 0; i < expression.length(); i++) {
      final char c = expression.charAt(i);

      switch (state) {
      //
      // State 0 - Looking for a $ sign that may be the start of a $$ property expression
      //
      case 0:
        if (c == '$') {
          // Seen a '$', start collecting a property name
          state = 1;
          propertyName.setLength(0);
        }
        else {
          buffer.append(c);
        }
        break;
      // State 1 - Assembling a possible property name token
      case 1:
        if (c == '$') {
          // Closing '$' seen, is this a property with a value?
          final String propName = propertyName.toString();
          final Object prop = src == null ? null : src.getLocalizedProperty(propName);
          final String propertyValue = prop == null ? null : prop.toString();
          if (propertyValue == null) {
            // Not a property value. Add the initial '$' plus the assembled text to the output and start
            // looking for a new property name from the end '$' sign.
            buffer.append('$');
            buffer.append(propertyName);
            propertyName.setLength(0);
            state = 1;
          }
          else {
            // This is a valid property value, add it to the output string and start looking for next property.
            buffer.append(propertyValue);
            state = 0;
          }
        }
        else {
          propertyName.append(c);
        }
        break;
      }
    }

    // End of String reached. If we are still in state 1, then the we need to copy over
    // the token in progress plus its initiating '$' sign
    if (state == 1) {
      buffer.append('$');
      buffer.append(propertyName);
    }

    return buffer.toString();
  }

  /**
   * Audit
   * Write a message to the errorlog
   *
   * @param message               Message to display. Can be an Expression
   * @param conditionOrOptionList If provided, is an expression that will be evaluated and the message will only be written if true
   * @param optionList            A String containing any of the following letters to invoke various options
   *                             'F' - Include full audit of the current expression
   *                             'C' - Display in Chatter
   *                             'S' - Suppress logging to errorLog
   * @return                     Returns a blank string
   */
  public Object audit(Object message, Object conditionOrOptionList, Object optionList, PropertySource ps) {

    // If this is being called from a GamePiece that isn't on Map, do nothing
    if (ps instanceof GamePiece && ((GamePiece) ps).getMap() == null) return "";

    // Arg 2 may be the condition, or it may have been skipped and its the optionlist.
    String condition = "";
    String options = optionList == null ? "" : optionList.toString();
    final String combined = conditionOrOptionList == null ? "" : conditionOrOptionList.toString();
    if (!combined.isEmpty()) {
      if (combined.startsWith("{")) {
        condition = combined;
      }
      else {
        options = combined;
      }
    }

    // Evaluate condition if supplied, skip reporting if not true
    if (!condition.isEmpty()) {
      final Expression expr = BeanShellExpression.createExpression(condition);
      final String result = expr.quietEvaluate(ps, null, "");
      if (!("true".equals(result))) {
        return "";
      }
    }

    String mess = "Audit: " + message.toString();
    if (mess.startsWith("{")) {
      final Expression expr = BeanShellExpression.createExpression(mess);
      mess = expr.quietEvaluate(ps, null, "");
    }

    // Add full Expression audit if requested
    if (options.indexOf('F') >= 0) {
      mess += "\n" + currentAudit.toString();
    }

    // Log to error log if not suppressed
    if (options.indexOf('S') < 0) {
      logger.warn(mess);
    }

    // Log to Chatter if requested
    if (options.indexOf('C') >= 0) {
      GameModule.getGameModule().warn(mess);
    }

    return "";
  }

  /**
   * Refresh the screen, then delay for the specified number of milliseconds
   *
   * This is not pretty, but is the only way I have found to force a proper UI refresh
   * - Open a modal dialog box way offscreen. This forces a an actual real Swing UI refresh, then hangs the UI
   * - Start a new thread that closes the dialog box after a specified delay
   *
   * @param ms  Milliseconds to delay
   * @param ps  A Property source
   * @return    blank String
   */
  public Object sleep(Object ms, PropertySource ps) {

    final int milliSeconds = IntPropValue(ms);
    final JDialog dialog = new JDialog(GameModule.getGameModule().getPlayerWindow(), true);
    dialog.setLocation(-5000, -5000);
    SwingUtilities.invokeLater(new DialogCloser(dialog, milliSeconds));

    dialog.setVisible(true);
    return "";
  }


}

