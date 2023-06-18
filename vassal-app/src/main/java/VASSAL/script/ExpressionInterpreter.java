/*
 *
 * Copyright (c) 2008-2020 by Brent Easton
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
import VASSAL.counters.SetAttachmentProperty;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
import VASSAL.script.expression.ExpressionException;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.WarningDialog;

import bsh.BeanShellExpressionValidator;
import bsh.EvalError;
import bsh.NameSpace;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    // logger.info("Attempting to load " + INIT_SCRIPT + " URI generated=" + ini); // No longer required

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
          audit.addMessage(origName + "=" + (value == null ? "" : value));
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

            try {
              setVar(var, Float.parseFloat(value));
            }
            catch (NumberFormatException ex2) {
              setVar(var, value);
            }
          }
        }
      }

      final StringBuilder argList = new StringBuilder();
      for (final String var : stringVariables) {
        if (argList.length() > 0) {
          argList.append(',');
        }
        final Object value = localized ? source.getLocalizedProperty(var) : source.getProperty(var);
        argList.append('"').append(value == null ? "" : value.toString().replace("\"", "\\\"")).append('"');
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

  /*****************************************************************
   * Callbacks from BeanShell Expressions to Vassal
   **/

  public Object getProperty(String name) {
    final Object value = source.getProperty(name);
    return value == null ? "" : wrap(value.toString());
  }

  public Object getLocalizedProperty(String name) {
    final Object value = source.getLocalizedProperty(name);
    return value == null ? "" : wrap(value.toString());
  }

  /**
   * getProperty minus the wrap
   */
  public Object getString(String name) {
    final Object value = source.getProperty(name);
    return value == null ? "" : value.toString();
  }

  public Object getZoneProperty(String propertyName, String zoneName) {
    if (source instanceof GamePiece) {
      final Map map = ((GamePiece) source).getMap();
      if (map != null) {
        final Zone zone = map.findZone(zoneName);
        if (zone != null) {
          return wrap((String) zone.getProperty(propertyName));
        }
      }
    }
    return wrap("");
  }

  public Object getZoneProperty(String propertyName, String zoneName, String mapName) {
    final Map map = findVassalMap(mapName);
    if (map != null) {
      final Zone zone = map.findZone(zoneName);
      if (zone != null) {
        return wrap((String) zone.getProperty(propertyName));
      }
    }
    return wrap("");
  }

  public Object getMapProperty(String propertyName, String mapName) {
    final Map map = findVassalMap(mapName);
    return map == null ? wrap("") : wrap((String) map.getProperty(propertyName));
  }

  public Object getAttachmentProperty(String attachment, String property, String indexOrName, PropertySource ps) {
    ps = translatePiece(ps);

    final int index = NumberUtils.isParsable(indexOrName) ? NumberUtils.toInt(indexOrName) : -1;

    if (ps instanceof GamePiece) {
      GamePiece p = Decorator.getOutermost((Decorator) ps);
      while (p instanceof Decorator) {
        if (p instanceof Attachment) {
          final Attachment a = (Attachment) p;
          if (a.getAttachName().equals(attachment)) {
            if (index > 0) {
              final GamePiece target = a.getAttachedPieceAt(index - 1);
              if (target == null) return "";
              return target.getProperty(property);
            }
            else {
              final String myName = (String)Decorator.getOutermost((Decorator)ps).getProperty(BasicPiece.BASIC_NAME);
              if (myName != null) {
                for (final GamePiece target : a.getAttachList()) {
                  final String name = (String) target.getProperty(BasicPiece.BASIC_NAME);
                  if (myName.equals(name)) {
                    return target.getProperty(property);
                  }
                }
              }
            }
          }
        }
        p = ((Decorator) p).getInner();
      }
    }
    return "";
  }

  private PropertySource translatePiece(PropertySource ps) {
    // Allows SetAttachmentProperty to use these functions correctly
    if (ps instanceof SetAttachmentProperty.SetAttachmentPropertySource) {
      ps = ((SetAttachmentProperty.SetAttachmentPropertySource) ps).getPiece();
    }
    // This allows ReportState to sum properties properly
    else if (ps instanceof ReportState.OldAndNewPieceProperties) {
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

  private static int propNonempty(Object prop) {
    if (prop != null) {
      final String val = prop.toString();
      if (!"".equals(val)) {
        return 1;
      }
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
  public Object sumStack(String property, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);
    if (ps instanceof GamePiece) {
      final Stack s = ((GamePiece) ps).getParent();
      if (s == null) {
        final Object prop = ps.getProperty(property);
        result += IntPropValue(prop);
      }
      else {
        for (final GamePiece gamePiece : s.asList()) {
          final Object prop = gamePiece.getProperty(property);
          result += IntPropValue(prop);
        }
      }
    }
    return result;
  }

  /**
   * CountStack(property) function count the number of pieces in
   * the same stack which have any non-blank value for the
   * specified property.
   *
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object countStack(String property, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      final Stack s = ((GamePiece) ps).getParent();
      if (s == null) {        
        if ("".equals(property)) {
          result++;
        }
        else {
          final Object prop = ps.getProperty(property);
          result += propNonempty(prop);
        }
      }
      else {
        if ("".equals(property)) {
          result = s.nVisible(); // Blank property returns number of visible-to-me pieces in the stack
        }
        else {
          for (final GamePiece gamePiece: s.asList()) {
            final Object prop = gamePiece.getProperty(property);
            result += propNonempty(prop);
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
  public Object maxAttachment(String attachment, String property, PropertySource ps) {
    int result = Integer.MIN_VALUE;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      GamePiece p = Decorator.getOutermost((Decorator)ps);
      while (p instanceof Decorator) {
        if (p instanceof Attachment) {
          final Attachment a = (Attachment)p;
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
        p = ((Decorator) p).getInner();
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
  public Object minAttachment(String attachment, String property, PropertySource ps) {
    int result = Integer.MAX_VALUE;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      GamePiece p = Decorator.getOutermost((Decorator)ps);
      while (p instanceof Decorator) {
        if (p instanceof Attachment) {
          final Attachment a = (Attachment)p;
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
        p = ((Decorator) p).getInner();
      }
    }

    return (result == Integer.MAX_VALUE) ? 0 : result;
  }


  /**
   * SumAttachment(attachment, property) function
   * Total the value of the named property in all pieces
   * attached
   *
   * @param attachment Attachment Name
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object sumAttachment(String attachment, String property, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      GamePiece p = Decorator.getOutermost((Decorator)ps);
      while (p instanceof Decorator) {
        if (p instanceof Attachment) {
          final Attachment a = (Attachment)p;
          if (a.getAttachName().equals(attachment)) {
            for (final GamePiece target : a.getAttachList()) {
              final Object prop = target.getProperty(property);
              result += IntPropValue(prop);
            }
          }
        }
        p = ((Decorator) p).getInner();
      }
    }
    return result;
  }


  /**
   * CountAttachment(property) function count the number of pieces
   * attached to this piece via a named Attachment trait that
   * *contain* the specified property
   *
   * @param attachment Attachment Name
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object countAttachment(String attachment, String property, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      for (final GamePiece target : Attachment.getAttachList((GamePiece) ps, attachment)) {
        if ("".equals(property)) {
          result++;
        }
        else {
          final Object prop = target.getProperty(property);
          result += propNonempty(prop);
        }
      }
    }
    return result;
  }


  /**
   * CountAttachmentExpression(attachment, expression) function count the number of pieces
   * attached to this piece via a named Attachment trait for which the expression is true
   *
   * @param attachment Attachment Name e.g. "MyTurrets"
   * @param expression Expression e.g. " { Ammo > 2 } "
   * @param ps       GamePiece
   * @return total
   */
  public Object countAttachmentExpression(String attachment, String expression, PropertySource ps) {
    int result = 0;

    ps = translatePiece(ps);

    if (ps instanceof GamePiece) {
      for (final GamePiece target : Attachment.getAttachList((GamePiece) ps, attachment)) {
        // We wait to run the beanshell until we've found a matching attachment, since there should properly speaking only be precisely 0 or 1 matches.
        final String matchString = replaceDollarVariables(expression, ps);
        final PieceFilter filter = matchString == null ? null : new PropertyExpression(unescape(matchString)).getFilter(ps);
        if (filter == null || filter.accept(target)) {
          result++;
        }
      }
    }

    return result;
  }


  /**
   * SumMat(property) function
   * Total the value of the named property in all counters
   * among the Mat-and-MatCargo grouping of the current piece
   *
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object sumMat(String property, PropertySource ps) {
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

        Object prop = mat.getProperty(property);
        result += IntPropValue(prop);

        for (final GamePiece cargo : actualMat.getContents()) {
          prop = Decorator.getOutermost(cargo).getProperty(property);
          result += IntPropValue(prop);
        }
      }
      else {
        final Object prop = ps.getProperty(property);
        result += IntPropValue(prop);
      }
    }
    return result;
  }

  /**
   * CountMat(property) function
   * Return the total number of counters with a non-blank value for the specified property
   * among the Mat-and-MatCargo grouping of the current piece
   *
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object countMat(String property, PropertySource ps) {
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

        Object prop = mat.getProperty(property);
        result += propNonempty(prop);

        for (final GamePiece cargo : actualMat.getContents()) {
          prop = Decorator.getOutermost(cargo).getProperty(property);
          result += propNonempty(prop);
        }
      }
      else {
        final Object prop = ps.getProperty(property);
        result += propNonempty(prop);
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
  public Object sumLocation(String property, PropertySource ps) {
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
  public Object sumLocation(String property, String expression, PropertySource ps) {

    if (ps instanceof GamePiece) {
      final GamePiece p = (GamePiece) ps;
      final Map m = p.getMap();
      if (m != null) {
        final String here = m.locationName(p.getPosition());
        return sumMapLocation(property, here, m.getConfigureName(), expression, ps);
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
  public Object sumMapLocation(String property, String location, String map, PropertySource ps) {
    return sumMapLocation(property, location, map, null, ps);
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

  public Object sumMapLocation(String property, String location, String map, String expression, PropertySource ps) {

    ps = translatePiece(ps);

    final String matchString = replaceDollarVariables((String) expression, ps);
    final PieceFilter filter = matchString == null ? null : new PropertyExpression(unescape(matchString)).getFilter((PropertySource) ps);
    final Map targetMap = findVassalMap(map);

    return targetMap == null ? 0 : sumLocation(property, location, targetMap, filter);
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
      final Object propertyValue = piece.getProperty(property);
      if (filter == null || filter.accept(piece)) {
        result += IntPropValue(propertyValue);
      }
    }
    return result;
  }

  /**
   * CountLocation() function.
   * Return count of pieces in the same location as the current piece
   * @return  Piece Count
   */
  public Object countLocation(PropertySource ps) {
    return countLocation("", ps);
  }

  /**
   * CountLocation(property) and CountLocation(expression) functions.
   * Return count of pieces in the same location as the current piece that have a non-nlank value for a property or that match an expression
   * @param   propertyOrExpression  Property Name or Match Expression
   * @return  Piece Count
   */
  public Object countLocation(String propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.trim().startsWith("{")) {
      return countLocation(null, propertyOrExpression, ps);
    }
    else {
      return countLocation(propertyOrExpression, null, ps);
    }
  }

  /**
   * CountLocation(property, expression) functions.
   * Return count of pieces in the same location as the current piece that have a non-nlank value for a property and that match an expression
   * @param   property      Property Name
   * @param   expression    Match Expression
   * @return  Piece Count
   */

  public Object countLocation(String property, String expression, PropertySource ps) {
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

  public Object countMapLocation(String locationName, String mapName, PropertySource ps) {
    return countMapLocation(locationName, mapName, null, ps);
  }

  public Object countMapLocation(String locationName, String mapName, String propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.trim().startsWith("{")) {
      return countMapLocation(locationName, mapName, null, propertyOrExpression, ps);
    }
    else {
      return countMapLocation(locationName, mapName, propertyOrExpression, "", ps);
    }
  }

  public Object countMapLocation(String locationName, String mapName, String property, String expression, PropertySource ps) {
    ps = translatePiece(ps);

    final String matchString = replaceDollarVariables(expression, ps);
    final PieceFilter filter = matchString == null ? null : new PropertyExpression(unescape(matchString)).getFilter((PropertySource) ps);
    final Map targetMap = findVassalMap(mapName);

    String propValue;
    if (property == null || property.isEmpty()) {
      propValue = null;
    }
    else {
      propValue = property;
    }

    return targetMap == null ? 0 : countLocation(locationName, targetMap, propValue, filter);
  }

  /**
   * Lowest-level CountLocation function called by all other versions
   * @param locationName Location Name to search for
   * @param map          Map to search on
   * @param propValue    null if no property was supplied, or property name if supplied
   * @param filter       Option filter implementing Property Match Expression
   * @return             Count of pieces
   */
  private Object countLocation(String locationName, Map map, String property, PieceFilter filter) {
    int result = 0;

    // Ask IndexManager for list of pieces on that map at that location. Stacks are not returned by the IM.
    for (final GamePiece piece : GameModule.getGameModule().getIndexManager().getPieces(map, BasicPiece.LOCATION_NAME, locationName)) {
      // If a property was supplied to be checked and has an empty value, then skip this piece in the count.
      // If a property was not supplied (propValue == null), then the piece is counted.
      if (property != null && !property.isEmpty()) {
        final String propValue = (String) piece.getProperty(property);
        if (propNonempty(propValue) == 0) {
          continue;
        }
      }

      // Add 1 to count if the no filter was supplied, or the piece passed the filter.
      if (filter == null || filter.accept(piece)) {
        result += 1;
      }
    }

    return result;
  }


  /**
   * SumZone(property) function
   * Total the value of the named property in all counters in the
   * same zone as the specified piece.
   * <p>
   * @param property Property Name
   * @param ps       GamePiece
   * @return total
   */
  public Object sumZone(String property, PropertySource ps) {
    return sumZone(property, "", ps);
  }

  /**
   * SumZone(property, expression) function
   * Total the value of the named property in all counters in the
   * same zone as the specified piece that meet the supplied expression.
   * <p>
   * @param property   Property Name
   * @param expression Expression
   * @param ps         GamePiece Source
   * @return total
   */
  public Object sumZone(String property, String expression, PropertySource ps) {

    if (ps instanceof GamePiece) {
      final GamePiece p = (GamePiece) ps;
      final Map m = p.getMap();
      if (m != null) {
        final String zone = (String) p.getProperty(BasicPiece.CURRENT_ZONE);
        return sumMapZone(property, zone, m.getConfigureName(), expression, ps);
      }
    }

    return 0;
  }

  /**
   * SumMapZone(property, location, map) function
   * Total the value of the named property in all counters in the
   * specified map and zone
   * <p>
   * @param property   Property Name
   * @param zoneName   Zone Name
   * @param map        Map Name
   * @param ps         GamePiece source
   * @return total
   */
  public Object sumMapZone(String property, String zoneName, String map, PropertySource ps) {
    return sumMapZone(property, zoneName, map, null, ps);
  }

  /**
   * SumMapZone(property, location, map, expression) function
   * Total the value of the named property in all counters in the
   * specified map and zone that match the supplied expression
   * <p>
   * @param property   Property Name
   * @param zoneName   Zone Name
   * @param map        Map Name
   * @param expression Expression
   * @param ps         GamePiece source
   * @return total
   */

  public Object sumMapZone(String property, String zoneName, String map, String expression, PropertySource ps) {

    ps = translatePiece(ps);

    final String matchString = replaceDollarVariables((String) expression, ps);
    final PieceFilter filter = matchString == null ? null : new PropertyExpression(unescape(matchString)).getFilter((PropertySource) ps);
    final Map targetMap = findVassalMap(map);

    return targetMap == null ? 0 : sumZone(property, zoneName, targetMap, filter);
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
      final Object propertyValue = piece.getProperty(property);
      if (filter == null || filter.accept(piece)) {
        result += IntPropValue(propertyValue);
      }
    }
    return result;
  }

  /**
   * CountZone() function.
   * Return count of pieces in the same zone as the current piece
   * @return  Piece Count
   */
  public Object countZone(PropertySource ps) {
    return countZone("", ps);
  }

  /**
   * CountZone(property) and CountZone(expression) functions.
   * Return count of pieces in the same zone as the current piece that have a non-blank value for a property or that match an expression
   * @param   propertyOrExpression  Property Name or Match Expression
   * @return  Piece Count
   */
  public Object countZone(String propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.trim().startsWith("{")) {
      return countZone(null, propertyOrExpression, ps);
    }
    else {
      return countZone(propertyOrExpression, null, ps);
    }
  }

  /**
   * CountLocation(zone, expression) function
   * Return count of pieces in the same zome as the current piece that have a non-blank value for a property and that match an expression
   * @param   property      property name
   * @param   expression    match expression
   * @return  Piece Count
   */

  public Object countZone(String property, String expression, PropertySource ps) {
    if (ps instanceof GamePiece) {
      final GamePiece p = (GamePiece) ps;
      final Map m = p.getMap();
      if (m != null) {
        final String here = (String) p.getProperty(BasicPiece.CURRENT_ZONE);
        return countMapZone(here, m.getConfigureName(), property, expression, ps);
      }
    }
    return 0;
  }

  /**
   * CountMapZone(Zone, Map) function
   * Return the count of pieces in the specified Zone/Map
   *
   * @param zoneName  Target Zone Name
   * @param mapName   Target Map Name
   * @param ps        Source Piece
   * @return          Count
   */
  public Object countMapZone(String zoneName, String mapName, PropertySource ps) {
    return countMapZone(zoneName, mapName, null, ps);
  }

  /**
   * CountMapZone(Zone, Map, property) and CountMapZone(Zone, Map, expression) function
   * Return the count of pieces in the specified Zone/Map that match the supplied expression OR
   * have a non-blank value for the supplied property name
   *
   * @param zoneName              Target Zone Name
   * @param mapName               Target Map Name
   * @param propertyOrExpression  A Property Name or a Beanshell expression
   * @param ps                    Source Piece
   * @return                      Count
   */
  public Object countMapZone(String zoneName, String mapName, String propertyOrExpression, PropertySource ps) {
    if (propertyOrExpression != null && propertyOrExpression.trim().startsWith("{")) {
      return countMapZone(zoneName, mapName, null, propertyOrExpression, ps);
    }
    else {
      return countMapZone(zoneName, mapName, propertyOrExpression, "", ps);
    }
  }

  /**
   * CountMapZone(Zone, Map, property, expression) function
   * Return the count of pieces in the specified Zone/Map that match the supplied expression AND
   * have a non-blank value for the supplied property name
   *
   * @param zoneName              Target Zone Name
   * @param mapName               Target Map Name
   * @param property              A Property Name
   * @param expression            A Beanshell expression
   * @param ps                    Source Piece
   * @return                      Count
   */
  public Object countMapZone(String zoneName, String mapName, String property, String expression, PropertySource ps) {
    ps = translatePiece(ps);

    final String matchString = replaceDollarVariables(expression, ps);
    final PieceFilter filter = matchString == null ? null : new PropertyExpression(unescape(matchString)).getFilter((PropertySource) ps);
    final Map targetMap = findVassalMap(mapName);

    String propValue;
    if (property == null || property.isEmpty()) {
      propValue = null;
    }
    else {
      propValue = property;
    }

    return targetMap == null ? 0 : countZone(zoneName, targetMap, propValue, filter);
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
      // If a property was supplied to be checked and has an empty value, then skip this piece in the count.
      // If a property was not supplied (propValue == null), then the piece is counted.
      if (property != null && !property.isEmpty()) {
        final String propValue = (String) piece.getProperty(property);
        if (propNonempty(propValue) == 0) {
          continue;
        }
      }

      // Add 1 to count if the no filter was supplied, or the piece passed the filter.
      if (filter == null || filter.accept(piece)) {
        result += 1;
      }
    }

    return result;
  }

  /**
   * Implementation of Range functions
   */

  public int rangeInPixels(String attachmentName, PropertySource ps) {
    return rangeAttach(attachmentName, ps, true);
  }

  public int rangeInCells(String attachmentName, PropertySource ps) {
    return rangeAttach(attachmentName, ps, false);
  }

  private int rangeAttach(String attachmentName, PropertySource ps, boolean asPixels) {
    if (ps instanceof GamePiece) {
      final Map map = ((GamePiece) ps).getMap();
      final Point from = ((GamePiece) ps).getPosition();
      for (final GamePiece target : Attachment.getAttachList((GamePiece) ps, attachmentName)) {
        // Act on the first attached piece on the same Map that is not the source piece (in case of self-attachments)
        if (target != ps) {
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

  public int rangeInPixels(Object x, Object y, PropertySource ps) {
    return rangeXY(x, y, ps, true);
  }

  public int rangeInCells(Object x, Object y, PropertySource ps) {
    return rangeXY(x, y, ps, false);
  }

  private int rangeXY(Object x, Object y, PropertySource ps, boolean asPixels) {
    if (ps instanceof GamePiece) {
      final Map map = ((GamePiece) ps).getMap();
      final Point from = ((GamePiece) ps).getPosition();
      return range(from, new Point(IntPropValue(x), IntPropValue(y)), map, asPixels);
    }
    return 0;
  }

  public int rangeInPixels(Object x1, Object y1, Object x2, Object y2, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, (String) ps.getProperty(BasicPiece.CURRENT_MAP), true);
  }

  public int rangeInCells(Object x1, Object y1, Object x2, Object y2, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, (String) ps.getProperty(BasicPiece.CURRENT_MAP), false);
  }

  public int rangeInPixels(Object x1, Object y1, Object x2, Object y2, String mapName, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, mapName, true);
  }

  public int rangeInCells(Object x1, Object y1, Object x2, Object y2, String mapName, PropertySource ps) {
    return rangeMap(x1, y1, x2, y2, mapName, false);
  }

  private int rangeMap(Object x1, Object y1, Object x2, Object y2, String mapName, boolean asPixels) {
    final Point from = new Point(IntPropValue(x1), IntPropValue(y1));
    final Point to = new Point(IntPropValue(x2), IntPropValue(y2));
    final Map map = Map.getMapById(mapName);
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
}
