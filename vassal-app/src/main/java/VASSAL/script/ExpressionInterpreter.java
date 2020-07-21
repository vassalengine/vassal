/*
 *
 * Copyright (c) 2008-2009 by Brent Easton
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

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.script.expression.ExpressionException;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.io.IOUtils;
import bsh.BeanShellExpressionValidator;
import bsh.EvalError;
import bsh.NameSpace;

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
public class ExpressionInterpreter extends AbstractInterpreter {

  private static final long serialVersionUID = 1L;
  private static final Logger logger = LoggerFactory.getLogger(ExpressionInterpreter.class);

  protected static final String INIT_SCRIPT = "/VASSAL/script/init_expression.bsh";
  protected static final String THIS = "_interp";
  protected static final String SOURCE = "_source";
  protected static final String MAGIC1 = "_xyzzy";
  protected static final String MAGIC2 = "_plugh";
  protected static final String MAGIC3 = "_plover";


  // Top-level static NameSpace shared between all ExpressionInterpreters
  // Loaded with utility methods available to all interpreters
  protected static NameSpace topLevelNameSpace;

  protected NameSpace expressionNameSpace;

  //protected NameSpace localNameSpace;

  protected String expression;
  protected PropertySource source;
  protected List<String> variables = new ArrayList<>();

  // Maintain a cache of all generated Interpreters. All Expressions
  // with the same Expression use the same Interpreter.
  protected static HashMap<String, ExpressionInterpreter> cache = new HashMap<>();

  public static ExpressionInterpreter createInterpreter (String expr) throws ExpressionException {
    final String e = expr == null ? "" : strip(expr);
    ExpressionInterpreter interpreter = cache.get(e);
    if (interpreter == null) {
      interpreter = new ExpressionInterpreter(e);
      cache.put(e, interpreter);
    }
    return interpreter;
  }

  protected static String strip(String expr) {
    final String s = expr.trim();
    if (s.startsWith("{") && s.endsWith("}")) {
      return s.substring(1, s.length()-1);
    }
    return expr;
  }

  /**
   * Private constructor to build an ExpressionInterpreter. Interpreters
   * can only be created by createInterpreter.
   *
   * @param expr Expression
   * @throws ExpressionException
   */
  private ExpressionInterpreter(String expr) throws ExpressionException {
    super();

    expression = expr;

    // Install the Vassal Class loader so that bsh can find Vassal classes
    this.setClassLoader(this.getClass().getClassLoader());

    // Initialise the top-level name space if this is the first
    // expression to be created
    if (topLevelNameSpace == null) {
      initialiseStatic();
    }

    // Create the Expression level namespace as a child of the
    // top level namespace
    expressionNameSpace = new NameSpace(topLevelNameSpace, "expression");

    // Get a list of any variables used in the expression. These are
    // property names that will need to be evaluated at expression
    // evaluation time
    variables = new BeanShellExpressionValidator(expression).getVariables();

    // Build a method enclosing the expression. This saves the results
    // of the expression parsing, improving performance. Force return
    // value to a String as this is what Vassal is expecting.
    setNameSpace(expressionNameSpace);
    if (expression.length() > 0) {
      try {
        eval("String "+MAGIC2+"() { "+MAGIC3+"=" + expression + "; return "+MAGIC3+".toString();}");
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
    topLevelNameSpace = new NameSpace(null, getClassManager(), "topLevel");
    setNameSpace(topLevelNameSpace);
    getNameSpace().importClass("VASSAL.build.module.properties.PropertySource");
    getNameSpace().importClass("VASSAL.script.ExpressionInterpreter");

    // Read the Expression initialisation script into the top level namespace
    URL ini = getClass().getResource(INIT_SCRIPT);
    logger.info("Attempting to load "+INIT_SCRIPT+" URI generated="+ ini);

    try (InputStream is = ini.openStream();
         InputStreamReader isr = new InputStreamReader(is);
         BufferedReader in = new BufferedReader(isr)) {
      try {
        eval(in);
      }
      catch (EvalError e) {
        logger.error("Error trying to read init script: "+ ini);
        WarningDialog.show(e, "");
      }
    }
    catch (IOException e) {
      logger.error("Error trying to read init script: "+ ini);
      WarningDialog.show(e, "");
    }
  }

  /**
   * Return the current expression
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

    if (getExpression().length() == 0) {
      return "";
    }

    // Default to the GameModule to satisfy properties if no
    // GamePiece supplied.
    source = ps == null ? GameModule.getGameModule() : ps;

    setNameSpace(expressionNameSpace);

    // Bind each undeclared variable with the value of the
    // corresponding Vassal property. Allow for old-style $variable$ references
    for (String var : variables) {
      String name = var;
      if (name.length() > 2 && name.startsWith("$") && name.endsWith("$")) {
        name = name.substring(1, name.length()-1);
      }
      Object prop = localized ? source.getLocalizedProperty(name) : source.getProperty(name);
      String value = prop == null ? "" : prop.toString();
      if (value == null) {
        setVar(var, "");
      }
      else if ("true".equals(value)) {
        setVar(var, true);
      }
      else if ("false".equals(value)) {
        setVar(var, false);
      }
      else {
        try {
          setVar(var, Integer.valueOf(value).intValue());
        }
        catch (NumberFormatException e) {
          try {
            setVar(var, Float.valueOf(value).floatValue());
          }
          catch (NumberFormatException e1) {
            setVar(var, value);
          }
        }
      }
    }

    // Re-evaluate the pre-parsed expression now that the undefined variables have
    // been bound to their Vassal property values.

    setVar(THIS, this);
    setVar(SOURCE, source);

    String result = "";
    try {
      eval(MAGIC1+"="+MAGIC2+"()");
      result = get(MAGIC1).toString();
    }
    catch (EvalError e) {
      final String s = e.getRawMessage();
      final String search = MAGIC2+"();'' : ";
      final int pos = s.indexOf(search);
      throw new ExpressionException(getExpression(), s.substring(pos+search.length()));
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
   * @param value
   * @return wrapped value
   */
  public Object wrap(String value) {
    if (value == null) {
      return "";
    }
    else if ("true".equals(value)) {
      return Boolean.TRUE;
    }
    else if ("false".equals(value)) {
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
   * SumStack(property) function
   * Total the value of the named property in all counters in the
   * same stack as the specified piece.
   *
   * @param property Property Name
   * @param ps GamePiece
   * @return total
   */
  public Object sumStack(String property, PropertySource ps) {
    int result = 0;
    if (ps instanceof GamePiece) {
      Stack s = ((GamePiece) ps).getParent();
      if (s != null) {
        for (GamePiece gamePiece : s.asList()) {
          try {
            result +=
              Integer.parseInt(gamePiece.getProperty(property).toString());
          }
          catch (Exception e) {
            // Anything at all goes wrong trying to add the property, just ignore it and treat as 0
          }
        }
      }
    }
    return result;
  }

  /**
   * SumLocation(property) function
   * Total the value of the named property in all counters in the
   * same location as the specified piece.
   *
   * * WARNING * This WILL be inneficient as the number of counters on the
   * map increases.
   *
   * @param property Property Name
   * @param ps GamePiece
   * @return total
   */
  public Object sumLocation(String property, PropertySource ps) {
    int result = 0;
    if (ps instanceof GamePiece) {
      GamePiece p = (GamePiece) ps;
      Map m = p.getMap();
      if (m != null) {
        String here = m.locationName(p.getPosition());
        GamePiece[] pieces = m.getPieces();
        for (GamePiece piece : pieces) {
          if (here.equals(m.locationName(piece.getPosition()))) {
            if (piece instanceof Stack) {
              Stack s = (Stack) piece;
              for (GamePiece gamePiece : s.asList()) {
                try {
                  result += Integer.parseInt(gamePiece.getProperty(property).toString());
                }
                catch (NumberFormatException e) {
                  //
                }
              }
            }
            else {
              try {
                result += Integer.parseInt(piece.getProperty(property).toString());
              }
              catch (NumberFormatException e) {
                //
              }
            }
          }
        }
      }
    }
    return result;
  }
}
