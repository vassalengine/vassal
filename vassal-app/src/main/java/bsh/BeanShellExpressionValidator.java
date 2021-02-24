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
package bsh;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Validate a single line BeanShell expression. 
 * Build a list of variable references in the expression.
 * 
 * This Class must be defined in package bsh to allow access to 
 * package visible elements in the bsh library.
 *
 */
public class BeanShellExpressionValidator {
  
  protected String expression;
  protected List<String> variables = new ArrayList<>();
  protected List<String> stringVariables = new ArrayList<>();
  protected List<String> methods = new ArrayList<>();
  protected String error;
  protected boolean valid;

  /**
   * List of all Java 15 String functions that can be applied to property names directly within Beanshell.
   * (i.e. those with return values and argument types that Vassal supports).
   *
   * This list is used to recognize and evaluate constructs of the form
   *
   *    stringVariable.function()
   *
   * And allow stringVariable to be used directly within beanshell with Java syntax,
   * rather than having to resort to
   *
   *    GetProperty("stringVariable").function()
   *
   */
  private static final Set<String> supportedStringFunctions = Set.of (
    ".compareTo",
    ".compareToIgnoreCase",
    ".contains",
    ".endsWith",
    ".equals",
    ".equalsIgnoreCase",
    ".format",
    ".hashCode",
    ".indexOf",
    ".isBlank",
    ".isEmpty",
    ".lastIndexOf",
    ".length",
    ".matches",
    ".regionMatches",
    ".repeat",
    ".replace",
    ".replaceAll",
    ".replaceFirst",
    ".startsWith",
    ".strip",
    ".stripLeading",
    ".stripTrailing",
    ".substring",
    ".toLowerCase",
    ".toUpperCase",
    ".trim"
  );


  /**
   * Build a new Validator and validate the expression
   * @param expression Expression to validate
   */
  public BeanShellExpressionValidator(String expression) {
    this.expression = expression; 
    valid = validate();
  }
  
  /**
   * Is the expression valid?
   * @return valid
   */
  public boolean isValid() {
    return valid;
  }
  
  protected void setValid(boolean b) {
    valid = b;
  }

  /**
   * Return a list of Variable references in the expression that we know must be Strings
   * @return List of variables
   */
  public List<String> getStringVariables() {
    return stringVariables;
  }

  /**
   * Return a list of Variable references in the expression
   * @return List of variables
   */
  public List<String> getVariables() {
    return variables;
  }
  
  /**
   * Return a list of Methods called by the expression
   * @return List of Methods
   */
  public List<String> getMethods() {
    return methods;
  }
  
  /**
   * Return an Error Message if no valid
   * @return Error message
   */
  public String getError() {
    return error;
  }
  
  protected void setError(String s) {
    error = s;
  }
  
  /**
   * Validate the expression
   * 
   * @return Expression validity
   */
  protected boolean validate() {
    final String expr = stripBraces(expression);
    
    setError("");
    try {
      Parser p = new Parser(new StringReader(expr + ";"));
      for (;;) {
        if (p.Line()) {
          return true;
        } 
        else {
          final SimpleNode node = p.popNode();
          if (! processNode(node)) {
            return false;
          }
        }
      }
    }
    catch (ParseException e) {
      setError(e.getMessage());
      return false;
    }
    catch (TokenMgrError e) {
      setError(e.getMessage());
      return false;
    }
  }
  
  /**
   * If the expression is surrounded by Vassal expression braces {}
   * replace them with spaces so that it will validate and report errors
   * in the correct location
   * 
   * @param s Expression
   * @return stripped expression
   */
  public static String stripBraces(String s) {
    String expr = s;
    if (s.trim().startsWith("{") && s.trim().endsWith("}")) {
      final int start = s.indexOf("{");
      final int end = s.lastIndexOf("}");
      StringBuilder buffer = new StringBuilder(s.length());
      for (int i = 0; i < s.length(); i++) {
        if (i == start || i == end) {
          buffer.append(' ');
        }
        else {
          buffer.append(s.charAt(i));        
        }
      }
      expr = buffer.toString();
    }
    return expr;
  }
  
  /**
   * Process a Parser Node and extract any Variable and Method references.
   * Assignments are not allowed in an expression, so flag as an error
   * @param node Parser Node
   */
  protected boolean processNode (SimpleNode node) {
    if (node == null) {
      return true;
    }
    
    if (node instanceof BSHAmbiguousName) {
      final String name = node.getText().trim();
      if ((node.parent instanceof BSHMethodInvocation)) {
        if (! methods.contains(name)) {
          // Check for x.y() where y is a String method. x will be a property name we need to report
          // node.getText() returns the unknown method name with parts split by spaces. Break this into an array of tokens
          String[] tokens = name.split(" ");
          // Only 1 Token, it's a straight method, we're not interested.
          if (tokens.length == 1) {
            if (! methods.contains(name)) {
              methods.add(name);
            }
          }
          else {
            // If Token 2 is one of the String methods, then token 1 is a property name we need to report as a String variable
            if (supportedStringFunctions.contains(tokens[1])) {
              if (! stringVariables.contains(tokens[0])) {
                stringVariables.add(tokens[0]);
              }
            }
          }
        }
      }
      else if (! variables.contains(name)) {
        variables.add(name);
      }
    }
    else if (node instanceof BSHAssignment) {
      setError("Assignments (=) not allowed in Expressions. See Help");
      return false;
    }
    else {
      if (node.children != null) {
        for (int i = 0; i < node.children.length; i++) {
          if (! processNode(node.getChild(i))) {
            return false;
          }
        }
      }
    }
    return true;
  }  
  
}