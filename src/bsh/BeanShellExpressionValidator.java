/*
 * $Id: BeanShellExpressionValidator.java,v 1.1 2006/09/28 04:59:19 swampwallaby Exp $
 *
 * Copyright (c) 2008-2013 by Brent Easton
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
import java.util.List;

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
  protected List<String> variables = new ArrayList<String>();
  protected List<String> methods = new ArrayList<String>();
  protected String error;
  protected boolean valid = false;
  
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
   * @return
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
      StringBuffer buffer = new StringBuffer(s.length());
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
   * Assignemnts are not allowed in an expression, so flag as an error
   * @param node Parser Node
   */
  protected boolean processNode (SimpleNode node) {
    if (node == null) {
      return true;
    }
    
    if (node instanceof BSHAmbiguousName) {
      final String name = ((BSHAmbiguousName) node).getText().trim();
      if ((node.parent instanceof BSHMethodInvocation)) {
        if (! methods.contains(name)) {
          methods.add(name);
        }
      }
      else if (! variables.contains(name)) {
        variables.add(name);
      }
    }
    else if (node instanceof BSHAssignment) {
      setError("Assignemts (=) not allowed in Expressions. See Help");
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