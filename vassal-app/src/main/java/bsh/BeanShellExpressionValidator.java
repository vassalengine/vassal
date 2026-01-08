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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Validates BeanShell expressions and extracts variable and method references.
 *
 * <p>This validator parses single-line BeanShell expressions to:
 * <ul>
 * <li>Check syntax validity</li>
 * <li>Extract variable references</li>
 * <li>Identify string variables used with method calls</li>
 * <li>Collect method references</li>
 * </ul>
 *
 * <p>This class must be in package {@code bsh} to access package-visible elements
 * in the BeanShell library.
 *
 * @author Brent Easton
 * @since 2008
 */
public class BeanShellExpressionValidator {

  protected String expression;
  protected List<String> variables = new ArrayList<>();
  protected List<String> stringVariables = new ArrayList<>();
  protected List<String> methods = new ArrayList<>();
  protected String error = "";
  protected boolean valid;

  /**
   * Java String methods supported for direct use with property names in BeanShell.
   *
   * <p>These methods have return values and argument types that VASSAL supports,
   * allowing constructs like {@code stringVariable.function()} instead of
   * {@code GetProperty("stringVariable").function()}.
   */
  private static final Set<String> SUPPORTED_STRING_FUNCTIONS = Set.of(
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
    ".toString",
    ".trim"
  );


  /**
   * Creates a new validator and validates the given expression.
   *
   * @param expression the BeanShell expression to validate
   * @throws IllegalArgumentException if expression is null
   */
  public BeanShellExpressionValidator(String expression) {
    this.expression = Objects.requireNonNull(expression, "Expression cannot be null");
    this.valid = validate();
  }

  /**
   * Checks if the expression is syntactically valid.
   *
   * @return {@code true} if the expression is valid, {@code false} otherwise
   */
  public boolean isValid() {
    return valid;
  }

  protected void setValid(boolean b) {
    valid = b;
  }

  /**
   * Returns variable references that are used as strings (with string method calls).
   *
   * @return a list of string variable names
   */
  public List<String> getStringVariables() {
    return stringVariables;
  }

  /**
   * Returns all variable references in the expression.
   *
   * @return a list of variable names
   */
  public List<String> getVariables() {
    return variables;
  }

  /**
   * Returns all variable references (both regular and string variables).
   *
   * @return a list containing all variable names
   */
  public List<String> getAllVariables() {
    final Set<String> allVars = new LinkedHashSet<>(variables);
    allVars.addAll(stringVariables);
    return new ArrayList<>(allVars);
  }
  /**
   * Returns method calls found in the expression.
   *
   * @return a list of method names
   */
  public List<String> getMethods() {
    return methods;
  }

  /**
   * Returns the error message if validation failed.
   *
   * @return the error message, or empty string if no error
   */
  public String getError() {
    return error;
  }

  protected void setError(String errorMessage) {
    this.error = Objects.requireNonNullElse(errorMessage, "");
  }

  /**
   * Validates the expression using BeanShell parser.
   *
   * @return {@code true} if expression is valid, {@code false} otherwise
   */
  protected boolean validate() {
    if (expression.trim().isEmpty()) {
      setError("Expression cannot be empty");
      return false;
    }

    final String expr = stripBraces(expression);
    setError("");

    try {
      final Parser parser = new Parser(new StringReader(expr + ";"));
      while (true) {
        if (parser.Line()) {
          return true;
        } else {
          final SimpleNode node = parser.popNode();
          if (!processNode(node)) {
            return false;
          }
        }
      }
    } catch (ParseException e) {
      setError("Parse error: " + e.getMessage());
      return false;
    } catch (TokenMgrError e) {
      setError("Token error: " + e.getMessage());
      return false;
    } catch (Exception e) {
      setError("Unexpected error: " + e.getMessage());
      return false;
    }
  }

  /**
   * Strips VASSAL expression braces {@code {}} from the expression.
   *
   * <p>If the expression is surrounded by braces, they are replaced with spaces
   * to maintain correct error location reporting during validation.
   *
   * @param expression the expression to process
   * @return the expression with braces replaced by spaces
   * @throws IllegalArgumentException if expression is null
   */
  public static String stripBraces(String expression) {
    Objects.requireNonNull(expression, "Expression cannot be null");

    final String trimmed = expression.trim();
    if (!trimmed.startsWith("{") || !trimmed.endsWith("}")) {
      return expression;
    }

    final int startBrace = expression.indexOf('{');
    final int endBrace = expression.lastIndexOf('}');

    final StringBuilder result = new StringBuilder(expression.length());
    for (int i = 0; i < expression.length(); i++) {
      if (i == startBrace || i == endBrace) {
        result.append(' ');
      } else {
        result.append(expression.charAt(i));
      }
    }

    return result.toString();
  }

  /**
   * Processes a parser node to extract variable and method references.
   *
   * <p>Assignments are not allowed in expressions and will cause validation to fail.
   *
   * @param node the parser node to process
   * @return {@code true} if processing succeeded, {@code false} if an error occurred
   */
  protected boolean processNode(SimpleNode node) {
    if (node == null) {
      return true;
    }

    if (node instanceof BSHAmbiguousName) {
      return processAmbiguousName(node);
    } else if (node instanceof BSHAssignment) {
      setError("Assignments (=) are not allowed in expressions. See Help.");
      return false;
    } else {
      return processChildNodes(node);
    }
  }

  /**
   * Processes ambiguous name nodes to identify variables and method calls.
   */
  private boolean processAmbiguousName(SimpleNode node) {
    final String name = node.getText().trim();

    if (node.parent instanceof BSHMethodInvocation) {
      processMethodInvocation(name);
    } else {
      if (!variables.contains(name)) {
        variables.add(name);
      }
    }

    return true;
  }

  /**
   * Processes method invocation to identify string variables and methods.
   */
  private void processMethodInvocation(String name) {
    // Parse method calls like "variable.method()" where method is a String function
    final String[] tokens = name.split(" ");

    if (tokens.length == 1) {
      // Simple method call
      if (!methods.contains(name)) {
        methods.add(name);
      }
    } else if (tokens.length >= 2) {
      // Check if second token is a supported String method
      if (SUPPORTED_STRING_FUNCTIONS.contains(tokens[1])) {
        if (!stringVariables.contains(tokens[0])) {
          stringVariables.add(tokens[0]);
        }
      } else {
        if (!methods.contains(name)) {
          methods.add(name);
        }
      }
    }
  }

  /**
   * Recursively processes child nodes.
   */
  private boolean processChildNodes(SimpleNode node) {
    if (node.children != null) {
      for (Node child : node.children) {
        if (!processNode((SimpleNode) child)) {
          return false;
        }
      }
    }
    return true;
  }

}