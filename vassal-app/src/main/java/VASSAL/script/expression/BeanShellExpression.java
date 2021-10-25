/*
 *
 * Copyright (c) 2009-2013 Brent Easton
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
package VASSAL.script.expression;

import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.script.BeanShell;
import VASSAL.script.ExpressionInterpreter;
import VASSAL.tools.FormattedString;

import java.util.Map;

import org.apache.commons.lang3.tuple.Pair;

/**
 * A basic beanShell expression
 */
public class BeanShellExpression extends Expression {
  public BeanShellExpression(String s) {
    super("{" + s + "}");
  }

  protected ExpressionInterpreter interpreter;

  @Override
  protected void reset() {
    interpreter = null;
  }

  /**
   * Evaluate this expression using a BeanShell Interpreter
   *
   * @param ps A property source to use to evaluate properties referenced in the expression
   * @param properties A map of additional properties to use
   * @param localized Use localized version of property values
   * @param audit Audit Trail to record significant information for error reporting
   *
   * @return Evaluated expression value
   * @throws ExpressionException If a catastrophic failure occurs in evaluation
   */
  @Override
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized, Auditable owner, AuditTrail audit) throws ExpressionException {
    if (interpreter == null) {
      interpreter = new ExpressionInterpreter(strip(getExpression()));
    }
    return interpreter.evaluate(ps, properties, localized, owner, audit);
  }

  /** @deprecated Use {@link #evaluate(PropertySource, Map, boolean, Auditable, AuditTrail)} */
  @Deprecated(since = "2021-06-11")
  @Override
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized) throws ExpressionException {
    return evaluate(ps, properties, localized, null, null);
  }

  @Override
  public String toBeanShellString() {
    return strip(getExpression());
  }

  protected static String strip(String expr) {
    final String s = expr.trim();
    if (s.startsWith("{") && s.endsWith("}")) {
      return s.substring(1, s.length() - 1);
    }
    return expr;
  }

  protected boolean isDynamic() {
    return getExpression() != null && getExpression().indexOf('$') >= 0;
  }

  /**
   * Return a PieceFilter that selects GamePieces that cause
   * this expression to evaluate to true
   * @deprecated Use {@link #getFilter(PropertySource, Auditable, AuditTrail)}
   */
  @Deprecated(since = "2021-06-11")
  @Override
  public PieceFilter getFilter(PropertySource ps) {
    return getFilter(ps, null, null);
  }

  @Override
  public PieceFilter getFilter(PropertySource ps, final Auditable owner, final AuditTrail audit) {
    /*
     * If this expression contains old-style $....$ variables, then we need
     * to evaluate these first on the source piece and return a filter using
     * the updated expression.
     */
    if (isDynamic()) {
      // Strip the Beanshell braces so the expression just looks like a
      // string and evaluate the $...$ variables
      final String s = new FormattedString(strip(getExpression())).getText(ps, owner, audit);

      // Turn the result back into a Beanshell expression and create a filter.
      return Expression.createExpression("{" + s + "}").getFilter(owner, audit);
    }

    // Non dynamic, return a standard filter based on the existing expression.
    return new BshFilter(owner, audit);
  }

  public class BshFilter implements PieceFilter {
    private final Auditable owningAuditable;
    private final AuditTrail auditTrail;

    public BshFilter(Auditable owner, AuditTrail auditTrail) {
      this.owningAuditable = owner;
      this.auditTrail = auditTrail;
    }

    @Deprecated(since = "2021-06-11")
    @Override
    public boolean accept(GamePiece piece) {
      return accept(piece, owningAuditable, auditTrail);
    }

    @Override
    public boolean accept(GamePiece piece, Auditable owner, String fieldKey) {
      return accept(piece, owner, AuditTrail.create(owner, getExpression(), Resources.getString(fieldKey)));
    }

    @Override
    public boolean accept(GamePiece piece, Auditable owner, AuditTrail audit) {
      return BeanShell.TRUE.equals(tryEvaluate(piece, owner, audit));
    }

  }
  /**
   * Convert a Property name to its BeanShell equivalent.
   *
   * @param prop Property name
   * @return beanshell equivalent
   */
  public static String convertProperty(String prop) {
    // Null Expression
    if (prop == null || prop.length() == 0) {
      return "";
    }

    // Already a bsh expression?
    if (isBeanShellExpression(prop)) {
      return strip(prop);
    }

    // Check it follows Java variable rules
    boolean ok = Character.isJavaIdentifierStart(prop.charAt(0));
    if (ok) {
      for (int i = 1; i < prop.length() && ok; i++) {
        ok = Character.isJavaIdentifierPart(prop.charAt(i));
      }
    }

    // If not a Java variable, wrap it in GetProperty()
    return ok ? prop : "GetProperty(\"" + prop + "\")"; // NON-NLS
  }

  public static boolean isBeanShellExpression(String expr) {
    return expr.startsWith("{") && expr.endsWith("}");
  }

  public static boolean isJavaIdentifier(String s) {
    if (s == null || s.length() == 0) {
      return false;
    }

    if (!Character.isJavaIdentifierStart(s.charAt(0))) {
      return false;
    }

    for (int i = 1; i < s.length(); ++i) {
      if (!Character.isJavaIdentifierPart(s.charAt(i))) {
        return false;
      }
    }

    return true;
  }

  /**
   * Create a BeanShellExpression.
   *
   * The expression may or may not be surrounded by {}.
   *
   * Create null, integer and simple Expressions as their basic type to
   * ensure efficient evaluation.
   */
  public static Expression createExpression(String s) {
    return createExpression(s, false);
  }

  /**
   *
   * @param s String to convert to a Beanshell expressions
   * @param dontCreateStringExpressions If True, then convert quoted string to Beanshell Expressions, not String Expressions
   * @return Expression
   */
  public static Expression createExpression(String s, boolean dontCreateStringExpressions) {
    final String expr = strip(s);
    if (expr.isBlank()) {
      return NullExpression.instance();
    }

    try {
      return IntExpression.instance(Integer.parseInt(expr));
    }
    catch (NumberFormatException e) {
      // Not an error
    }

    // Return a single String as a string without quotes
    if (!dontCreateStringExpressions) {
      if (expr.length() > 1 && expr.startsWith("\"") && expr.endsWith("\"")
        && expr.indexOf('"', 1) == expr.length() - 1) {
        return StringExpression.instance(expr.substring(1, expr.length() - 1));
      }
    }

    // Return a generalised Beanshell expression
    return instance(expr);
  }

  public static Expression instance(String s) {
    return CACHE.computeIfAbsent(Pair.of(s, BeanShellExpression.class), k -> new BeanShellExpression(s));
  }
}
