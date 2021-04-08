/*
 * Copyright (c) 2009 Brent Easton
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

import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.tuple.Pair;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.concurrent.ConcurrentSoftHashMap;

/**
 * An abstract class representing an expression that can be evaluated.
 *
 * Subclasses implement specific types of expression and the way they are
 * evaluated.
 *
 */
public class Expression {
  protected static final Map<Pair<Object, Class<? extends Expression>>, Expression> CACHE = new ConcurrentSoftHashMap<>();

  private final String expr;

  protected Expression(String s) {
    expr = s;
  }

  public String getExpression() {
    return expr;
  }

  public void setExpression(String s) {
    throw new UnsupportedOperationException();
  }

  protected void reset() {
  }

  /**
   * Each subclass must implement evaluate() to evaluate itself
   *
   * @param ps
   *          Property Source providing property values
   * @param properties
   *          default property values
   * @param localized
   *          localize property calls?
   * @return evaluated String.
   */
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized) throws ExpressionException {
    return expr;
  }

  public String evaluate() throws ExpressionException {
    return evaluate(null, null, false);
  }

  public String evaluate(PropertySource ps) throws ExpressionException {
    return evaluate(ps, null, false);
  }

  public String evaluate(PropertySource ps, boolean localized) throws ExpressionException {
    return evaluate(ps, null, localized);
  }

  public String evaluate(boolean localized) throws ExpressionException {
    return evaluate(null, null, localized);
  }

  protected void handleError(ExpressionException e) {
    ErrorDialog.dataWarning(new BadDataReport(
      Resources.getString("Error.expression_error"),
      "Expression=" + getExpression() + ", Error=" + e.getError(), //NON-NLS
      e
    ));
  }

  /**
   * Evaluate an expression with data warning support built in
   * @param ps Property Source providing property values
   * @return evaluated String
   */
  public String tryEvaluate(PropertySource ps) {
    return tryEvaluate(ps, null, false);
  }

  /**
   * Evaluate an expression with data warning support built in
   * @return evaluated String
   */
  public String tryEvaluate() {
    return tryEvaluate(null, null, false);
  }

  /**
   * Evaluate an expression with data warning support built in
   * @param ps Property Source providing property values
   * @param localized Localize property calls?
   * @return evaluated String
   */
  public String tryEvaluate(PropertySource ps, boolean localized) {
    return tryEvaluate(ps, null, localized);
  }

  /**
   * Evaluate an expression with data warning support built in
   * @param localized Localize property calls?
   * @return evaluated String
   */
  public String tryEvaluate(boolean localized) {
    return tryEvaluate(null, null, localized);
  }

  /**
   * Evaluate an expression with data warning support built in
   * @param ps Property Source providing property values
   * @param properties Default property values
   * @param localized Localize property calls?
   * @return evaluated String
   */
  public String tryEvaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    try {
      return evaluate(ps, properties, localized);
    }
    catch (ExpressionException e) {
      handleError(e);
      return ""; // Return something that won't cause downstream code to throw exceptions, since we're basically dealing with "Bad Module Data".
    }
  }

  /**
   * Return a PieceFilter using the expression.
   *
   * @param ps PropertySource to use as source of filter
   * @return Created PieceFilter
   */
  public PieceFilter getFilter(PropertySource ps) {
    return piece -> true;
  }

  public PieceFilter getFilter() {
    return getFilter(GameModule.getGameModule());
  }

  /**
   * Output a BeanShell equivalent of this expression.
   *
   * @return BeanShell equivalent
   */
  public String toBeanShellString() {
    return expr;
  }

  /**
   * Factory method to create an appropriate expression based on the supplied
   * String. The majority of expressions in a module are going to be blank,
   * integers or simple strings, so return optimised Expression subclasses for
   * these types.
   */
  public static Expression createExpression(String s) {
    // A null expression?
    if (s == null || s.isBlank()) {
      return NullExpression.instance();
    }

    final String t = s.trim();

    // BeanShell expression enclosed by braces?
    if (t.startsWith("{") && t.endsWith("}")) {
      return BeanShellExpression.createExpression(s);
    }

    // A simple integer expression
    try {
      return IntExpression.instance(Integer.parseInt(t));
    }
    catch (NumberFormatException e) {
      // Not an error
    }

    // An old-style Formatted String?
    if (t.indexOf('$') >= 0) {
      return FormattedStringExpression.instance(t);
    }

    // Must be a plain String
    return StringExpression.instance(s);
  }

  @Override
  public int hashCode() {
    return expr == null ? 0 : expr.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;

    if (obj == null || getClass() != obj.getClass()) return false;

    final Expression other = (Expression) obj;
    return Objects.equals(expr, other.expr);
  }

  /**
   * Factory method to create a new Property Match Expression.
   *
   * @param s Expression string
   * @return Generated Expression
   */
  public static Expression createPropertyExpression(String s) {
    // A null expression?
    if (s == null || s.isBlank()) {
      return NullExpression.instance();
    }

    final String t = s.trim();

    // BeanShell expression?
    if (t.startsWith("{") && t.endsWith("}")) {
      return BeanShellExpression.instance(t.substring(1, t.length() - 1));
    }

    // An old-style Property Match String
    return PropertyMatchExpression.instance(t);
  }

  /**
   * Factory method to create a Beanshell expression of a value that
   * is known to be a property name.
   * Used to convert values such as the Follow property field in Embellishment
   *
   */
  public static Expression createSimplePropertyExpression(String s) {
    // A null expression?
    if (s == null || s.isBlank()) {
      return NullExpression.instance();
    }

    final String t = s.trim();

    // BeanShell expression?
    if (t.startsWith("{") && t.endsWith("}")) {
      return BeanShellExpression.instance(t.substring(1, t.length() - 1));
    }

    return SinglePropertyExpression.instance(t);
  }

  public static void resetCachedExpressions() {
    CACHE.values().forEach(e -> {
      if (e != null) {
        e.reset();
      }
    });
  }
}
