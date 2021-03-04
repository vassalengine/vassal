/*
 *
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

import VASSAL.build.BadDataReport;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

/**
 * An abstract class representing an expression that can be evaluated.
 *
 * Subclasses implement specific types of expression and the way they are
 * evaluated.
 *
 */
public abstract class BaseExpression extends Expression {
  private String expression;

  @Override
  public void setExpression(String s) {
    expression = s;
  }

  @Override
  public String getExpression() {
    return expression;
  }

  private void handleError(ExpressionException e) {
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
  @Override
  public String tryEvaluate(PropertySource ps) {
    try {
      return evaluate(ps);
    }
    catch (ExpressionException e) {
      handleError(e);
      return null;
    }
  }

  /**
   * Evaluate an expression with data warning support built in
   * @return evaluated String
   */
  @Override
  public String tryEvaluate() {
    try {
      return evaluate();
    }
    catch (ExpressionException e) {
      handleError(e);
      return null;
    }
  }

  /**
   * Evaluate an expression with data warning support built in
   * @param ps Property Source providing property values
   * @param localized Localize property calls?
   * @return evaluated String
   */
  @Override
  public String tryEvaluate(PropertySource ps, boolean localized) {
    try {
      return evaluate(ps, localized);
    }
    catch (ExpressionException e) {
      handleError(e);
      return null;
    }
  }

  /**
   * Evaluate an expression with data warning support built in
   * @param ps Property Source providing property values
   * @param properties Default property values
   * @param localized Localize property calls?
   * @return evaluated String
   */
  @Override
  public String tryEvaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    try {
      return evaluate(ps, properties, localized);
    }
    catch (ExpressionException e) {
      handleError(e);
      return null;
    }
  }

  @Override
  public int hashCode() {
    return expression == null ? 0 : expression.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;

    if (obj == null || getClass() != obj.getClass()) return false;

    final BaseExpression other = (BaseExpression) obj;
    return Objects.equals(expression, other.expression);
  }
}
