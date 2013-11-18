/*
 * $Id: PropertyNameExpressionBuilder.java 7725 2011-07-31 18:51:43Z uckelman $
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
package VASSAL.script.expression;

import javax.swing.JDialog;

import VASSAL.configure.Configurer;
import VASSAL.counters.EditablePiece;

/**
 * A Builder for a field that can contain a property Name or a beanshell expression.
 * @author Brent
 *
 */
public class PropertyNameExpressionBuilder extends ExpressionBuilder {
  private static final long serialVersionUID = 1L;

  public PropertyNameExpressionBuilder(Configurer c, JDialog parent) {
    super(c, parent);
  }

  public PropertyNameExpressionBuilder(Configurer c, JDialog parent, EditablePiece piece) {
    super(c, parent, piece);
    expression.setValue((BeanShellExpression.convertProperty(target.getValueString())));
  }

  /**
   * Convert a property name to an equivalent Beanshell expression
   */
  public String convert(String s) {
    return "{" + s + "}";
  }

  /**
   * Save entered expression to the target.
   * If a single property name has been entered, then return it as a simple name, not as an expression
   */
  public void save() {
    final String expr = expression.getValueString().trim();
    if (BeanShellExpression.isJavaIdentifier(expr)) {
      target.setValue(expr);
      dispose();
      return;
    }

    if (expr.startsWith("GetProperty(\"") && expr.endsWith("\")") &&
        (expr.length() - expr.replaceAll("\"", "").length()) == 2) {
      target.setValue(expr.substring(13, expr.length()-2));
      dispose();
      return;
    }

    super.save();

  }

}