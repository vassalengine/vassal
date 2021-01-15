/*
 *
 * Copyright (c) 2007-2009 by Brent Easton
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
package VASSAL.configure;

import java.awt.event.ActionEvent;

import javax.swing.JDialog;

import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.script.expression.PropertyExpressionBuilder;

/**
 * A Configurer for Java Expressions
 */
public class PropertyExpressionConfigurer extends FormattedExpressionConfigurer {


  public PropertyExpressionConfigurer(String key, String name) {
    this(key, name, "");
  }

  public PropertyExpressionConfigurer(String key, String name, String val) {
    this(key, name, val, null);
  }

  public PropertyExpressionConfigurer(PropertyExpression val) {
    this(null, "", val);
  }

  public PropertyExpressionConfigurer(String key, String name, PropertyExpression val) {
    this(key, name, val.getExpression());
  }

  public PropertyExpressionConfigurer(String key, String name, PropertyExpression val, GamePiece piece) {
    this(key, name, val.getExpression(), piece);
  }

  public PropertyExpressionConfigurer(PropertyExpression val, GamePiece piece) {
    this(null, "", val.getExpression(), piece);
  }

  public PropertyExpressionConfigurer(String key, String name, String val, GamePiece piece) {
    super(key, name, val, piece);
    setHintKey("Editor.property_match_hint");
  }

  @Override
  protected ExpressionButton buildButton() {
    return new PropertyExpressionButton(this, nameField.getPreferredSize().height, pieceTarget);
  }

  public static class PropertyExpressionButton extends ExpressionButton {
    private static final long serialVersionUID = 1L;

    public PropertyExpressionButton(Configurer config, int size,
        EditablePiece piece) {
      super(config, size, piece);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      new PropertyExpressionBuilder(config, (JDialog) getTopLevelAncestor(), piece).setVisible(true);
    }


  }

}
