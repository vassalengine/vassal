/*
 * $Id: PropertyNameExpressionConfigurer.java 7725 2011-07-31 18:51:43Z uckelman $
 *
 * Copyright (c) 2007-2012 by Brent Easton
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
import VASSAL.script.expression.PropertyNameExpressionBuilder;

/**
 * A Configurer for Java Expressions
 */
public class PropertyNameExpressionConfigurer extends FormattedExpressionConfigurer {


  public PropertyNameExpressionConfigurer(String key, String name) {
    this(key, name, "");
  }

  public PropertyNameExpressionConfigurer(String key, String name, String val) {
    this(key, name, val, null);
  }

  public PropertyNameExpressionConfigurer(String key, String name, PropertyExpression val) {
    this(key, name, val.getExpression());
  }

  public PropertyNameExpressionConfigurer(String key, String name, PropertyExpression val, GamePiece piece) {
    this(key, name, val.getExpression(), piece);
  }

  public PropertyNameExpressionConfigurer(String key, String name, String val, GamePiece piece) {
    super(key, name, val, piece);
  }

  protected ExpressionButton buildButton() {
    return new PropertyNameExpressionButton(this, nameField.getPreferredSize().height, pieceTarget);
  }

  public static class PropertyNameExpressionButton extends ExpressionButton {
    private static final long serialVersionUID = 1L;

    public PropertyNameExpressionButton(Configurer config, int size,
        EditablePiece piece) {
      super(config, size, piece);
    }

    public void actionPerformed(ActionEvent e) {
      new PropertyNameExpressionBuilder(config, (JDialog) getTopLevelAncestor(), piece).setVisible(true);
    }


  }

}
