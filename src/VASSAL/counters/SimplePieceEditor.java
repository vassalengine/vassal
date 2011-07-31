/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.counters;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class SimplePieceEditor implements PieceEditor {
  JTextField typeField, stateField;
  JPanel panel;

  public SimplePieceEditor(GamePiece p) {
    String type = null;
    if (p instanceof Decorator) {
      type = ((Decorator) p).myGetType();
    }
    else {
      type = p.getType();
    }
    typeField = new JTextField(type);
    typeField.setMaximumSize(new java.awt.Dimension(typeField.getMaximumSize().width, typeField.getPreferredSize().height));

    String state = null;
    if (p instanceof Decorator) {
      state = ((Decorator) p).myGetState();
    }
    else {
      state = p.getState();
    }
    stateField = new JTextField(state);
    stateField.setMaximumSize(new java.awt.Dimension(stateField.getMaximumSize().width, stateField.getPreferredSize().height));

    panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    Box b = Box.createHorizontalBox();
    b.add(new JLabel("Type: "));
    b.add(typeField);
    panel.add(b);

    b = Box.createHorizontalBox();
    b.add(new JLabel("State: "));
    b.add(stateField);
    panel.add(b);

  }

  public String getType() {
    return typeField.getText();
  }

  public String getState() {
    return stateField.getText();
  }

  public java.awt.Component getControls() {
    return panel;
  }

}
