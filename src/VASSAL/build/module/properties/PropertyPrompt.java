/*
 * $Id$
 *
 * Copyright (c) 2006 by Rodney Kinney
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
package VASSAL.build.module.properties;

import java.awt.Component;

import javax.swing.JOptionPane;

/**
 * Prompts user for a new value
 *
 * @author rkinney
 *
 */
public class PropertyPrompt implements PropertyChanger {
  protected String promptText;
  protected Constraints constraints;

  public PropertyPrompt(Constraints constraints, String prompt) {
    this.constraints = constraints;
    this.promptText = prompt;
  }

  public String getNewValue(String oldValue) {
    String newValue = null;
    if (constraints != null && constraints.isNumeric()) {
      newValue = new NumericPropertyPrompt(constraints.getComponent(), promptText, constraints.getMinimumValue(), constraints.getMaximumValue()).getNewValue(oldValue);
    }
    else {
      newValue = (String) JOptionPane.showInputDialog(constraints.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null, null, oldValue);
    }

    return newValue == null ? oldValue : newValue;
  }

  public String getPrompt() {
    return promptText;
  }

  public static interface DialogParent {
    Component getComponent();
  }

  public static interface Constraints extends DialogParent {
    boolean isNumeric();

    int getMaximumValue();

    int getMinimumValue();

    PropertySource getPropertySource();
  }

}
