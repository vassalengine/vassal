/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney
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
 * Prompts for an integer value
 * @author rkinney
 *
 */
public class NumericPropertyPrompt extends PropertyPrompt {
  private int min;
  private int max;
  private Component dialogParent;

  public NumericPropertyPrompt(Component dialogParent, String prompt, int minValue, int maxValue) {
    super(null, prompt);
    min = minValue;
    max = maxValue;
    this.dialogParent = dialogParent;
  }

  public String getNewValue(String oldValue) {
    String s = null;
    do {
      s = (String) JOptionPane.showInputDialog(dialogParent, promptText, null, JOptionPane.QUESTION_MESSAGE, null, null, oldValue);
    } while (s != null && !isValidValue(s));
    return s;
  }

  private boolean isValidValue(String s) {
    try {
      final int value = Integer.parseInt(s);
      return value <= max && value >= min;
    }
    catch (NumberFormatException e) {
      return false;
    }
  }
}
