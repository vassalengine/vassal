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

import javax.swing.JOptionPane;

/**
 * Prompts user to select from a list
 * @author rkinney
 *
 */
public class EnumeratedPropertyPrompt extends PropertyPrompt {
  protected String[] validValues;
  protected DialogParent dialogParent;

  public EnumeratedPropertyPrompt(DialogParent dialogParent, String prompt, String[] validValues) {
    super(null, prompt);
    this.validValues = validValues;
    this.dialogParent = dialogParent;
  }

  public String getNewValue(String oldValue) {
    final String newValue = (String) JOptionPane.showInputDialog(dialogParent.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null,validValues,oldValue);
    return newValue == null ? oldValue : newValue;
  }

  public String[] getValidValues() {
    return validValues;
  }




}
