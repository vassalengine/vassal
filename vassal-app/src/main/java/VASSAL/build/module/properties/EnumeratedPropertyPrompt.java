/*
 *
 * Copyright (c) 2000-2011 by Rodney Kinney, Brent Easton
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

import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;

/**
 * Prompts user to select from a list
 * @author rkinney
 *
 */
public class EnumeratedPropertyPrompt extends PropertyPrompt {
  protected String[] validValues;
  protected Expression[] valueExpressions;
  protected DialogParent dialogParent;
  protected Constraints propertySource;

  public EnumeratedPropertyPrompt(DialogParent dialogParent, String prompt, String[] validValues, Constraints propertySource) {
    super(null, prompt);
    this.validValues = validValues;
    valueExpressions = new Expression[validValues.length];
    for (int i = 0; i < validValues.length; i++) {
      valueExpressions[i] = Expression.createExpression(validValues[i]);
    }
    this.dialogParent = dialogParent;
    this.propertySource = propertySource;
  }

  public Expression[] getValueExpressions() {
    return valueExpressions;
  }

  @Override
  public String getNewValue(String oldValue) {
    final String[] finalValues = new String[valueExpressions.length];
    for (int i = 0; i < finalValues.length; i++) {
      String value;
      try {
        final AuditTrail audit = AuditTrail.create(constraints.getPropertySource(), valueExpressions[i].getExpression());
        if (propertySource == null) {
          value = valueExpressions[i].evaluate(propertySource.getPropertySource(), audit);
        }
        else {
          value = valueExpressions[i].evaluate(propertySource.getPropertySource(), propertySource.getPropertySource(), audit);
        }
      }
      catch (final ExpressionException e) {
        value = valueExpressions[i].getExpression();
      }
      finalValues[i] = value;
    }
    final String newValue = (String) JOptionPane.showInputDialog(
      dialogParent.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null, finalValues, oldValue);
    return newValue == null ? oldValue : newValue;
  }

  public String[] getValidValues() {
    return validValues;
  }

}
