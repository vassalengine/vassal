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

import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;

import javax.swing.JOptionPane;
import java.util.ArrayList;
import java.util.List;

/**
 * Prompts user to select from a list
 * @author rkinney
 *
 */
public class EnumeratedPropertyPrompt extends PropertyPrompt {
  protected String[] validValues;
  protected Expression[] valueExpressions;
  protected DialogParent dialogParent;

  public EnumeratedPropertyPrompt(DialogParent dialogParent, String prompt, String[] validValues, Constraints propertySource) {
    super(propertySource, prompt);
    this.validValues = validValues;
    valueExpressions = new Expression[validValues.length];
    for (int i = 0; i < validValues.length; i++) {
      valueExpressions[i] = Expression.createExpression(validValues[i]);
    }
    this.dialogParent = dialogParent;
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
        final AuditTrail audit = AuditTrail.create(constraints == null ? null : constraints.getPropertySource(), valueExpressions[i].getExpression());
        if (constraints == null) {
          value = valueExpressions[i].evaluate(constraints.getPropertySource(), audit);
        }
        else {
          value = valueExpressions[i].evaluate(constraints.getPropertySource(), constraints.getPropertySource(), audit);
        }
      }
      catch (final ExpressionException e) {
        value = valueExpressions[i].getExpression();
      }
      finalValues[i] = value;
    }

    final List<String> nonBlankValues = new ArrayList<>();
    for (final String value : finalValues) {
      if (value.isEmpty()) continue;
      nonBlankValues.add(value);
    }

    if (nonBlankValues.isEmpty()) {
      return oldValue;
    }

    final String newValue = (String) JOptionPane.showInputDialog(
      dialogParent.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null, nonBlankValues.toArray(new String[0]), oldValue);
    return newValue == null ? oldValue : newValue;
  }

  public String[] getValidValues() {
    return validValues;
  }

}
