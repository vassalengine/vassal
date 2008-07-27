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
      int value = Integer.parseInt(s.toString());
      return value <= max && value >= min;
    }
    catch (NumberFormatException e) {
      return false;
    }
  }
}
