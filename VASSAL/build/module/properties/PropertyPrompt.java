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
    if (constraints != null && constraints.isNumeric()) {
      return new NumericPropertyPrompt(constraints.getComponent(), promptText, constraints.getMinimumValue(), constraints.getMaximumValue()).getNewValue(oldValue);
    }
    return (String) JOptionPane.showInputDialog(constraints.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null, null, oldValue);
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
  }

}
