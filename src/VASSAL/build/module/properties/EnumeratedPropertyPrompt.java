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
    return (String) JOptionPane.showInputDialog(dialogParent.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null,validValues,oldValue);
  }

  public String[] getValidValues() {
    return validValues;
  }
  
  
  

}
