package VASSAL.i18n;

import java.awt.Component;

import javax.swing.JOptionPane;

/**
 * Internationalized version of JOptionPanne
 */
public class VassalOptionPane extends JOptionPane {

  public static int showConfirmDialog(Component parentComponent, Object message, String title,
      int optionType) {
    Object[] options;

    if (optionType == JOptionPane.YES_OPTION) {
      options = new Object[] {Resources.getString(Resources.YES)};
    }
    else if (optionType == JOptionPane.YES_NO_OPTION) {
      options = new Object[] {Resources.getString(Resources.YES), Resources.getString(Resources.NO)};
    }
    else if (optionType == JOptionPane.YES_NO_CANCEL_OPTION) {
      options = new Object[] {Resources.getString(Resources.YES), Resources.getString(Resources.NO),
          Resources.getString(Resources.CANCEL)};
    }
    else {
      return JOptionPane.showConfirmDialog(parentComponent, message, title, optionType);
    }
    return JOptionPane.showOptionDialog(parentComponent, message, title, JOptionPane.YES_NO_OPTION,
        JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
  }
  
  public static void showMessageDialog(Component parentComponent, Object message, String title,
      int optionType) {
    
    Object[] options = {Resources.getString(Resources.OK)};
    JOptionPane.showOptionDialog(parentComponent, message, title, JOptionPane.YES_OPTION,
        optionType, null, options, options[0]);
  }
}