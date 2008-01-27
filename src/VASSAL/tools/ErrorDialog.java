package VASSAL.tools;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashSet;
import java.util.Set;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import VASSAL.build.GameModule;

public class ErrorDialog {
//  static boolean disabled = false;

  private static final Set<Class<? extends Exception>> disabled =
    new HashSet<Class<? extends Exception>>();

  public static void raise(Exception e, String msg) {
    if (disabled.contains(e.getClass())) return;

    final JButton okButton = new JButton("Ok");
//    final JButton disableButton = new JButton("Don't show this dialog again");

    final JCheckBox disableCheck =
      new JCheckBox("Do not show this dialog again");

    final JLabel msgLabel = new JLabel(msg);

    final Box msgBox = new Box(BoxLayout.Y_AXIS);
    msgBox.add(msgLabel);
    msgBox.add(Box.createVerticalStrut(
      msgLabel.getFontMetrics(msgLabel.getFont()).getHeight()));
    msgBox.add(disableCheck);
    
    final JOptionPane pane = new JOptionPane(
//      msg,
      msgBox,
      JOptionPane.DEFAULT_OPTION,
      JOptionPane.ERROR_MESSAGE,
      UIManager.getIcon("OptionPane.errorIcon"),
//      new Object[]{okButton, disableButton},
//      new Object[]{disableCheck, okButton},
      new Object[]{okButton},
      okButton
    );

    final Component comp = GameModule.getGameModule() == null
                         ? null : GameModule.getGameModule().getFrame();

    final JDialog dialog = pane.createDialog(comp, "Error");
    
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        pane.setValue(!disableCheck.isSelected());
        dialog.dispose();
      }
    });

    dialog.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent evt) {
        pane.setValue(!disableCheck.isSelected());
        dialog.dispose();
      }
    });

    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.pack();

//    disableButton.addActionListener(new ActionListener() {
/*
    disableCheck.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        pane.setValue(Boolean.TRUE);
        dialog.dispose();
      }
    });
*/

    final Class<? extends Exception> exceptionClass = e.getClass();

    final Runnable runnable = new Runnable() {
      public void run() {
        dialog.setVisible(true);
        if (Boolean.FALSE.equals(pane.getValue())) {
          disabled.add(exceptionClass);
        }
      }
    };
    SwingUtilities.invokeLater(runnable);
  }
}
