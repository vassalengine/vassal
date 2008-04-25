
package VASSAL.tools;

import java.awt.Frame;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

// FIXME: switch to these once we move to Java 1.6
//import javax.swing.GroupLayout;
//import javax.swing.LayoutStyle;
import org.jdesktop.layout.GroupLayout;
import org.jdesktop.layout.LayoutStyle;

import VASSAL.i18n.Resources;

public class ProgressDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private final JLabel label;
  private final JProgressBar progbar;
  private final JButton cancel;

  public ProgressDialog(Frame parent, String title, String text) {
    super(parent, title, true);

    label = new JLabel(text);

    progbar = new JProgressBar(0, 100);
    progbar.setStringPainted(true);
    progbar.setValue(0);
  
    cancel = new JButton(Resources.getString("General.cancel"));
    cancel.setSelected(true);

    final JPanel panel = new JPanel();

    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.LEADING, true)
        .add(label)
        .add(progbar)
        .add(layout.createSequentialGroup()
          .add(0, 0, Integer.MAX_VALUE)
          .add(cancel)
          .add(0, 0, Integer.MAX_VALUE)));
          
    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .add(label)
        .add(progbar)
        .addPreferredGap(LayoutStyle.UNRELATED,
                         GroupLayout.DEFAULT_SIZE, Integer.MAX_VALUE)
        .add(cancel));

    add(panel);

    pack();
    setMinimumSize(getSize());

/*
    final Box box = Box.createVerticalBox();
    box.setBorder(new EmptyBorder(12, 12, 11, 11));
    add(box);

    final Box lb = Box.createHorizontalBox();
    label = new JLabel(text);
    lb.add(label);
    lb.add(Box.createHorizontalGlue());
    box.add(lb);

    box.add(Box.createVerticalStrut(11));

    progbar = new JProgressBar(0, 100);
    progbar.setStringPainted(true);
    progbar.setValue(0);
    box.add(progbar);
  
    box.add(Box.createVerticalStrut(17));

    final Box bb = Box.createHorizontalBox();
    bb.add(Box.createHorizontalGlue());
    cancel = new JButton(Resources.getString("General.cancel"));
    cancel.setSelected(true);
    bb.add(cancel);
    bb.add(Box.createHorizontalGlue());
    box.add(bb);
*/
  }

  public void setLabel(String text) {
    label.setText(text);
  }

  public void setIndeterminate(boolean indet) {
    progbar.setIndeterminate(indet);
  }

  public void setProgress(int percent) {
    progbar.setValue(percent);
  }

  public void setStringPainted(boolean painted) {
    progbar.setStringPainted(painted);
  }

  public void addActionListener(ActionListener l) {
    cancel.addActionListener(l);
  }
}
