package VASSAL.chat;

import javax.swing.*;

/**
 * A window that displays information on a {@link VASSAL.chat.SimplePlayer}
 */
public class PlayerInfoWindow extends JDialog {
  public PlayerInfoWindow(java.awt.Frame f, SimplePlayer p) {
    super(f, p.getName());
    getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    Box b = Box.createHorizontalBox();
    JTextField tf = new JTextField(p.getName().length());
    tf.setText(p.getName());
    tf.setEditable(false);
    tf.setMaximumSize(new java.awt.Dimension(tf.getMaximumSize().width,
                                             tf.getPreferredSize().height));
    b.add(new JLabel("Real Name: "));
    b.add(tf);
    getContentPane().add(b);

    JCheckBox box = new JCheckBox("Looking for a game");
    box.setSelected(((SimpleStatus)p.getStatus()).isLooking());
    box.setEnabled(false);
    getContentPane().add(box);

    box = new JCheckBox("Away from keyboard");
    box.setSelected(((SimpleStatus)p.getStatus()).isAway());
    box.setEnabled(false);
    getContentPane().add(box);

    getContentPane().add(new JLabel("Personal Info"));
    JTextArea ta = new JTextArea();
    ta.setText(((SimpleStatus)p.getStatus()).getProfile());
    ta.setEditable(false);
    getContentPane().add(new JScrollPane(ta));

    pack();
  }
}
