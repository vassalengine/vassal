/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.counters;

import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class KeySpecifier extends JPanel implements KeyListener {
  private static final long serialVersionUID = 1L;

  private JTextField tf = new JTextField(3);
  private String key = " ";

  public KeySpecifier(char c) {
    setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
    key = String.valueOf(c);

    add(new JLabel("CTRL-"));
    tf.setMaximumSize(tf.getPreferredSize());
    tf.setMinimumSize(tf.getPreferredSize());
    tf.setText(c == 0 ? "" : key);
    add(tf);
  }

  public void addFocusListener(FocusListener l) {
    if (tf != null)
      tf.addFocusListener(l);
  }

  public void addActionListener(ActionListener l) {
    tf.addActionListener(l);
  }

  public void keyPressed(KeyEvent e) {
  }

  public void keyTyped(KeyEvent e) {
  }

  public void keyReleased(KeyEvent e) {
    if (e.getKeyCode() == KeyEvent.VK_BACK_SPACE
      || e.getKeyCode() == KeyEvent.VK_DELETE) {
      key = key.length() < 1 ? "" : key.substring(0, key.length() - 1);
    }
    else if (e.getKeyChar() != KeyEvent.CHAR_UNDEFINED) {
      //    key = tf.getText().trim().length() == 0 ?
      //        " " : key+(char)e.getKeyCode();
      key += (char) e.getKeyCode();
    }
  }

  public String getKey() {
    String s = tf.getText().trim();
    tf.setText(s.toUpperCase());
    return s.toUpperCase();
  }

  public void setKey(char c) {
    if (Character.isDefined(c)) {
      setKey(String.valueOf(c));
    }
    else {
      setKey("");
    }
  }

  public void setKey(String s) {
    tf.setText(s);
  }

  public void setEnabled(boolean enable) {
    tf.setEnabled(enable);
    super.setEnabled(enable);
  }
}


