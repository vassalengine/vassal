/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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

package VASSAL.launch;

import java.awt.Toolkit;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;

/**
 * The window with controls for VASSAL prior to loading a particular module
 * @author rodneykinney
 *
 */
public class ConsoleWindow {
  protected JFrame frame;
  
  public ConsoleWindow() {
    initComponents();
  }

  protected void initComponents() {
    frame = new JFrame("VASSAL");
    frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(),BoxLayout.Y_AXIS));
    JLabel label = new JLabel(new ImageIcon(Toolkit.getDefaultToolkit().getImage(ConsoleWindow.class.getResource("/images/Splash.gif"))));
    label.setAlignmentX(Box.CENTER_ALIGNMENT);
    frame.getContentPane().add(label);
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
    frame.addWindowListener(new java.awt.event.WindowAdapter() {
      public void windowClosing(java.awt.event.WindowEvent evt) {
        System.exit(0);
      }
    }
    );

   }
  
  public void setControls(JComponent c) {
    while (frame.getContentPane().getComponentCount() > 1) {
      frame.getContentPane().remove(frame.getContentPane().getComponentCount()-1);
    }
    frame.getContentPane().add(c);
    c.setAlignmentX(Box.CENTER_ALIGNMENT);
    frame.pack();
    frame.setLocationRelativeTo(null);
  }

  public JFrame getFrame() {
    return frame;
  }
}
