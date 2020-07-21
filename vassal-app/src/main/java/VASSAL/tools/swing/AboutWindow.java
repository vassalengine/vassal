/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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

package VASSAL.tools.swing;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JWindow;

import VASSAL.tools.swing.SwingUtils;

/**
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class AboutWindow extends JWindow {
  private static final long serialVersionUID = 1L;

  public AboutWindow(Window w, BufferedImage img, String text) {
    super(w);

    getContentPane().setBackground(Color.black);
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

    final JLabel l1 = new JLabel(new ImageIcon(img));
    l1.setAlignmentX(0.5F);
    add(l1);

    final JLabel l2 = new JLabel(text);
    l2.setBackground(Color.blue);
    l2.setForeground(Color.white);
    l2.setHorizontalAlignment(JLabel.CENTER);
    l2.setAlignmentX(0.5F);
    add(l2);

    pack();

    final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    setLocation(d.width / 2 - getWidth() / 2,
                d.height / 2 -getHeight() / 2);

    addMouseListener(new MouseAdapter() {
      @Override
      public void mouseReleased(MouseEvent e) {
        if (SwingUtils.isLeftMouseButton(e)) {
          setVisible(false);
          dispose();
        }
      }
    });
  }
}
