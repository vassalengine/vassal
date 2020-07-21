/*
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
package VASSAL.tools;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JWindow;

import VASSAL.tools.swing.SwingUtils;

/**
 * Displays an image centered on the screen
 */
public class SplashScreen extends JWindow {
  private static final long serialVersionUID = 1L;

  private static List<SplashScreen> instances = new ArrayList<>();

  public SplashScreen(Image im) {
    instances.add(this);
    add(new JLabel(new ImageIcon(im)));
    pack();
    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    setLocation(d.width / 2 - getSize().width / 2,
                d.height / 2 - getSize().height / 2);
    addMouseListener(new MouseAdapter() {
      @Override
      public void mouseReleased(MouseEvent e) {
        if (SwingUtils.isLeftMouseButton(e)) {
          setVisible(false);
        }
      }
    });
  }

  @Override
  public void setVisible(boolean vis) {
    super.setVisible(vis);
    if (vis) {
      toFront();
    }
  }

  public static void sendAllToBack() {
    for (SplashScreen s : instances) {
      s.toBack();
    }
  }

  public static void disposeAll() {
    for (SplashScreen s : instances) {
      s.dispose();
    }
  }
}
