/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package VASSAL.build.module.gamepieceimage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Window;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;


public class Visualizer extends JPanel {
  private static final long serialVersionUID = 1L;

  protected static final int OFFSET = 20;
  protected Visualizable observer;
  protected JPanel visPanel;

  public Visualizer() {

  }

  public Visualizer(Visualizable obs) {

    observer = obs;
    setBorder(BorderFactory.createLineBorder(Color.black));

    visPanel = new JPanel() {
      private static final long serialVersionUID = 1L;

      public void paint(Graphics g) {
        g.clearRect(0, 0, observer.getVisualizerWidth(), observer.getVisualizerHeight());
        g.drawImage(observer.getVisualizerImage(), 0, 0, this);
      }
    };

    add(visPanel, BorderLayout.CENTER);
  }

  public void setObserver(Visualizable obs) {
    observer = obs;
    rebuild();
    refresh();
  }

  public void rebuild() {
    if (observer != null) {
      observer.rebuildVisualizerImage();
      refresh();
    }
  }

  public void refresh() {
    if (observer != null) {
      int width = observer.getVisualizerWidth();
      int height = observer.getVisualizerHeight();

      visPanel.setSize(width, height);
      visPanel.setPreferredSize(new Dimension(width, height));

      setSize(width + OFFSET, height + OFFSET);
      setPreferredSize(new Dimension(width + OFFSET, height + OFFSET));

      Window w = SwingUtilities.getWindowAncestor(this);
      if (w != null) {
        w.pack();
      }

      repaint();
    }
  }
}
