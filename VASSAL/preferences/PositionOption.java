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
package VASSAL.preferences;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.StringTokenizer;

public class PositionOption extends VASSAL.configure.Configurer
  implements ComponentListener {
  public static String key = "BoundsOf";
  private static Point initialPos = new Point(0, 0);

  protected Window theFrame;
  protected Rectangle bounds;
  protected Rectangle defaultValue;

  public PositionOption(String key, Window f, Rectangle defaultValue) {
    super(key, null, defaultValue);
    adjustInitialOffset();
    theFrame = f;
    theFrame.pack();
    setFrameBounds();
    this.defaultValue = defaultValue;
    theFrame.addComponentListener(this);
  }

  private static void adjustInitialOffset() {
    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    if (initialPos.x >= d.width-30
      || initialPos.y >= d.height-30) {
      initialPos.move(0,0);
    }
    else {
      initialPos.translate(30, 30);
    }
  }

  public PositionOption(String key, Window f) {
    this(key,f,new Rectangle(initialPos,new Dimension(0,0)));
  }

  public Object getValue() {
    return bounds;
  }

  public void setValue(Object o) {
    if (o instanceof Rectangle) {
      bounds = new Rectangle((Rectangle)o);
      if (theFrame != null) {
        setFrameBounds();
      }
    }
    super.setValue(o);
  }

  public java.awt.Component getControls() {
    return null;
  }

  public void setValue(String in) {
    StringTokenizer st = new StringTokenizer
      (in, ",");
    try {
      setValue(new Rectangle(Integer.parseInt(st.nextToken()),
                             Integer.parseInt(st.nextToken()),
                             Integer.parseInt(st.nextToken()),
                             Integer.parseInt(st.nextToken())));
    }
    catch (NumberFormatException e) {
    }
  }

  public String getValueString() {
    return bounds.x + "," + bounds.y + "," +
      bounds.width + "," + bounds.height;
  }

  private boolean isOnScreen(Point p) {
    return
      p.x < Toolkit.getDefaultToolkit().getScreenSize().width &&
      p.y < Toolkit.getDefaultToolkit().getScreenSize().height;
  }

  public void componentMoved(ComponentEvent e) {
    if (theFrame.isShowing()) {
      Point p = theFrame.getLocationOnScreen();
      if (isOnScreen(p)) {
        bounds.setLocation(p);
      }
    }
  }

  public void componentResized(ComponentEvent e) {
    if (theFrame.isShowing()) {
      bounds.setSize(theFrame.getSize());
    }
  }

  public void componentShown(ComponentEvent e) {
  }

  public void componentHidden(ComponentEvent e) {
  }

  protected void setFrameBounds() {
    Dimension maxSize = Toolkit.getDefaultToolkit().getScreenSize();
    if (bounds.width != 0 && bounds.height != 0) {
      theFrame.setSize(new Dimension(Math.abs(bounds.width),Math.abs(bounds.height)));
    }
    theFrame.setLocation(bounds.getLocation());
    if (theFrame.getLocation().x + theFrame.getSize().width
      > maxSize.width) {
      theFrame.setSize(maxSize.width - theFrame.getLocation().x,
                       theFrame.getSize().height);
    }
    if (theFrame.getLocation().y + theFrame.getSize().height
      > maxSize.height) {
      theFrame.setSize(theFrame.getSize().width,
                       maxSize.height - theFrame.getLocation().y);
    }
  }
}
