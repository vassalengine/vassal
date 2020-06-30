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
package VASSAL.preferences;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.StringTokenizer;

import VASSAL.build.BadDataReport;
import VASSAL.tools.ErrorDialog;

public class PositionOption extends VASSAL.configure.Configurer
  implements ComponentListener {
  public static String key = "BoundsOf";
  private static Point initialPos = new Point(0, 0);

  protected Window theFrame;
  protected Rectangle bounds;
  protected Rectangle defaultValue;
  protected Rectangle previousBounds;

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

  @Override
  public Object getValue() {
    return bounds;
  }

  @Override
  public void setValue(Object o) {
    if (o instanceof Rectangle) {
      bounds = new Rectangle((Rectangle)o);
      if (theFrame != null) {
        setFrameBounds();
      }
    }
    super.setValue(o);
  }

  @Override
  public java.awt.Component getControls() {
    return null;
  }

  @Override
  public void setValue(String in) {
    final StringTokenizer st = new StringTokenizer(in, ",");
    try {
      setValue(new Rectangle(Integer.parseInt(st.nextToken()),
                             Integer.parseInt(st.nextToken()),
                             Integer.parseInt(st.nextToken()),
                             Integer.parseInt(st.nextToken())));
    }
    catch (NumberFormatException e) {
      // This can happen if a VisibilityOption has the same name
      // as a PositionOption, either currently, or due to editing.
      // Don't throw a bug, just log it.
      if (in.indexOf('\t') > 0) {
        ErrorDialog.dataError(new BadDataReport("Map or Chart window with same name as piece Palette", getKey(), e));
      }
      else {
        ErrorDialog.bug(e);
      }
    }
  }

  @Override
  public String getValueString() {
    return bounds.x + "," + bounds.y + "," +
      bounds.width + "," + bounds.height;
  }

  private boolean isOnScreen(Point p) {
    return
      p.x < Toolkit.getDefaultToolkit().getScreenSize().width &&
      p.y < Toolkit.getDefaultToolkit().getScreenSize().height;
  }

  @Override
  public void componentMoved(ComponentEvent e) {
    if (theFrame.isShowing()) {
      Point p = theFrame.getLocationOnScreen();
      if (isOnScreen(p)) {
        // Save the previous size in case this is the start of a Maximize
        previousBounds = new Rectangle(bounds);
        bounds.setLocation(p);
      }
    }
  }

  @Override
  public void componentResized(ComponentEvent e) {
    if (theFrame.isShowing()) {
      // A resize when the window is already maximised only happens when
      // a window is first resized. Record the pre-maximised bounds.
      if (theFrame instanceof Frame &&
          previousBounds != null &&
          ((((Frame) theFrame).getExtendedState() & Frame.MAXIMIZED_BOTH) == Frame.MAXIMIZED_BOTH)) {
        bounds.setBounds(previousBounds);
      }
      else {
        bounds.setSize(theFrame.getSize());
      }
    }
  }

  @Override
  public void componentShown(ComponentEvent e) {
  }

  @Override
  public void componentHidden(ComponentEvent e) {
  }

  protected void setFrameBounds() {
    final Rectangle desktopBounds = GraphicsEnvironment.getLocalGraphicsEnvironment( ).getMaximumWindowBounds( );

    // Respect any existing bounds
    if (bounds.width != 0 && bounds.height != 0) {
      theFrame.setSize(new Dimension(Math.abs(bounds.width),Math.abs(bounds.height)));
    }
    theFrame.setLocation(bounds.getLocation());

    // Reduce size to fit on desktop
    int width = Math.min(theFrame.getSize().width, desktopBounds.width);
    int height = Math.min(theFrame.getSize().height, desktopBounds.height);
    if (width != theFrame.getSize().width || height != theFrame.getSize().height) {
      theFrame.setSize(width, height);
    }

    // Slide whole window onto desktop if any part off desktop
    int x = theFrame.getLocation().x;
    int y = theFrame.getLocation().y;

    if (x < desktopBounds.x) x = desktopBounds.x;
    if (y < desktopBounds.y) y = desktopBounds.y;

    if (x + theFrame.getSize().width > desktopBounds.x + desktopBounds.width) {
      x = (desktopBounds.x + desktopBounds.width) - theFrame.getSize().width;
    }
    if (y + theFrame.getSize().height > desktopBounds.y + desktopBounds.height) {
      y = (desktopBounds.y + desktopBounds.height) - theFrame.getSize().height;
    }
    if (x != theFrame.getLocation().x || y != theFrame.getLocation().y) {
      theFrame.setLocation(x, y);
    }
  }

}
