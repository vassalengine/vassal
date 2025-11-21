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
import java.awt.GraphicsDevice;
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
  public static final String key = "BoundsOf"; //NON-NLS
  private static final Point initialPos = new Point(0, 0);

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
    final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    if (initialPos.x >= d.width - 30
      || initialPos.y >= d.height - 30) {
      initialPos.move(0, 0);
    }
    else {
      initialPos.translate(30, 30);
    }
  }

  public PositionOption(String key, Window f) {
    this(key, f, new Rectangle(initialPos, new Dimension(0, 0)));
  }

  @Override
  public Object getValue() {
    return bounds;
  }

  @Override
  public void setValue(Object o) {
    if (o instanceof Rectangle) {
      bounds = new Rectangle((Rectangle) o);
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
        ErrorDialog.dataWarning(new BadDataReport("Map or Chart window with same name as piece Palette", getKey(), e));  //NON-NLS
      }
      else {
        ErrorDialog.bug(e);
      }
    }
  }

  /**
   * Returns the union of the bounds of all available screens (aka the virtual desktop bounds).
   * This is used for validating and clamping window locations/sizes in multi-monitor setups.
   */
  private static Rectangle getVirtualDesktopBounds() {
    final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
    Rectangle virtual = null;
    for (final GraphicsDevice gd : ge.getScreenDevices()) {
      final Rectangle b = gd.getDefaultConfiguration().getBounds();
      if (virtual == null) {
        virtual = new Rectangle(b);
      }
      else {
        virtual = virtual.union(b);
      }
    }
    // Fallback (shouldn't happen) to primary screen size
    if (virtual == null) {
      final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      virtual = new Rectangle(0, 0, d.width, d.height);
    }
    return virtual;
  }

  /**
   * Returns the bounds of all physical screens as reported by the GraphicsEnvironment.
   * The rectangles are in the global (virtual desktop) coordinate space, so they may
   * have non-zero x/y and can be vertically/horizontally offset and of different sizes.
   */
  private static Rectangle[] getAllScreenBounds() {
    final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
    final GraphicsDevice[] devices = ge.getScreenDevices();
    final Rectangle[] result = new Rectangle[devices.length];
    for (int i = 0; i < devices.length; i++) {
      result[i] = new Rectangle(devices[i].getDefaultConfiguration().getBounds());
    }
    return result;
  }

  /** Returns true if r intersects any actual screen (not just the union bounding box). */
  private static boolean intersectsAnyScreen(Rectangle r) {
    for (final Rectangle s : getAllScreenBounds()) {
      if (s.intersects(r) || s.contains(r)) return true;
    }
    return false;
  }

  /**
   * Choose the best screen for the given rectangle. Preference order:
   * 1) Screen with the largest intersection area with r
   * 2) If no intersection, screen containing r's top-left point
   * 3) Otherwise, nearest screen by distance from r's center to the screen's rectangle
   */
  private static Rectangle chooseBestScreen(Rectangle r) {
    final Rectangle[] screens = getAllScreenBounds();
    if (screens.length == 0) return getVirtualDesktopBounds();

    // 1) Max intersection area
    long bestArea = -1;
    Rectangle best = null;
    for (final Rectangle s : screens) {
      final Rectangle inter = s.intersection(r);
      final long area = (long) Math.max(0, inter.width) * Math.max(0, inter.height);
      if (area > bestArea) {
        bestArea = area;
        best = s;
      }
    }
    if (bestArea > 0) return new Rectangle(best);

    // 2) Contains top-left
    for (final Rectangle s : screens) {
      if (s.contains(r.getLocation())) return new Rectangle(s);
    }

    // 3) Nearest by center distance
    final double cx = r.getCenterX();
    final double cy = r.getCenterY();
    double bestDist = Double.MAX_VALUE;
    best = screens[0];
    for (final Rectangle s : screens) {
      final double sx = clamp(cx, s.getMinX(), s.getMaxX());
      final double sy = clamp(cy, s.getMinY(), s.getMaxY());
      final double dx = cx - sx;
      final double dy = cy - sy;
      final double dist2 = dx * dx + dy * dy;
      if (dist2 < bestDist) {
        bestDist = dist2;
        best = s;
      }
    }
    return new Rectangle(best);
  }

  private static double clamp(double v, double lo, double hi) {
    return Math.max(lo, Math.min(v, hi));
  }

  @Override
  public String getValueString() {
    return bounds.x + "," + bounds.y + "," + bounds.width + "," + bounds.height;
  }

  private boolean isOnScreen(Point p) {
    return getVirtualDesktopBounds().contains(p);
  }

  @Override
  public void componentMoved(ComponentEvent e) {
    if (theFrame.isShowing()) {
      final Point p = theFrame.getLocationOnScreen();
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
    // Use the union of all screen bounds for general validation,
    // but select a specific screen for clamping to handle different resolutions and offsets.
    final Rectangle desktopBounds = getVirtualDesktopBounds();
    final Rectangle requested = bounds == null ? null : new Rectangle(bounds);

    // Respect any existing bounds
    if (bounds != null) {
      if (bounds.width != 0 && bounds.height != 0) {
        theFrame.setSize(new Dimension(Math.abs(bounds.width), Math.abs(bounds.height)));
      }
      theFrame.setLocation(bounds.getLocation());
    }

    // Choose the screen we will clamp to
    final Rectangle targetScreen = (requested != null) ? chooseBestScreen(requested) : chooseBestScreen(new Rectangle(desktopBounds));

    // Reduce size to fit on chosen screen
    final int width = Math.min(theFrame.getSize().width, targetScreen.width);
    final int height = Math.min(theFrame.getSize().height, targetScreen.height);
    if (width != theFrame.getSize().width || height != theFrame.getSize().height) {
      theFrame.setSize(width, height);
    }

    // Slide whole window onto the chosen screen if any part off that screen
    int x = theFrame.getLocation().x;
    int y = theFrame.getLocation().y;
    if (x < targetScreen.x) x = targetScreen.x;
    if (y < targetScreen.y) y = targetScreen.y;
    if (x + theFrame.getSize().width > targetScreen.x + targetScreen.width) {
      x = (targetScreen.x + targetScreen.width) - theFrame.getSize().width;
    }
    if (y + theFrame.getSize().height > targetScreen.y + targetScreen.height) {
      y = (targetScreen.y + targetScreen.height) - theFrame.getSize().height;
    }
    final boolean willRelocate = (x != theFrame.getLocation().x || y != theFrame.getLocation().y);
    if (willRelocate) theFrame.setLocation(x, y);

    // Log final applied bounds and environment
    Rectangle applied = new Rectangle(theFrame.getX(), theFrame.getY(), theFrame.getWidth(), theFrame.getHeight());
    boolean appliedOnAnyScreen = intersectsAnyScreen(applied);

    // Final safety: ensure never off-screen
    if (!appliedOnAnyScreen) {
      // Clamp again just in case and log a warning
      // Choose best screen again based on the applied rect and clamp to it
      final Rectangle safeScreen = chooseBestScreen(applied);
      x = Math.max(safeScreen.x, Math.min(applied.x, safeScreen.x + safeScreen.width - applied.width));
      y = Math.max(safeScreen.y, Math.min(applied.y, safeScreen.y + safeScreen.height - applied.height));
      theFrame.setLocation(x, y);
      applied = new Rectangle(theFrame.getX(), theFrame.getY(), theFrame.getWidth(), theFrame.getHeight());
      appliedOnAnyScreen = intersectsAnyScreen(applied);
      if (!appliedOnAnyScreen) {
        // As a last resort, move to desktop origin
        theFrame.setLocation(desktopBounds.x, desktopBounds.y);
      }
    }

  }

}
