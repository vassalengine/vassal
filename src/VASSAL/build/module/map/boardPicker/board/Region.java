/*
 * $Id$
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module.map.boardPicker.board;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Area;

import javax.swing.JComponent;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.counters.Labeler;
import VASSAL.i18n.Resources;


public class Region extends AbstractConfigurable {

  private Point origin = new Point(0, 0);

  private RegionGrid myGrid;

  // Variables for the GUI configurer
  private Rectangle selectionRect = new Rectangle();
  private boolean selected = false;

  public Region() {
    setConfigureName(Resources.getString("Editor.Region.new_region")); //$NON-NLS-1$
  }

  public Region(Point p) {
    this();
    origin = p;
  }

  public Region(Region r) {
    setConfigureName(r.getName());
    origin.x = r.getOrigin().x;
    origin.y = r.getOrigin().y;
    myGrid = r.myGrid;
  }

  public String getName() {
    return getConfigureName();
  }

  public String getLocalizedName() {
    return getLocalizedConfigureName();
  }

  public Rectangle getSelectionRect() {
    return selectionRect;
  }

  public Point getOrigin() {
    return origin;
  }

  public void setOrigin(Point p) {
    origin = p;
  }

  public void moveOrigin(int dx, int dy) {
    origin.translate(dx, dy);
  }

  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String X = "originx"; //$NON-NLS-1$
  public static final String Y = "originy"; //$NON-NLS-1$

  public String[] getAttributeNames() {
    return new String[] {
      NAME,
      X,
      Y
    };
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString("Editor.Region.x_coord"), //$NON-NLS-1$
      Resources.getString("Editor.Region.y_coord"), //$NON-NLS-1$
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Integer.class,
      Integer.class
    };
  }

  public void addTo(Buildable b) {
    myGrid = (RegionGrid) b;
    myGrid.addRegion(this);
  }

  public void removeFrom(Buildable b) {
    ((RegionGrid) b).removeRegion(this);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Region.component_type"); //$NON-NLS-1$
  }

  public boolean contains(Point p) {
    return selectionRect.contains(p);
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }


  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (X.equals(key)) {
      return String.valueOf(origin.x);
    }
    else if (Y.equals(key)) {
      return String.valueOf(origin.y);
    }
    return null;
  }

  public void setAttribute(String key, Object val) {
    if (val == null)
      return;

    if (NAME.equals(key)) {
      setConfigureName((String) val);
      if (myGrid != null) myGrid.addRegion(this);
    }
    else if (X.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }
      origin.x = ((Integer) val).intValue();
    }
    else if (Y.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }
      origin.y = ((Integer) val).intValue();
    }

  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public String locationName() {
    return getConfigureName();
  }

  public String localizedLocationName() {
    return getLocalizedConfigureName();
  }

  public boolean isSelected() {
    return selected;
  }

  public void setSelected(boolean s) {
    selected = s;
  }

  public Board getBoard() {
    return myGrid.getBoard();
  }
  /*
   * Move the region. Ensure the selectable region remains within
   * its container
   */
  public void move(int dx, int dy, JComponent c) {

    final Point newP = new Point (getOrigin());
    newP.translate(dx, dy);

    if (myGrid.container.contains(newP)) {
      moveOrigin(dx, dy);
    }

    return;
  }

  /*
   * If the grid is visible, draw a dot and a label. Mainly of use for testing
   * a newly created grid.
   */
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    draw(g, bounds, visibleRect, scale, reversed, 0, 0);
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed, int xOffset, int yOffset) {
    if (!bounds.intersects(visibleRect)) {
      return;
    }

    final int labelOffset = 7;

    int size = (int) (scale * myGrid.getFontSize() + 0.5);
    Font f = new Font("Dialog", Font.PLAIN, size); //$NON-NLS-1$

    Color fg = selected ? Color.white : Color.black;
    Color bg = selected ? Color.black : Color.white;

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g.getClip();
    if (oldClip != null) {
      Area clipArea = new Area(oldClip);
      clipArea.intersect(new Area(region));
      g.setClip(clipArea);
    }

    int posX = (int) (scale * origin.x + 0.5) + bounds.x - 1 + xOffset;
    int posY = (int) (scale * origin.y + 0.5) + bounds.y - 1 + yOffset;

    Color saveColor = g.getColor();

    g.setColor(bg);
    g.fillRect(posX, posY, 3, 3);
    g.setColor(fg);
    g.drawRect(posX, posY, 3, 3);

    g.setColor(saveColor);

    Labeler.drawLabel(g, getLocalizedConfigureName(), posX, posY + labelOffset, f, Labeler.CENTER,
                      Labeler.TOP, fg, bg, fg);
    g.setClip(oldClip);

    // Calculate and store the selection rectangle
    int width = g.getFontMetrics().stringWidth(getConfigureName() + "  ")+1; //$NON-NLS-1$
    int height = g.getFontMetrics().getHeight()+1;

    selectionRect.setLocation(posX - (width / 2), posY - 1);
    selectionRect.setSize(width, height + labelOffset + 1);

  }
}

