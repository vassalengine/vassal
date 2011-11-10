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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 21, 2002
 * Time: 10:15:59 PM
 * To change template for new interface use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;

import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;

/**
 * Provides methods for assigning names to locations on a MapGrid, and drawing those locations when drawing a grid
 */
public interface GridNumbering {
  public static final String FIRST = "first";
  public static final String SEP = "sep";
  public static final String H_TYPE = "hType";
  public static final String V_TYPE = "vType";
  public static final String H_LEADING = "hLeading";
  public static final String V_LEADING = "vLeading";
  public static final String H_OFF = "hOff";
  public static final String V_OFF = "vOff";
  public static final String V_DESCEND = "vDescend";
  public static final String H_DESCEND = "hDescend";
  public static final String FONT_SIZE = "fontSize";
  public static final String COLOR = "color";
  public static final String VISIBLE = "visible";
  public static final String ROTATE_TEXT = "rotateText";
  public static final String H_DRAW_OFF = "hDrawOff";
  public static final String V_DRAW_OFF = "vDrawOff";
  public static final String LOCATION_FORMAT = "locationFormat";

  public String locationName(Point pt);
   public String localizedLocationName(Point pt);

   public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed);

   public boolean isVisible();
   public void setVisible(boolean isVisible);
   public Point getLocation(String location) throws BadCoords;
}
