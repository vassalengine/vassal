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

package VASSAL.build.module.map;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;

import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

/**
 * Handles the drawing of cards in a {@link VASSAL.build.module.PlayerHand}.
 * Lays out the cards horizontally with no overlap and even spacing.
 */
public class HandMetrics extends StackMetrics {
  public HandMetrics() {
    super(false, 12, 0, 12, 0);
  }

  public void draw(Stack stack, Graphics g, int x, int y, Component obs, double zoom) {
    stack.setExpanded(true);
    super.draw(stack, g, x, y, obs, zoom);
  }

  public void draw(Stack stack, Point location, Graphics g, Map map, double zoom, Rectangle visibleRect) {
    stack.setExpanded(true);
    super.draw(stack, location, g, map, zoom, visibleRect);
  }

  protected void nextPosition(Point currentPos, Rectangle currentBounds, Point nextPos, Rectangle nextBounds, int dx, int dy) {
    int x = currentPos.x + currentBounds.width + dx;
    int y = currentPos.y;
    nextBounds.setLocation(x, y);
    nextPos.setLocation(x, y);
  }

  public Command merge(GamePiece fixed, GamePiece moving) {
    Command c =  super.merge(fixed, moving);
    map.getView().revalidate();
    return c;
  }
}
