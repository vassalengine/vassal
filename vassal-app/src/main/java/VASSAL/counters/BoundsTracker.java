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
package VASSAL.counters;

import java.awt.Rectangle;
import java.util.List;
import java.util.LinkedList;

import VASSAL.build.module.Map;
import VASSAL.tools.lang.Pair;

/**
 * Records the bounding boxes of GamePieces.  Use addPiece() to
 * record the bounding box of a GamePiece at a certain time.  Use
 * repaint() to repaint the appropriate areas of the maps to which the
 * added pieces belonged.
 */
public class BoundsTracker {
  private final List<Pair<Rectangle, Map>> regions;

  public BoundsTracker() {
    regions = new LinkedList<>();
  }

  public void clear() {
    regions.clear();
  }

  public void addPiece(GamePiece p) {
    if (p.getMap() != null) {
      final Rectangle region = p.boundingBox();
      region.translate(p.getPosition().x, p.getPosition().y);
      regions.add(new Pair<>(region, p.getMap()));
    }
  }

  public void repaint() {
    for (final Pair<Rectangle, Map> rmPair : regions) {
      rmPair.second.repaint(rmPair.first);
    }
  }
}
