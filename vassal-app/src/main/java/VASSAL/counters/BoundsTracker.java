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
import java.util.ArrayList;
import java.util.List;

import VASSAL.build.module.Map;
import org.apache.commons.lang3.tuple.Pair;

/**
 * Records the bounding boxes of GamePieces.  Use addPiece() to
 * record the bounding box of a GamePiece at a certain time.  Use
 * repaint() to repaint the appropriate areas of the maps to which the
 * added pieces belonged.
 */
public class BoundsTracker {
  private final List<Pair<Map, Rectangle>> regions;

  public BoundsTracker() {
    regions = new ArrayList<>();
  }

  public void clear() {
    regions.clear();
  }

  public void addPiece(GamePiece p) {
    if (p.getMap() != null) {
      if(p.getParent() != null) {
        // NB: We track the Stack if there is one. This is because individual pieces within a Stack do not include
        // their stack-offsets in `boundingBox()` and so the repaint would not include these offsets. This matters
        // for rotate-piece (for example). Note: other effects (like area-of-effect) are not offset from the Stack.
        p = p.getParent();
      }
      final Rectangle region = p.boundingBox();
      region.translate(p.getPosition().x, p.getPosition().y);
      regions.add(Pair.of(p.getMap(), region));
    }
  }

  public void repaint() {
    for (final Pair<Map, Rectangle> mrPair : regions) {
      mrPair.getLeft().repaint(mrPair.getRight());
    }
  }
}
