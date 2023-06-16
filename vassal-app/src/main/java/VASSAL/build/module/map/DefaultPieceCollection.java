/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;

/**
 * {@link VASSAL.build.module.Map}s now start with a Game Piece Layers component (i.e. {@link LayeredPieceCollection}) by default, and so
 * will use that more complex implementation of visual layers. However, the LayeredPieceCollection can be deleted, in
 * which case this simpler implementation is used. This fallback implementation of {@link PieceCollection} separates
 * pieces into two visual layers: stacking pieces always drawn above non-stacking pieces
 */
public class DefaultPieceCollection extends CompoundPieceCollection {
  public DefaultPieceCollection() {
    super(2);
  }

  @Override
  public int getLayerForPiece(GamePiece p) {
    return Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK)) ? 0 : 1;
  }
}
