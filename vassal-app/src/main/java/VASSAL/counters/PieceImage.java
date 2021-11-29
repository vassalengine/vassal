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

import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import VASSAL.tools.imageop.GamePieceOp;

/**
 * Maintains an {@link Image} built from the {@link GamePiece#draw}
 * method of a {@link GamePiece}
 * @deprecated Use {@link GamePieceOp} instead.
 */
@Deprecated(since = "2021-12-01", forRemoval = true)
public class PieceImage {
  private final GamePiece piece;
  private String lastState = null;
  private Image im;

  public PieceImage(GamePiece piece) {
    this.piece = piece;
  }

  public Image getImage(Component obs) {
    if (isChanged()) {
      lastState = String.valueOf(piece.getProperty(Properties.VISIBLE_STATE));

      final Rectangle bbox = piece.boundingBox();
      im = new BufferedImage(Math.max(bbox.width, 1), Math.max(bbox.height, 1), BufferedImage.TYPE_4BYTE_ABGR);
      ((BufferedImage) im).setRGB(0, 0, bbox.width, bbox.height, new int[bbox.width * bbox.height], 0, bbox.width);

      final Graphics2D g = (Graphics2D) im.getGraphics();
      piece.draw(g, -bbox.x, -bbox.y, obs, 1.0);
      g.dispose();
    }
    return im;
  }

  public boolean isChanged() {
    return !String.valueOf(piece.getProperty(Properties.VISIBLE_STATE)).equals(lastState);
  }
}
