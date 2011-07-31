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
package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.image.FilteredImageSource;

import VASSAL.build.GameModule;

/**
 * A class that draws a GamePiece with a specifyable level of transparency
 * @deprecated No longer used by anything.
 */
@Deprecated
public class Transparent {
  private double alpha = 0.2;
  private PieceImage opaque;
  private Image im;
  private GamePiece piece;
  private Point offset;

  public Transparent(GamePiece p) {
    setPiece(p);
  }

  public void setPiece(GamePiece p) {
    piece = p;
    opaque = new PieceImage(p);
  }

  public GamePiece getPiece() {
    return piece;
  }

  public void setAlpha(double val) {
    alpha = val;
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (alpha == 1.0) {
      piece.draw(g, x, y, obs, zoom);
      return;
    }
    if (opaque.isChanged()) {

      int trans = VASSAL.tools.TransparentFilter.getOffscreenEquivalent(obs.getBackground().getRGB(), obs);
      VASSAL.tools.TransparentFilter filter = new VASSAL.tools.TransparentFilter();
      filter.setAlpha(alpha);
      filter.setAlpha(0.0, trans);
      im = opaque.getImage(obs);
      Image im2 = obs.createImage(im.getWidth(obs), im.getHeight(obs));
      Graphics2D gg = (Graphics2D) im2.getGraphics();
      gg.drawImage(im, 0, 0, obs);
      gg.dispose();
      im = obs.createImage(new FilteredImageSource
        (im2.getSource(), filter));
      offset = new Point(piece.boundingBox().x,
                         piece.boundingBox().y);

/*
      im = opaque.getImage(obs);
      final Image im2 = obs.createImage(im.getWidth(obs), im.getHeight(obs));
      final Graphics2D gg = (Graphics2D) im2.getGraphics();
      gg.setComposite(
        AlphaComposite.getInstance(AlphaComposite.SRC_OVER, (float) alpha));
      gg.drawImage(im, 0, 0, obs);
      gg.dispose();

      offset = new Point(piece.boundingBox().x,
                         piece.boundingBox().y);
*/
    }

    Image scaled = im;
    if (zoom != 1.0) {
      scaled = GameModule.getGameModule().getDataArchive().getScaledImage(im,zoom);
    }
    g.drawImage(scaled,
                x + (int) (zoom * offset.x),
                y + (int) (zoom * offset.y),
                obs);
  }
}
