/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.imageop;

import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.opcache.Op;
import VASSAL.tools.opcache.OpObserver;

/**
 * An {@link ImageOp} which uses a {@link GamePiece} as its source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class GamePieceOpImpl extends AbstractTileOpImpl implements GamePieceOp {
  /** The image source for this <code>ImageOp</code>. */
  private final GamePiece piece;
  private final String state;
  private final int hash;

  /**
   * Constructs an <code>ImageOp</code> which will produce an image
   * from the given <code>GamePiece</code>.
   *
   * @param gp the source
   * @throws IllegalArgumentException if <code>gp == null</code>.
   */
  public GamePieceOpImpl(GamePiece gp) {
    if (gp == null) throw new IllegalArgumentException();
    piece = gp;
    state = String.valueOf(piece.getProperty(Properties.VISIBLE_STATE));
    hash = piece.hashCode();
  }

  @Override
  public BufferedImage get(OpObserver<BufferedImage> obs)
    throws CancellationException, InterruptedException, ExecutionException
  {
    // GamePieceOpImpl CANNOT be called asynchronously becuase it cannot
    // reliably report on its dependencies.
    if (obs != null) throw new UnsupportedOperationException();
    return super.get(obs);
  }

  @Override
  public Future<BufferedImage> getFuture(OpObserver<BufferedImage> obs)
                                                  throws ExecutionException {
    // GamePieceOpImpl CANNOT be called asynchronously becuase it cannot
    // reliably report on its dependencies.
    if (obs != null) throw new UnsupportedOperationException();
    return super.getFuture(obs);
  }

  public List<Op<?>> getSources() {
    return Collections.emptyList();
  }

  /** {@inheritDoc} */
  public BufferedImage eval() {
    final Rectangle b = piece.boundingBox();
    final BufferedImage im = ImageUtils.createCompatibleTranslucentImage(
      Math.max(b.width, 1),
      Math.max(b.height, 1)
    );
    final Graphics2D g = im.createGraphics();
    piece.draw(g, -b.x, -b.y, null, 1.0);
    g.dispose();
    return im;
  }

  /** {@inheritDoc} */
  protected void fixSize() {
    size = piece.boundingBox().getSize();
    if (size.width < 1) size.width = 1;
    if (size.height < 1) size.height = 1;
  }

  /**
   * Returns the source <code>GamePiece</code>.
   *
   * @return the source
   */
  public GamePiece getPiece() {
    return piece;
  }

  public String getState() {
    return state;
  }

  /**
   * Returns <code>true</code> iff the source <code>GamePiece</code>
   * has changed state.
   *
   * @return <code>true</code> iff the source has changed.
   */
  public boolean isChanged() {
    return !state.equals(piece.getProperty(Properties.VISIBLE_STATE));
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    GamePieceOp op = (GamePieceOp) o;
    return piece.equals(op.getPiece()) &&
           state.equals(op.getState());
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
