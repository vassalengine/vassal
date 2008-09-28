/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public interface ImageOp extends VASSAL.tools.opcache.Op<Image> {
  public Image apply() throws Exception;

  public Image getImage();

  public Image getImage(ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  public Future<Image> getFutureImage(ImageOpObserver obs)
    throws ExecutionException;

  public Dimension getSize();

  public int getWidth();

  public int getHeight();

  public Dimension getTileSize();

  public int getTileHeight();

  public int getTileWidth();

  public int getNumXTiles();

  public int getNumYTiles();

  public Image getTile(Point p, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  public Image getTile(int tileX, int tileY, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  public Future<Image> getFutureTile(Point p, ImageOpObserver obs)
    throws ExecutionException;

  public Future<Image> getFutureTile(int tileX, int tileY, ImageOpObserver obs)
    throws ExecutionException;

  public ImageOp getTileOp(Point p); 

  public ImageOp getTileOp(int tileX, int tileY);

  public Point[] getTileIndices(Rectangle rect);
}
