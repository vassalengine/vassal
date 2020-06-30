/*
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

import VASSAL.counters.GamePiece;

/**
 * An {@link ImageOp} which has a {@link GamePiece} as its source.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public interface GamePieceOp extends ImageOp {
  public GamePiece getPiece();

  public String getState();

  public boolean isChanged();
}
