/*
 * $Id$
 *
 * Copyright (c) 2007 by Rodney Kinney, Brent Easton
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
package VASSAL.i18n;

import VASSAL.counters.EditablePiece;

/**
 * Decorators that contain localizable elements must implement
 * this interface
 *
 * @author Brent Easton
 *
 */
public interface TranslatablePiece extends EditablePiece {

  public static final String PREFIX = "Piece.";

  /**
   * Return a PieceI18nData object returning the I18n data about this GamePiece.
   */
  public PieceI18nData getI18nData();

}
