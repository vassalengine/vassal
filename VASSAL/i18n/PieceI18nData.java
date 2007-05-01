/*
 * $Id: PieceI18nData.java 1417 2006-11-03 14:57:33 +0000 (Fri, 03 Nov 2006) rodneykinney $
 *
 * Copyright (c) 2000-2006 by Rodney Kinney, Brent Easton
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

import java.util.ArrayList;

import VASSAL.counters.GamePiece;

/**
 * Object encapsulating the internationalization information for a GamePiece.
 * @author Brent Easton
 *
 */
public class PieceI18nData {

  protected GamePiece piece;
  protected ArrayList<String> values = new ArrayList<String>();
  protected ArrayList<String> descriptions = new ArrayList<String>();
  
  public PieceI18nData(GamePiece piece) {
    this.piece = piece;
  }
  
  public String[] getValues() {
    return values.toArray(new String[values.size()]);
  }

  public String[] getDescriptions() {
    return descriptions.toArray(new String[descriptions.size()]);
  }
  
  public void add(String value, String description) {
    if (value != null && value.length() > 0) {
      values.add(value);
      descriptions.add(description);
    }
  }
}