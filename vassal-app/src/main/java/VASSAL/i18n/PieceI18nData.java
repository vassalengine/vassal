/*
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
import java.util.Collections;
import java.util.List;

import VASSAL.counters.GamePiece;

/**
 * Object encapsulating the internationalization information for a GamePiece.
 *
 * @author Brent Easton
 *
 */
public class PieceI18nData {
  protected GamePiece piece;
  protected List<Property> properties = new ArrayList<>();

  public PieceI18nData(GamePiece piece) {
    this.piece = piece;
  }

  public List<Property> getProperties() {
    return Collections.unmodifiableList(properties);
  }

  public void add(String value, String description) {
    if (value != null && value.length() > 0) {
      properties.add(new Property(value, description));
    }
  }

  public String translate(String value) {
    String localisedValue = value;
    String i18nKey = null;

    for (final Property p : getProperties()) {
      if (p.getName().equals(value)) {
        i18nKey = TranslatablePiece.PREFIX + p.getName();
        break;
      }
    }
    if (i18nKey != null) {
      localisedValue = Localization.getInstance().translate(i18nKey, value);
    }
    return localisedValue;
  }

  public static class Property {
    private final String name;
    private final String description;

    public Property(String value, String description) {
      super();
      this.name = value;
      this.description = description;
    }

    public String getDescription() {
      return description;
    }

    public String getName() {
      return name;
    }
  }
}
