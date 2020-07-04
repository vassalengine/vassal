/*
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

/**
 * A Class representing an Attribute of a Component that can be translated.
 */
public class TranslatableAttribute {
  Translatable component;
  String attributeName;
  String originalValue;

  public TranslatableAttribute(Translatable t, String name, String value) {
    component = t;
    attributeName = name;
    originalValue = value;
  }

  public void applyTranslation(String translation) {
    if (translation != null && translation.length() > 0) {
      component.getI18nData().applyTranslation(attributeName, translation);
    }
  }

  public String getUntranslatedValue() {
    return originalValue;
  }

  public String getKey() {
    return component.getI18nData().getFullPrefix() + attributeName; //$NON-NLS-1$
  }

  public boolean isTranslatable() {
    return component.getI18nData().isAttributeTranslatable(attributeName);
  }
}