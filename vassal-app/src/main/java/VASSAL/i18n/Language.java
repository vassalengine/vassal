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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

/**
 * Container for definitions of Translations.
 */
public class Language extends AbstractConfigurable {
  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public String getAttributeValueString(String key) {
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
  }

  protected String getDescription() {
    return "";
  }

  @Override
  public void addTo(Buildable parent) {
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{Translation.class};
  }

  public static String getConfigureTypeName() {
    return "Translations";
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public void removeFrom(Buildable parent) {
  }
}
