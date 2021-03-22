/*
 *
 * Copyright (c) 2008-2009 by Brent Easton
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
package VASSAL.script;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.i18n.ComponentI18nData;

/**
 * Container for Scripts.
 */
public class ScriptContainer extends AbstractConfigurable {
  private static ScriptContainer instance;

  public static ScriptContainer getInstance() {
    if (instance == null) {
      instance = new ScriptContainer();
    }
    return instance;
  }

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

  @Override
  public Configurer getConfigurer() {
    return null;
  }

  @Override
  public void addTo(Buildable parent) {
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{ActionScript.class, GeneralScript.class};
  }

  public static String getConfigureTypeName() {
    return "Scripting";
  }

  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof AbstractScript) {
      ((AbstractScript) b).compile();
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Script.html"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public ComponentI18nData getI18nData() {
    final ComponentI18nData data = super.getI18nData();
    data.setAllAttributesUntranslatable();
    return data;
  }

}
