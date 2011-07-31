/*
 * $Id$
 *
 * Copyright (c) 2004-2009 by Brent Easton, Rodney Kinney
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
import VASSAL.i18n.ComponentI18nData;

public abstract class AbstractScript extends AbstractConfigurable {

  public static final String NAME = "name";
  public static final String DESC = "desc";
  public static final String SCRIPT = "script";

  private String description = "";
  private String script = "";

  public void setDescription(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

  public void setScript(String script) {
    this.script = script;
  }

  public String getScript() {
    return script;
  }

  public abstract CompileResult compile();

  public ComponentI18nData getI18nData() {
    ComponentI18nData data = super.getI18nData();
    data.setAllAttributesUntranslatable();
    return data;
  }

  /* --------------------------------------------
   * AbstractConfigurable implementation
   * Minimal due to custom Configurer
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {NAME, DESC, SCRIPT};
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DESC.equals(key)) {
      return getDescription();
    }
    else if (SCRIPT.equals(key)) {
      return getScript();
    }
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DESC.equals(key)) {
      setDescription((String) value);
    }
    else if (SCRIPT.equals(key)) {
      setScript((String) value);
    }
    return;
  }


}