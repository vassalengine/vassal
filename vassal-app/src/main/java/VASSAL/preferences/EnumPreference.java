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
package VASSAL.preferences;

import VASSAL.i18n.Resources;
import org.apache.commons.lang3.ArrayUtils;

import VASSAL.configure.Configurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;

/**
 * A Drop-down list preference.
 */
public class EnumPreference extends BasicPreference {

  public static final String LIST = "list"; //NON-NLS

  protected String defaultValue = "";
  protected String[] options = new String[0];
  protected StringEnumConfigurer config;

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.EnumPreference.component_type");
  }

  @Override
  public Class<?> getDefaultClass() {
    return String.class;
  }

  @Override
  public String getDefaultValue() {
    return defaultValue;
  }

  @Override
  public void setDefaultValue(Object value) {
    defaultValue = (String) value;
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.add(
      super.getAttributeNames(),
      LIST
    );
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.add(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.EnumPreference.list_values")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.add(
      super.getAttributeTypes(),
      String[].class
    );
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (LIST.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      options = (String[]) value;
      if (config != null) {
        config.setValidValues(options);
      }
    }
    else
      super.setAttribute(key, value);
  }

  @Override
  public String getAttributeValueString(String key) {
    if (LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(options);
    }
    else
      return super.getAttributeValueString(key);
  }

  @Override
  public Configurer getPreferenceConfigurer() {
    if (config == null) {
      config = new StringEnumConfigurer(getVariableName(), getDescription(), options);
      config.addPropertyChangeListener(e -> updateGlobalProperty(config.getValueString()));
      config.setValue(defaultValue);
    }
    return config;
  }
}
