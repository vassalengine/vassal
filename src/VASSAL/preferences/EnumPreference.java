/*
 * $Id$
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import VASSAL.configure.Configurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.tools.ArrayUtils;

/**
 * A Drop-down list preference.
 */
public class EnumPreference extends BasicPreference {

  public static final String LIST = "list";

  protected String defaultValue = "";
  protected String[] options = new String[0];
  protected StringEnumConfigurer config;

  public static String getConfigureTypeName() {
    return "Drop-down List Preference";
  }

  public Class<?> getDefaultClass() {
    return String.class;
  }

  public String getDefaultValue() {
    return defaultValue;
  }

  public void setDefaultValue(Object value) {
    defaultValue = (String) value;
  }

  public String[] getAttributeNames() {
    return ArrayUtils.append(
      super.getAttributeNames(),
      LIST
    );
  }

  public String[] getAttributeDescriptions() {
    return ArrayUtils.append(
      super.getAttributeDescriptions(),
      "List Values:  "
    );
  }

  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.append(
      super.getAttributeTypes(),
      String[].class
    );
  }

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

  public String getAttributeValueString(String key) {
    if (LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(options);
    }
    else
      return super.getAttributeValueString(key);
  }

  public Configurer getPreferenceConfigurer() {
    if (config == null) {
      config = new StringEnumConfigurer(getVariableName(), getDescription(), options);
      config.setValue(defaultValue);
      config.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateGlobalProperty(config.getValueString());
        }});
    }
    return config;
  }

}
