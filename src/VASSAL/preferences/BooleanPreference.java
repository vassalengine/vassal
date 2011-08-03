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

import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;

/**
 * A Checkbox Module Preference.
 */
public class BooleanPreference extends BasicPreference {

  protected boolean defaultValue = false;
  protected BooleanConfigurer config;

  public static String getConfigureTypeName() {
    return "Checkbox Preference";
  }

  public Class<?> getDefaultClass() {
    return Boolean.class;
  }

  public String getDefaultValue() {
    return Boolean.toString(defaultValue);
  }

  public void setDefaultValue(Object value) {
    if (value instanceof String) {
      value = Boolean.valueOf((String) value);
    }
    defaultValue = ((Boolean) value).booleanValue();
  }

  public Configurer getPreferenceConfigurer() {
    if (config == null) {
      config = new BooleanConfigurer(getVariableName(), getDescription(), defaultValue);
      config.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateGlobalProperty(config.getValueString());
        }});
    }
    return config;
  }
}
