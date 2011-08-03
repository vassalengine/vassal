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
import VASSAL.configure.DoubleConfigurer;

/**
 * A Decimal Number Module Preference.
 */
public class DoublePreference extends BasicPreference {

  protected double defaultValue = 0.0;
  protected DoubleConfigurer config;

  public static String getConfigureTypeName() {
    return "Decimal Number Preference";
  }

  public Class<?> getDefaultClass() {
    return Double.class;
  }

  public String getDefaultValue() {
    return Double.toString(defaultValue);
  }

  public void setDefaultValue(Object value) {
    if (value instanceof String) {
      value = Double.valueOf((String) value);
    }
    defaultValue = ((Double) value).doubleValue();

  }

  public Configurer getPreferenceConfigurer() {
    if (config == null) {
      config =
        new DoubleConfigurer(getVariableName(), getDescription(), defaultValue);
      config.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateGlobalProperty(config.getValueString());
        }
      });
    }
    return config;
  }
}
