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
import VASSAL.configure.TextConfigurer;

/**
 * A multi-line textbox Module Preference.
 */
public class TextPreference extends BasicPreference {

  protected String defaultValue = "";
  protected TextConfigurer config;

  public static String getConfigureTypeName() {
    return "Text Box Preference";
  }

  public Class<?> getDefaultClass() {
    return TextConfigurer.class;
  }

  public String getDefaultValue() {
    return defaultValue;
  }

  public void setDefaultValue(Object value) {
    defaultValue = (String) value;
  }

  public Configurer getPreferenceConfigurer() {
    if (config == null) {
      config = new TextConfigurer(getVariableName(), getDescription(), defaultValue);
      config.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateGlobalProperty(config.getValueString());
        }});
    }
    return config;
  }

}
