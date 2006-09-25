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

  public Class getDefaultClass() {
    return String.class;
  }

  public String getDefaultValue() {
    return defaultValue;
  }

  public void setDefaultValue(Object value) {
    defaultValue = (String) value;
  }
  
  public String[] getAttributeNames() {
    String[] a = super.getAttributeNames();
    String[] b = new String[a.length+1];
    System.arraycopy(a, 0, b, 0, a.length);
    b[a.length] = LIST;
    return b;
  }

  public String[] getAttributeDescriptions() {
    String[] a = super.getAttributeDescriptions();
    String[] b = new String[a.length+1];
    System.arraycopy(a, 0, b, 0, a.length);
    b[a.length] = "List Values:  ";
    return b;
  }
  
  public Class[] getAttributeTypes() {
    Class[] a = super.getAttributeTypes();
    Class[] b = new Class[a.length+1];
    System.arraycopy(a, 0, b, 0, a.length);
    b[a.length] = String[].class;
    return b;
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
