/*
 * $Id$
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.configure;

/**
 * A Configurer for Integer values
 */
public class IntConfigurer extends StringConfigurer {
  public IntConfigurer(String key, String name) {
    this(key, name, 0);
  }

  public IntConfigurer(String key, String name, Integer val) {
    super(key, name, 4);
    if (val != null) {
      setValue(val);
    }
  }

  public void setValue(String s) {
    Integer i = null;
    try {
      i = Integer.valueOf(s);
    }
    catch (NumberFormatException e) {
      i = null;
    }
    if (i != null) {
      setValue(i);
    }
  }

  public int getIntValue(int defaultValue) {
    if (getValue() instanceof Integer) {
      return ((Integer)getValue()).intValue();
    }
    else {
      return defaultValue;
    }
  }

  public void setValue(Object o) {
    if (!noUpdate && nameField != null && o != null) {
      nameField.setText(o.toString());
    }
    super.setValue(o);
  }

  public String getValueString() {
    return value == null ? null : value.toString();
  }
}
