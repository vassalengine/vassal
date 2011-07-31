/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
 * A Configurer for Double values
 */
public class DoubleConfigurer extends StringConfigurer {
  public DoubleConfigurer(String key, String name) {
    this(key, name, 0.0);
  }

  public DoubleConfigurer(String key, String name, Double val) {
    super(key, name, val == null ? null : val.toString());
  }

  public void setValue(String s) {
    Double d = null;
    try {
      d = Double.valueOf(s);
    }
    catch (NumberFormatException e) {
      d = null;
    }

    if (d != null) {
      setValue(d);
    }

    if (!noUpdate && nameField != null) {
      nameField.setText(d.toString());
    }
  }

  public String getValueString() {
    return value == null ? null : value.toString();
  }
}
