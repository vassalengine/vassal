/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
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
 * A Configurer for Long values
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LongConfigurer extends StringConfigurer {
  public LongConfigurer(String key, String name) {
    this(key, name, 0L);
  }

  public LongConfigurer(String key, String name, Long val) {
    super(key, name);
    if (val != null) {
      setValue(val);
    }
  }

  public void setValue(String s) {
    Long l = null;
    try {
      l = Long.valueOf(s);
    }
    catch (NumberFormatException e) {
      l = null;
    }
    if (l != null) {
      setValue(l);
    }
  }

  public long getLongValue(long defaultValue) {
    if (getValue() instanceof Long) {
      return ((Long) getValue()).longValue();
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
