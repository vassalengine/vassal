/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.i18n;

import java.text.MessageFormat;
import java.util.ResourceBundle;

/**
 * Utility class for extracting strings from a ResourceBundle
 * 
 * @author rodneykinney
 * 
 */
public class BundleHelper {
  private ResourceBundle bundle;

  public BundleHelper(ResourceBundle bundle) {
    this.bundle = bundle;
  }

  public String getString(String id) {
    String s = null;
    try {
      s = bundle.getString(id);
    }
    catch (Exception ex) {
      System.err.println("No Translation: " + id);
    }
    // 2. Worst case, return the key
    if (s == null) {
      s = id;
    }
    return s;
  }

  public String getString(String id, Object... args) {
    return new MessageFormat(getString(id)).format(args);
  }

  public ResourceBundle getResourceBundle() {
    return bundle;
  }
}
