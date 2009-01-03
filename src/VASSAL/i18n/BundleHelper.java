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
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import VASSAL.tools.logging.Logger;

/**
 * Utility class for extracting strings from a {@link ResourceBundle}.
 * 
 * @author rodneykinney
 */
public class BundleHelper {
  private ResourceBundle bundle;

  public BundleHelper(ResourceBundle bundle) {
    this.bundle = bundle;
  }

  public String getString(String id) {
    try {
      return bundle.getString(id);
    }
    catch (ClassCastException e) {
      Logger.log("No Translation: " + id);
    }
    catch (MissingResourceException e) {
      Logger.log("No Translation: " + id);
    }
        
    // fallback: return the key
    return id;
  }

  public String getString(String id, Object... args) {
    try {
      return new MessageFormat(getString(id)).format(args);
    }
    catch (IllegalArgumentException e) {
      Logger.log("Illegal Message Format: " + id);
    }
    
    // fallback: return the key
    return id;
  }

  public ResourceBundle getResourceBundle() {
    return bundle;
  }
}
