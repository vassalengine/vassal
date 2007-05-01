/*
 * $Id: VassalTranslation.java 1856 2007-03-09 04:30:49 +0000 (Fri, 09 Mar 2007) rodneykinney $
 *
 * Copyright (c) 2007 by Rodney Kinney, Brent Easton
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Locale;
import java.util.Properties;

/**
 * Utility class to allow translation of VASSAL using the Component
 * Translation mechanism
 * 
 * @author Brent Easton
 *
 */
public class VassalTranslation extends Translation {
  
  public VassalTranslation() {
    setConfigureName("VASSAL");
  }
  
  protected String getDescription() {
    return "VASSAL";
  }


  public void clearProperties() {
    localProperties = new Properties();  
    dirty = false;
  }
  
  public String getAttributeValueString(String key) {
    return Resources.getString(key);
  }
  
  public void saveProperties(File file, Locale locale) {
    try {
      OutputStream out = new FileOutputStream(file);
      localProperties.store(out, locale.getDisplayName());
      dirty = false;
    }
    catch (Exception e) {
      // Some error message here
    }
  }
  
  protected void loadProperties(File file) {

    try {
      InputStream in = new FileInputStream(file);
      localProperties.load(in);
      dirty = false;
    }
    catch (Exception e) {
//    Some error message here
    }
  }
}