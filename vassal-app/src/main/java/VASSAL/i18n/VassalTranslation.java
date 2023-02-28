/*
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Locale;
import java.util.Properties;

import VASSAL.tools.ReadErrorDialog;

/**
 * Utility class to allow translation of VASSAL using the Component
 * Translation mechanism.
 *
 * @author Brent Easton
 */
public class VassalTranslation extends Translation {
  protected String[] allKeys;

  protected Properties baseValues = new Properties();

  public VassalTranslation() {
    setConfigureName(Resources.VASSAL);

    final String propFile = "VASSAL" + Resources.BASE_BUNDLE;

    try (InputStream is = getClass().getResourceAsStream(propFile)) {
      if (is == null) {
        throw new FileNotFoundException(propFile + " not found");
      }

      try (BufferedInputStream in = new BufferedInputStream(is)) {
        baseValues.load(in);
      }
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, propFile);
    }
  }

  @Override
  protected String getI18nPrefix() {
    return "";
  }

  @Override
  protected String getDescription() {
    return Resources.VASSAL;
  }

  public void clearProperties() {
    localProperties = new Properties();
    dirty = false;
    allKeys = null;
  }

  @Override
  public String getAttributeValueString(String key) {
    return baseValues.getProperty(key);
  }

  @Override
  public String[] getAttributeDescriptions() {
    initkeys();
    return allKeys;
  }

  @Override
  public String[] getAttributeNames() {
    initkeys();
    return allKeys;
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    initkeys();
    final Class<?>[] types = new Class<?>[allKeys.length];
    Arrays.fill(types, String.class);
    return types;
  }

  protected void initkeys() {
    if (allKeys == null) {
      allKeys = Resources.getVassalKeys().toArray(new String[0]);
    }
  }

  public void saveProperties(File file, Locale locale) throws IOException {
    try (OutputStream fout = Files.newOutputStream(file.toPath());
         BufferedOutputStream out = new BufferedOutputStream(fout)) {
      localProperties.store(out, locale.getDisplayName());
      dirty = false;
    }
  }

  protected void loadProperties(InputStream in) throws IOException {
    try (in) {
      localProperties.load(in);
      dirty = false;
    }
  }
}
