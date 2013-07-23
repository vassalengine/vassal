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

package VASSAL.preferences;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import VASSAL.Info;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.io.IOUtils;

/**
 * A simple preferences class which permits reading stored values.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ReadOnlyPrefs {
  protected Properties storedValues = new Properties();

  /**
   * @param name the module name of the preferences to read
   */
  public ReadOnlyPrefs(String name) {
    this(new File(Info.getPrefsDir(), Prefs.sanitize(name)));
  }

  protected ReadOnlyPrefs(File file) {
    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(new FileInputStream(file));
      storedValues.load(in);
      in.close();
    }
    catch (FileNotFoundException e) {
      // First time for this module, not an error.
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, file);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  /**
   * Return the value of a given preference.
   *
   * @param key the name of the preference to retrieve
   * @return the value of this option in the Preferences file, or
   * <code>null</code> if undefined
   */
  public String getStoredValue(String key) {
    return storedValues.getProperty(key);
  }

  /**
   * Return the module-independent global preferences.
   *
   * @return a global preferences object
   */
  public static ReadOnlyPrefs getGlobalPrefs() {
    return new ReadOnlyPrefs(new File(Info.getPrefsDir(), "V_Global"));
  }
}
