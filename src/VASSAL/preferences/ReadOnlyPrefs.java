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
import java.io.InputStream;
import java.io.IOException;
import java.net.URI;
import java.util.Properties;

import VASSAL.Info;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.URIUtils;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.zipfs.ZipFileSystem;

/**
 * A simple prefernces class which permits reading stored values.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ReadOnlyPrefs {
  protected Properties storedValues = new Properties();
  protected String name;

  /**
   * @param name the module name of the preferences to read
   */
  public ReadOnlyPrefs(String name) {
    this.name = name;

    FileSystem fs = null;
    try {
      final File pfile = new File(Info.getHomeDir(), "Preferences");

      final URI uri = URIUtils.toURI("zip", pfile);
      fs = FileSystems.newFileSystem(uri, DataArchive.zipOpts);

      final Path path = fs.getPath(name);
      if (path.exists()) {
        InputStream in = null;
        try {
          in = new BufferedInputStream(path.newInputStream());
          storedValues.load(in);
          in.close();
        }
        finally {
          IOUtils.closeQuietly(in);
        }
      }

      fs.close();
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, ((ZipFileSystem) fs).getZipFileSystemFile());
    }
    finally {
      IOUtils.closeQuietly(fs);
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
    return new ReadOnlyPrefs("VASSAL");
  }
}
