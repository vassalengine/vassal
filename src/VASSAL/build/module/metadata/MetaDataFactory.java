/* $Id$
 *
 * Copyright (c) 2008 by Brent Easton and Joel Uckelman
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
package VASSAL.build.module.metadata;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.tools.imports.ImportAction;
import VASSAL.tools.io.IOUtils;

public class MetaDataFactory {
  private static final Logger logger =
    LoggerFactory.getLogger(MetaDataFactory.class);

  protected static final String BUILDFILE_MODULE_ELEMENT1 = "VASSAL.launch.BasicModule";
  protected static final String BUILDFILE_MODULE_ELEMENT2 = "VASSAL.build.GameModule";
  protected static final String BUILDFILE_EXTENSION_ELEMENT = "VASSAL.build.module.ModuleExtension";

  /**
   * Factory method to build and return an appropriate MetaData class based on
   * the contents of the file. Return null if the file is not a Zip archive, or
   * it is not a VASSAL Module, Extension or Save Game.
   *
   * @param file
   *          metadata file
   * @return MetaData object
   */
  public static AbstractMetaData buildMetaData(File file) {

    // Check the file exists and is a file
    if (file == null || !file.exists() || !file.isFile())
      return null;

    ZipFile zip = null;
    try {
      // Check it is a Zip file
      zip = new ZipFile(file);

      // Check if it is a Save Game file
      ZipEntry entry = zip.getEntry(GameState.SAVEFILE_ZIP_ENTRY);
      if (entry != null) {
        return new SaveMetaData(zip);
      }

      // Check if it has a buildFile
      ZipEntry buildFileEntry = zip.getEntry(GameModule.BUILDFILE);
      if (buildFileEntry == null) {
        return null;
      }

      // It's either a module or an Extension, check for existence of metadata
      entry = zip.getEntry(ExtensionMetaData.ZIP_ENTRY_NAME);
      if (entry != null) {
        return new ExtensionMetaData(zip);
      }

      entry = zip.getEntry(ModuleMetaData.ZIP_ENTRY_NAME);
      if (entry != null) {
        return new ModuleMetaData(zip);
      }

      // read the first few lines of the buildFile
      BufferedReader br = null;
      try {
        br = new BufferedReader(new InputStreamReader(zip
            .getInputStream(buildFileEntry)));
        for (int i = 0; i < 10; i++) {
          final String s = br.readLine();
          if (s.indexOf(BUILDFILE_MODULE_ELEMENT1) > 0
              || s.indexOf(BUILDFILE_MODULE_ELEMENT2) > 0) {
            br.close();
            return new ModuleMetaData(zip);
          }
          else if (s.indexOf(BUILDFILE_EXTENSION_ELEMENT) > 0) {
            br.close();
            return new ExtensionMetaData(zip);
          }
        }
        br.close();
      }
      finally {
        IOUtils.closeQuietly(br);
      }

      zip.close();
    }
    catch (ZipException e) {
      // It is not a Zip file, check for an Importable file
      return ImportAction.buildMetaData(file);
    }
    catch (IOException e) {
      logger.error("", e);
    }
    finally {
      IOUtils.closeQuietly(zip);
    }

    return null;
  }
}
