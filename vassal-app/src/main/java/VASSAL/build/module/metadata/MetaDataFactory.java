/*
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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.tools.imports.ImportAction;

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

    try (ZipFile zip = new ZipFile(file)) {
      // Check it is a Zip file

      // Check if it is a Save Game file
      ZipEntry entry = zip.getEntry(GameState.SAVEFILE_ZIP_ENTRY);
      if (entry != null) {
        return new SaveMetaData(zip);
      }

      // Check if it has a buildFile
      ZipEntry buildFileEntry = zip.getEntry(GameModule.BUILDFILE);
      if (buildFileEntry == null) {
        buildFileEntry = zip.getEntry(GameModule.BUILDFILE_OLD);
        if (buildFileEntry == null) {
          return null;
        }
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
      try (InputStream zin = zip.getInputStream(buildFileEntry);
           InputStreamReader isr = new InputStreamReader(zin, StandardCharsets.UTF_8);
           BufferedReader br = new BufferedReader(isr)) {
        for (int i = 0; i < 10; i++) {
          final String s = br.readLine();
          if (s.indexOf(BUILDFILE_MODULE_ELEMENT1) > 0
              || s.indexOf(BUILDFILE_MODULE_ELEMENT2) > 0) {
            return new ModuleMetaData(zip);
          }
          else if (s.indexOf(BUILDFILE_EXTENSION_ELEMENT) > 0) {
            return new ExtensionMetaData(zip);
          }
        }
      }
    }
    catch (final ZipException e) {
      // It is not a Zip file, check for an Importable file
      return ImportAction.buildMetaData(file);
    }
    catch (final IOException e) {
      logger.error("", e);
    }

    return null;
  }
}
