/*
 * $Id$
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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.JOptionPane;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.IOUtils;

/**
 * Class representing the metadata for a Save Game/Log File. Details
 * about the module this savegame was created with are saved in a
 * seperate moduledata file in the saved game zip.
 *
 * @author Brent Easton
 * @since 3.1.0
 */
public class SaveMetaData extends AbstractMetaData {

  private static final Logger logger =
    LoggerFactory.getLogger(SaveMetaData.class);

  public static final String ZIP_ENTRY_NAME = "savedata";
  public static final String DATA_VERSION = "1";
  public static final String PROMPT_LOG_COMMENT = "promptLogComment";

  protected ModuleMetaData moduleData;

  public SaveMetaData() {
    super();

    String comments = "";
    if ( (Boolean)GameModule.getGameModule().getPrefs().getValue( PROMPT_LOG_COMMENT )){
      comments = (String) JOptionPane.showInputDialog(
        GameModule.getGameModule().getFrame(),
        Resources.getString("BasicLogger.enter_comments"),
        Resources.getString("BasicLogger.log_file_comments"),
        JOptionPane.PLAIN_MESSAGE,
        null,
        null,
        ""
      );

      setDescription(new Attribute(DESCRIPTION_ELEMENT, comments));
    }
  }

  public SaveMetaData(ZipFile zip) {
    read(zip);
  }

  public String getModuleName() {
    return moduleData == null ? "" : moduleData.getName();
  }

  public String getModuleVersion() {
    return moduleData == null ? "" : moduleData.getVersion();
  }

  public ModuleMetaData getModuleData() {
    return moduleData;
  }

  public String getZipEntryName() {
    return ZIP_ENTRY_NAME;
  }

  public String getMetaDataVersion() {
    return DATA_VERSION;
  }

  /**
   * Write Save Game metadata to the specified Archive
   * @param archive Save game Archive
   * @throws IOException If anything goes wrong
   */
  public void save(FileArchive archive) throws IOException {
    super.save(archive);

    // Also save a copy of the current module metadata in the save file. Copy
    // module metadata from the module archive as it will contain full i18n
    // information.
    copyModuleMetadata(archive);
  }

  /**
   * Add Elements specific to SaveMetaData
   */
  protected void addElements(Document doc, Element root) {
    return;
  }

  /**
   * Read and validate a Saved Game/Log file.
   * Check that it has a Zip Entry named savedgame.
   * If it has a metadata file, read and parse it.
   *
   * @param file Saved Game File
   */
  public void read(ZipFile zip) {
    try {
      // Try to parse the metadata. Failure is not catastrophic, we can
      // treat it like an old-style save with no metadata.
      final ZipEntry data = zip.getEntry(getZipEntryName());
      if (data == null) return;

      // set up the handler
      final XMLHandler handler = new XMLHandler();

      // parse! parse!
      BufferedInputStream in = null;
      try {
        in = new BufferedInputStream(zip.getInputStream(data));

        synchronized (parser) {
          parser.setContentHandler(handler);
          parser.setDTDHandler(handler);
          parser.setEntityResolver(handler);
          parser.setErrorHandler(handler);
          parser.parse(new InputSource(in));
        }

        in.close();
      }
      finally {
        IOUtils.closeQuietly(in);
      }

      // read the matching Module data
      moduleData = new ModuleMetaData(zip);

      zip.close();
    }
    catch (IOException e) {
      logger.error("", e);
    }
    catch (SAXException e) {
      logger.error("", e);
    }
    finally {
      IOUtils.closeQuietly(zip);
    }
  }
}
