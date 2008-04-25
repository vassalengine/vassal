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
package VASSAL.build.module;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import org.xml.sax.SAXException;

import VASSAL.build.GameModule;

/**
 * 
 * Base Class representing the metadata for a Saved Game, Module or Extension. 
 * A Base MetaData class is returned only for invalid files.
 * 
 * @author Brent Easton
 * @since 3.1.0
 *
 */
public class MetaData {

  public static final String ZIP_ENTRY_NAME = "metadata";
  public static final String DATA_VERSION = "1";

  protected static final String TRUE = "true";
  protected static final String FALSE = "false";
  
  protected static final String NAME_ATTR = "name";
  protected static final String VERSION_ATTR = "version";
  protected static final String VASSAL_VERSION_ATTR = "vassalVersion";
  protected static final String EXTENSION_ATTR = "extension";
  protected static final String MODULE_NAME_ATTR = "moduleName";
  protected static final String MODULE_VERSION_ATTR = "moduleVersion";


  protected static final String ROOT_ELEMENT = "data";
  protected static final String VERSION_ELEMENT = "version";
  protected static final String VASSAL_VERSION_ELEMENT = "VassalVersion";
  protected static final String MODULE_NAME_ELEMENT = "moduleName";
  protected static final String MODULE_VERSION_ELEMENT = "moduleVersion";
  protected static final String DESCRIPTION_ELEMENT = "description";
  protected static final String NAME_ELEMENT = "name";
  
  protected static final String BUILDFILE_MODULE_ELEMENT1 = "VASSAL.launch.BasicModule";
  protected static final String BUILDFILE_MODULE_ELEMENT2 = "VASSAL.build.GameModule";
  protected static final String BUILDFILE_EXTENSION_ELEMENT = "VASSAL.build.module.ModuleExtension";
  
  protected String version;
  protected String vassalVersion;
  protected String description;
  
  public String getVersion() {
    return version;
  }
  
  public String getVassalVersion() {
    return vassalVersion;
  }
  
  public String getDescription() {
    return description;
  }
  
  /**
   * Build and return an appropriate MetaData class based on the
   * contents of the file. 
   * 
   * @param file metadata file
   * @return MetaData object
   */
  public static MetaData buildMetaData(File file) {
    
    // Check the file exists and is a file
    if (!file.exists() || !file.isFile()) {
      return null;
    }
    
    // Check it is a Zip file
    ZipFile zip = null;
    
    try {
      zip = new ZipFile(file);
    }
    catch (ZipException e) {
      return null;
    }
    catch (IOException e) {
      return null;
    } 

    // Check if it is a Save Game file
    ZipEntry entry = zip.getEntry(GameState.SAVEFILE_ZIP_ENTRY);
    if (entry != null) {
      return new SaveMetaData(zip);
    }
    
    // Check if it has a buildFile
    ZipEntry buildFileEntry = zip.getEntry(GameModule.BUILDFILE);
    if (buildFileEntry == null) {
      closeZip(zip);
      return null;
    }
    
    // It's either a module or an Extension, check for existence of
    // metadata
    entry = zip.getEntry(ModuleMetaData.ZIP_ENTRY_NAME);
    if (entry != null) {
      return new ModuleMetaData(zip);
    }
    
    entry = zip.getEntry(ExtensionMetaData.ZIP_ENTRY_NAME);
    if (entry != null) {
      return new ExtensionMetaData(zip);
    }
    
    // read the first few lines of the buildFile    
    BufferedReader br = null;
    try {
            
      br = new BufferedReader(new InputStreamReader(zip.getInputStream(buildFileEntry)));
      for (int i = 0; i < 10; i++) {
        String s = br.readLine();
        if (s.indexOf(BUILDFILE_MODULE_ELEMENT1) > 0 || s.indexOf(BUILDFILE_MODULE_ELEMENT2) > 0) {
          br.close();
          return new ModuleMetaData(zip);
        }  
        else if (s.indexOf(BUILDFILE_EXTENSION_ELEMENT) > 0) {
          br.close();
          return new ExtensionMetaData(zip);
        }
      }
    }
    catch (IOException e) {
      
    }
    finally {
      if (br != null) {
        try {
          br.close();
        }
        catch (IOException e) {
          // No stacktrace
        }
      }
    }
    
    closeZip(zip);
    
    return null;
  }
  
  // Quietly close a zip file, throw no errors.
  private static void closeZip(ZipFile zip) {
    try {
      zip.close();
    }
    catch (IOException e) {
      // No stack trace
    }
    return;
  }
  
  /**
   * Utility Exception class, used to cancel SAX parsing
   *
   */
  class SAXEndException extends SAXException {
    private static final long serialVersionUID = 1L;
    
  }
}
