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

import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import VASSAL.Info;
import VASSAL.build.GameModule;

public class ExtensionMetaData extends AbstractMetaData {

  public static final String ZIP_ENTRY_NAME = "extensiondata";
  public static final String DATA_VERSION = "1";

  protected static final String MODULE_NAME_ATTR = "moduleName";
  protected static final String MODULE_VERSION_ATTR = "moduleVersion";

  protected String moduleName;
  protected String moduleVersion;

  /**
   * Build an ExtensionMetaData for the given extension
   * 
   * @param ext
   *          Extension
   */
  public ExtensionMetaData(ModuleExtension ext) {
    setVersion(ext.getVersion());
    setVassalVersion(Info.getVersion());
    setDescription(new Attribute(ModuleExtension.DESCRIPTION, ext.getDescription()));
    moduleName = GameModule.getGameModule().getGameName();
    moduleVersion = GameModule.getGameModule().getGameVersion();
  }

  /**
   * Read Extension metadata from specified zip archive
   * 
   * @param zip
   *          Archive
   */
  public ExtensionMetaData(ZipFile zip) {
    read(zip);
  }

  public String getZipEntryName() {
    return ZIP_ENTRY_NAME;
  }
  
  public String getMetaDataVersion() {
    return DATA_VERSION;
  }
  
  /**
   * Add elements specific to an ExtensionMetaData 
   * 
   * @param doc Document
   * @param root Root element
   */
  protected void addElements(Document doc, Element root) {

      Element e = doc.createElement(MODULE_NAME_ELEMENT);
      e.appendChild(doc.createTextNode(moduleName));
      root.appendChild(e);

      e = doc.createElement(MODULE_VERSION_ELEMENT);
      e.appendChild(doc.createTextNode(moduleVersion));
      root.appendChild(e);
  }
  
  /**
   * Read and validate an Extension file.
   *  - Check it has a Zip Entry named buildfile
   *  - If it has a metadata file, read and parse it.
   *  
   * @param file Module File
   */
  public void read(ZipFile zip) {
    version = moduleName = moduleVersion = "";

    InputStream is = null;
    try {

      // Try to parse the metadata. Failure is not catastrophic, we can
      // treat it like an old-style module with no metadata and parse
      // the first lines of the buildFile
      try {
        final XMLReader parser = XMLReaderFactory.createXMLReader();
        DefaultHandler handler = null;
        ZipEntry data = zip.getEntry(ZIP_ENTRY_NAME);
        if (data == null) {
          data = zip.getEntry(GameModule.BUILDFILE);
          handler = new ExtensionBuildFileXMLHandler();
        }
        else {
          handler = new MetadataXMLHandler();
        }
        
        parser.setContentHandler(handler);
        parser.setDTDHandler(handler);
        parser.setEntityResolver(handler);
        parser.setErrorHandler(handler);

        // parse! parse!
        is = zip.getInputStream(data);
        parser.parse(new InputSource(is));
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      catch (SAXEndException e) {
        // Indicates End of module/extension parsing. not an error.
      }
      catch (SAXException e) {
        e.printStackTrace();
      }
    }
    finally {
      if (zip != null) {
        try {
          zip.close();
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
      
      if (is != null) {
        try {
          is.close();
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
  }
  
  /**
   * XML Handler for parsing an Extension metadata file
   */
  private class MetadataXMLHandler extends XMLHandler {
    
    @Override
    public void endElement(String uri, String localName, String qName) {
      // handle all of the elements which have CDATA here
      if (MODULE_NAME_ELEMENT.equals(qName)) {
         moduleName = accumulator.toString().trim();
      }
      else if (MODULE_VERSION_ELEMENT.equals(qName)) {
         moduleVersion = accumulator.toString().trim();
      }
      else {
        super.endElement(uri, localName, qName);
      }
    }
  }
  
  /**
   * XML Handle for parsing an extension buildFile. Used to read minimal data from
   * extensions saved prior to 3.1.0. 
   */
  private class ExtensionBuildFileXMLHandler extends BuildFileXMLHandler {

    @Override
    public void startElement(String uri, String localName,
                             String qName, Attributes attrs) 
        throws SAXEndException {
      super.startElement(uri, localName, qName, attrs);

      // handle element attributes we care about
      if (BUILDFILE_EXTENSION_ELEMENT.equals(qName)) {
        version = getAttr(attrs, VERSION_ATTR);
        vassalVersion  = getAttr(attrs, VASSAL_VERSION_ATTR);
        moduleName = getAttr(attrs, MODULE_NAME_ATTR);
        moduleVersion = getAttr(attrs, MODULE_VERSION_ATTR);
        throw new SAXEndException();
      }
    }
  }
}