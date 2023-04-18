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

import VASSAL.build.GameModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 *
 * Class representing the metadata for a Module or an Extension. Modules
 * and extensions can't be differentiated until either the metadata or the
 * buildfile is parsed, so they share the same metadata structure.
 *
 * @author Brent Easton
 * @since 3.1.0
 *
 */
public class ModuleMetaData extends AbstractMetaData {

  private static final Logger logger =
    LoggerFactory.getLogger(ModuleMetaData.class);

  public static final String ZIP_ENTRY_NAME = "moduledata"; //NON-NLS
  public static final String DATA_VERSION = "1";

  protected Attribute nameAttr;

  public ModuleMetaData(ZipFile zip) {
    read(zip);
  }

  public ModuleMetaData(GameModule module) {
    super();
    nameAttr = new Attribute(module, GameModule.MODULE_NAME);
    setDescription(new Attribute(module, GameModule.DESCRIPTION));
    setVersion(module.getGameVersion());
    setExtra1(module.getModuleOther1());
    setExtra2(module.getModuleOther2());
  }

  public ModuleMetaData(String name, String version) {
    super();
    nameAttr = new Attribute(GameModule.MODULE_NAME, name);
    setVersion(version);
  }

  public String getName() {
    return nameAttr == null ? "" : nameAttr.getValue();
  }

  public String getLocalizedName() {
    return nameAttr == null ? "" : nameAttr.getLocalizedValue();
  }

  @Override
  public String getZipEntryName() {
    return ZIP_ENTRY_NAME;
  }

  @Override
  public String getMetaDataVersion() {
    return DATA_VERSION;
  }

  /**
   * Add elements specific to a ModuleMetaData
   *
   * @param doc Document
   * @param root Root element
   */
  @Override
  protected void addElements(Document doc, Element root) {
    nameAttr.generateXML(doc, root, NAME_ELEMENT);
  }

  /**
   * Read and validate a Module file.
   *  - Check it has a Zip Entry named buildfile
   *  - If it has a metadata file, read and parse it.
   *
   * Closes the {@link ZipFile}.
   *
   * @param zip Module File
   */
  public void read(ZipFile zip) {
    version = "";

    try (zip) {
      // Try to parse the metadata. Failure is not catastrophic, we can
      // treat it like an old-style module with no metadata and parse
      // the first lines of the buildFile
      final DefaultHandler handler;

      ZipEntry data = zip.getEntry(ZIP_ENTRY_NAME);
      if (data == null) {
        data = zip.getEntry(GameModule.BUILDFILE_OLD);
        if (data == null) return;

        handler = new ModuleBuildFileXMLHandler();
      }
      else {
        handler = new MetadataXMLHandler();
      }

      try (InputStream zin = zip.getInputStream(data);
           BufferedInputStream in = new BufferedInputStream(zin)) {
        synchronized (parser) {
          parser.setContentHandler(handler);
          parser.setDTDHandler(handler);
          parser.setEntityResolver(handler);
          parser.setErrorHandler(handler);
          parser.parse(new InputSource(in));
        }
      }
    }
    catch (final SAXEndException e) {
      // Indicates End of module/extension parsing. not an error.
    }
    catch (final IOException | SAXException e) {
      logger.error("", e);
    }
  }

  /**
   * XML Handler for parsing a Module/Extension metadata file
   */
  private class MetadataXMLHandler extends XMLHandler {

    @Override
    public void endElement(String uri, String localName, String qName) {
      // handle all of the elements which have CDATA here
      if (NAME_ELEMENT.equals(qName)) {
        if (nameAttr == null) {
          nameAttr = new Attribute(NAME_ELEMENT, accumulator.toString().trim());
        }
        else {
          // Modules saved prior to 3.6.11 have the language and the translation reversed
          if (isPreBug11929(getVassalVersion())) {
            nameAttr.addTranslation(accumulator.toString().trim(), language);
          }
          else {
            nameAttr.addTranslation(language, accumulator.toString().trim());
          }
        }
      }
      else {
        super.endElement(uri, localName, qName);
      }
    }
  }

  /**
   * XML Handle for parsing a Module buildFile. Used to read minimal data from
   * modules saved prior to 3.1.0.
   */
  private class ModuleBuildFileXMLHandler extends BuildFileXMLHandler {

    @Override
    public void startElement(String uri, String localName,
                             String qName, Attributes attrs)
        throws SAXEndException {
      super.startElement(uri, localName, qName, attrs);

      // handle element attributes we care about
      if (BUILDFILE_MODULE_ELEMENT1.equals(qName) ||
          BUILDFILE_MODULE_ELEMENT2.equals(qName)) {
        nameAttr = new Attribute(NAME_ELEMENT, getAttr(attrs, NAME_ATTR));
        setVersion(getAttr(attrs, VERSION_ATTR));
        setVassalVersion(getAttr(attrs, VASSAL_VERSION_ATTR));
        setDescription(getAttr(attrs, DESCRIPTION_ATTR));
        throw new SAXEndException();
      }
    }
  }
}
