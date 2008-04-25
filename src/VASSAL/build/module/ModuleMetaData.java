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
package VASSAL.build.module;

import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.BridgeStream;

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
public class ModuleMetaData extends MetaData {

  public static final String ZIP_ENTRY_NAME = "moduledata";
  public static final String DATA_VERSION = "1";
    
  protected String name;

  public ModuleMetaData(ZipFile zip) {
    read(zip);
  }
  
  public ModuleMetaData(GameModule module) {
    name = GameModule.getGameModule().getGameName();
    version = GameModule.getGameModule().getGameVersion();
    vassalVersion = Info.getVersion();
    description = GameModule.getGameModule().getAttributeValueString(GameModule.DESCRIPTION);
  }

  public String getName() {
    return name;
  }

  /**
   * Write Save Game metadata to the specified Archive
   * @param archive Save game Archive
   * @throws IOException If anything goes wrong
   */
  public void save(ArchiveWriter archive) throws IOException {
    Document doc = null;
    try {
      doc = DocumentBuilderFactory.newInstance()
                                  .newDocumentBuilder()
                                  .newDocument();
      
      final Element rootEl = doc.createElement(ROOT_ELEMENT);
      rootEl.setAttribute(VERSION_ATTR, DATA_VERSION);
      doc.appendChild(rootEl);
      
      Element e = doc.createElement(NAME_ELEMENT);
      e.appendChild(doc.createTextNode(getName()));
      rootEl.appendChild(e);
      // FIXME: Extend to include any translations of the name

      e = doc.createElement(VERSION_ELEMENT);
      e.appendChild(doc.createTextNode(getVersion()));
      rootEl.appendChild(e);

      e = doc.createElement(VASSAL_VERSION_ELEMENT);
      e.appendChild(doc.createTextNode(getVassalVersion()));
      rootEl.appendChild(e);
      
      e = doc.createElement(DESCRIPTION_ELEMENT);
      e.appendChild(doc.createTextNode(getDescription()));
      rootEl.appendChild(e);
      // FIXME: Extend to include any translations of the description
      
    }
    catch (ParserConfigurationException ex) {
      throw new IOException(ex.getMessage());
    }

    final BridgeStream out = new BridgeStream();
    try {
      final Transformer xformer =
        TransformerFactory.newInstance().newTransformer();
      xformer.setOutputProperty(OutputKeys.INDENT, "yes");
      xformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount",
          "2");
      xformer.transform(new DOMSource(doc), new StreamResult(out));
    }
    catch (TransformerConfigurationException ex) {
      throw new IOException(ex.getMessage());
    }
    catch (TransformerFactoryConfigurationError ex) {
      throw new IOException(ex.getMessage());
    }
    catch (TransformerException ex) {
      throw new IOException(ex.getMessage());
    }

    archive.addFile(ZIP_ENTRY_NAME, out.toInputStream());
  }

  /**
   * Read and validate a Module file.
   *  - Check it has a Zip Entry named buildfile
   *  - If it has a metadata file, read and parse it.
   *  
   * @param file Module File
   */
  public void read(ZipFile zip) {
    description = name = version = "";

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
          handler = new BuildFileXMLHandler();
        }
        else {
          handler = new MetadataXMLHandler();
        }
        
        if (data == null) {
          return;
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
   * XML Handler for parsing a Module/Extension metadata file
   */
  private class MetadataXMLHandler extends DefaultHandler {
    final StringBuilder accumulator = new StringBuilder();

    @Override
    public void startElement(String uri, String localName,
                             String qName, Attributes attrs) {
      // clear the content accumulator
      accumulator.setLength(0);

      // handle element attributes we care about
    }

    @Override
    public void endElement(String uri, String localName, String qName) {
      // handle all of the elements which have CDATA here
      if (NAME_ELEMENT.equals(qName)) {
         name = accumulator.toString().trim();
      }
      else if (VERSION_ELEMENT.equals(qName)) {
         version = accumulator.toString().trim();
      }
      if (DESCRIPTION_ELEMENT.equals(qName)) {
        description = accumulator.toString().trim();
      }
    }

    @Override
    public void characters(char[] ch, int start, int length) {
      accumulator.append(ch, start, length);
    }

    @Override
    public void warning(SAXParseException e) throws SAXException {
      e.printStackTrace();
    }

    @Override
    public void error(SAXParseException e) throws SAXException {
      e.printStackTrace();
    }

    @Override
    public void fatalError(SAXParseException e) throws SAXException {
      throw e;
    }
  }
  
  /**
   * XML Handle for parsing a buildFile. Used to read minimal data from
   * modules saved prior to 3.1.0. 
   */
  private class BuildFileXMLHandler extends DefaultHandler {
    final StringBuilder accumulator = new StringBuilder();

    @Override
    public void startElement(String uri, String localName,
                             String qName, Attributes attrs) 
        throws SAXEndException {
      // clear the content accumulator
      accumulator.setLength(0);

      // handle element attributes we care about
      if (BUILDFILE_MODULE_ELEMENT1.equals(qName) || BUILDFILE_MODULE_ELEMENT2.equals(qName)) {
        name = getAttr(attrs, NAME_ATTR);
        version = getAttr(attrs, VERSION_ATTR);
        vassalVersion  = getAttr(attrs, VASSAL_VERSION_ATTR);
        throw new SAXEndException();
      }
    }

    private String getAttr(Attributes attrs, String qName) {
      final String value = attrs.getValue(qName);
      return value == null ? "" : value;
    }
    
    @Override
    public void endElement(String uri, String localName, String qName) {
      // handle all of the elements which have CDATA here
    }

    @Override
    public void characters(char[] ch, int start, int length) {
      accumulator.append(ch, start, length);
    }

    @Override
    public void warning(SAXParseException e) throws SAXException {
      e.printStackTrace();
    }

    @Override
    public void error(SAXParseException e) throws SAXException {
      e.printStackTrace();
    }

    @Override
    public void fatalError(SAXParseException e) throws SAXException {
      throw e;
    }
  }

}
