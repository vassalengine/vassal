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

import javax.swing.JOptionPane;
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

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.BridgeStream;

/**
 * 
 * Class representing the metadata for a Save Game/Log File. Details
 * about the module this savegame was created with are saved in a
 * seperate moduledata file in the saved game zip.
 * 
 * @author Brent Easton
 * @since 3.1.0
 *
 */
public class SaveMetaData extends MetaData {

  public static final String ZIP_ENTRY_NAME = "savedata";
  public static final String DATA_VERSION = "1";

  public static final String COMMENTS_ELEMENT = "comments";

  protected String comments;
  protected ModuleMetaData moduleData;

  public SaveMetaData() {
    comments  = (String) JOptionPane.showInputDialog(
        GameModule.getGameModule().getFrame(),
        Resources.getString("BasicLogger.enter_comments"),
        Resources.getString("BasicLogger.log_file_comments"),
        JOptionPane.PLAIN_MESSAGE,
        null,
        null,
        comments);
  }
  
  public SaveMetaData(ZipFile zip) {
    read(zip);
  }
  
  public String getComments() {
    return comments;
  }
  
  public String getModuleName() {
    return moduleData == null ? "" : moduleData.getName();
  }
  
  public String getModuleVersion() {
    return moduleData == null ? "" : moduleData.getVersion();
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
      
      final Element root = doc.createElement(ROOT_ELEMENT);
      root.setAttribute(VERSION_ATTR, DATA_VERSION);
      doc.appendChild(root);

      final Element comments = doc.createElement(COMMENTS_ELEMENT);
      comments.appendChild(doc.createTextNode(getComments()));
      root.appendChild(comments);
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
    
    // Also save a copy of the current module metadata in the save file
    (new ModuleMetaData(GameModule.getGameModule())).save(archive);
  }

  /**
   * Read and validate a Saved Game/Log file.
   *  - Check it has a Zip Entry named savedgame
   *  - If it has a metadata file, read and parse it.
   *  - 
   * @param file Saved Game File
   */
  public void read(ZipFile zip) {
    comments = "";

    InputStream is = null;
    try {
      // Try to parse the metadata. Failure is not catastrophic, we can
      // treat it like an old-style save with no metadata.
      try {
        final ZipEntry data = zip.getEntry(SaveMetaData.ZIP_ENTRY_NAME);
        if (data == null) return;

        final XMLReader parser = XMLReaderFactory.createXMLReader();

        // set up the handler
        final XMLHandler handler = new XMLHandler();
        parser.setContentHandler(handler);
        parser.setDTDHandler(handler);
        parser.setEntityResolver(handler);
        parser.setErrorHandler(handler);

        // parse! parse!
        is = zip.getInputStream(data);
        parser.parse(new InputSource(is));
        
        // read the matching Module data
        moduleData = new ModuleMetaData(zip); 
      }
      catch (IOException e) {
        e.printStackTrace();
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

  private class XMLHandler extends DefaultHandler {
    final StringBuilder accumulator = new StringBuilder();
    String currentElement = null;

    @Override
    public void startElement(String uri, String localName,
                             String qName, Attributes attrs) {
      // clear the content accumulator
      accumulator.setLength(0);

      // handle element attributes we care about
/*
      else if (VASSAL_ELEMENT.equals(localName)) {
        vassalVersion = attrs.getName(VERSION_ATTR);
      }
*/
      currentElement = qName;
    }

//    private String getAttr(Attributes attrs, String qName) {
//      final String value = attrs.getValue(qName);
//      return value == null ? "" : value;
//    }

    @Override
    public void endElement(String uri, String localName, String qName) {
      // handle all of the elements which have CDATA here
      if (COMMENTS_ELEMENT.equals(qName)) {
        comments = accumulator.toString().trim();
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
}
