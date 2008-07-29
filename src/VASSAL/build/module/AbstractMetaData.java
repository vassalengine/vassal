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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Locale;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
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
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import VASSAL.Info;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.i18n.Translation;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.BridgeStream;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.IOUtils;

/**
 * 
 * Base class representing the metadata for a Saved Game, Module or Extension. 
 * 
 * @author Brent Easton
 * @since 3.1.0
 */
public abstract class AbstractMetaData {

  protected static final String TRUE = "true";
  protected static final String FALSE = "false";
  
  protected static final String NAME_ATTR = "name";
  protected static final String VERSION_ATTR = "version";
  protected static final String VASSAL_VERSION_ATTR = "vassalVersion";
  protected static final String DESCRIPTION_ATTR = "description";
  protected static final String EXTENSION_ATTR = "extension";
  protected static final String MODULE_NAME_ATTR = "moduleName";
  protected static final String MODULE_VERSION_ATTR = "moduleVersion";
  protected static final String LANG_ATTR = "lang";


  protected static final String ROOT_ELEMENT = "data";
  protected static final String VERSION_ELEMENT = "version";
  protected static final String VASSAL_VERSION_ELEMENT = "VassalVersion";
  protected static final String MODULE_NAME_ELEMENT = "moduleName";
  protected static final String MODULE_VERSION_ELEMENT = "moduleVersion";
  protected static final String DESCRIPTION_ELEMENT = "description";
  protected static final String NAME_ELEMENT = "name";
  protected static final String DATE_SAVED_ELEMENT = "dateSaved";
  
  protected static final String BUILDFILE_MODULE_ELEMENT1 = "VASSAL.launch.BasicModule";
  protected static final String BUILDFILE_MODULE_ELEMENT2 = "VASSAL.build.GameModule";
  protected static final String BUILDFILE_EXTENSION_ELEMENT = "VASSAL.build.module.ModuleExtension";
  
  protected String version;
  protected String vassalVersion;
  protected Attribute descriptionAttr;
 
  public AbstractMetaData() {
    setVassalVersion(Info.getVersion());
  }
 
  public String getVersion() {
    return version == null ? "" : version;
  }
  
  public void setVersion(String s) {
    version = s;
  }
  
  public String getVassalVersion() {
    return vassalVersion == null ? "" : vassalVersion;
  }
  
  public void setVassalVersion(String s) {
    vassalVersion = s;
  }
  
  public void setDescription(Attribute desc) {
    descriptionAttr = desc;
  }
 
  public void setDescription(String desc) {
    descriptionAttr = new Attribute(GameModule.DESCRIPTION, desc);
  }
 
  public String getDescription() {
    return descriptionAttr == null ? "" : descriptionAttr.getValue();
  }

  public String getLocalizedDescription() {
    return descriptionAttr == null ? "" : descriptionAttr.getLocalizedValue();
  }

  public enum FileType { MODULE, EXTENSION, SAVE, UNKNOWN }

  public static FileType getFileType(String filename) {
    return getFileType(new File(filename));
  }
 
  public static FileType getFileType(File file) {
    final AbstractMetaData data = AbstractMetaData.buildMetaData(file);

    if (data == null) {
      // Not a Module, Extension or Saved game of any type, old or new.
      return FileType.UNKNOWN;
    }
    else if (data instanceof SaveMetaData) {
      return FileType.SAVE;
    }
    else if (data instanceof ExtensionMetaData) {
      return FileType.EXTENSION;
    }
    else if (data instanceof ModuleMetaData) {
      return FileType.MODULE;
    }
    else {
      // Some other type of metadata we don't know about.
      // This should not happen, they should all be listed here.
      throw new IllegalStateException();
    }
  }

  /**
   * Factory method to build and return an appropriate MetaData class based 
   * on the contents of the file. Return null if the file is not a Zip archive,
   * or it is not a VASSAL Module, Extension or Save Game. 
   * 
   * @param file metadata file
   * @return MetaData object
   */
  public static AbstractMetaData buildMetaData(File file) {
    // Check the file exists and is a file
    if (!file.exists() || !file.isFile()) return null;
    
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
        br = new BufferedReader(
          new InputStreamReader(zip.getInputStream(buildFileEntry)));
        for (int i = 0; i < 10; i++) {
          final String s = br.readLine();
          if (s.indexOf(BUILDFILE_MODULE_ELEMENT1) > 0 ||
              s.indexOf(BUILDFILE_MODULE_ELEMENT2) > 0) {
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
      // Ignore. This (usually) means that the module has no metadata.
    }
    catch (IOException e) {
      ErrorLog.log(e);
    } 
    finally {
      IOUtils.closeQuietly(zip);
    }    

    return null;
  }
  
  /**
   * Write common metadata to the specified Archive. Call addElements to
   * add elements specific to particular concrete subclasses.
   * 
   * @param archive
   *          Extension Archive
   * @throws IOException
   *           If anything goes wrong
   */
  public void save(ArchiveWriter archive) throws IOException {
    Document doc = null;
    Element e = null;
    try {
      doc = DocumentBuilderFactory.newInstance()
                                  .newDocumentBuilder()
                                  .newDocument();

      final Element root = doc.createElement(ROOT_ELEMENT);
      root.setAttribute(VERSION_ATTR, getMetaDataVersion());
      doc.appendChild(root);

      if (getVersion() != null) { 
        e = doc.createElement(VERSION_ELEMENT);
        e.appendChild(doc.createTextNode(getVersion()));
        root.appendChild(e);
      }

      if (getVassalVersion() != null) {
        e = doc.createElement(VASSAL_VERSION_ELEMENT);
        e.appendChild(doc.createTextNode(getVassalVersion()));
        root.appendChild(e);
      }
      
      e = doc.createElement(DATE_SAVED_ELEMENT);
      e.appendChild(doc.createTextNode(
        String.valueOf(System.currentTimeMillis())));
      root.appendChild(e);
      
      if (descriptionAttr != null) {
        descriptionAttr.generateXML(doc, root, DESCRIPTION_ELEMENT);
      }
      
      addElements(doc, root);

    }
    catch (ParserConfigurationException ex) {
      ErrorDialog.bug(ex);
      // FIXME: switch to IOException(Throwable) ctor in Java 1.6
      throw (IOException) new IOException().initCause(ex);
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
      ErrorDialog.bug(ex);
      // FIXME: switch to IOException(Throwable) ctor in Java 1.6
      throw (IOException) new IOException().initCause(ex);
    }
    catch (TransformerFactoryConfigurationError ex) {
      ErrorDialog.bug(ex);
      // FIXME: switch to IOException(Throwable) ctor in Java 1.6
      throw (IOException) new IOException().initCause(ex);
    }
    catch (TransformerException ex) {
      // FIXME: switch to IOException(Throwable) ctor in Java 1.6
      throw (IOException) new IOException().initCause(ex);
    }

    archive.addFile(getZipEntryName(), out.toInputStream());
  }

  /**
   * Copy the Module metatdata from the current module into the specified
   * archive.
   *
   * @param archive Archive to copy into
   * @throws IOException
   */
  public void copyModuleMetadata(ArchiveWriter archive) throws IOException {
    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(
        GameModule.getGameModule()
                  .getDataArchive()
                  .getFileStream(ModuleMetaData.ZIP_ENTRY_NAME));
      archive.addFile(ModuleMetaData.ZIP_ENTRY_NAME, in);
      in.close();
    }
    catch (FileNotFoundException e) {
      // No Metatdata in source module, create a fresh copy 
      new ModuleMetaData(GameModule.getGameModule()).save(archive);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }
  
  /**
   * Return the Entry name for the metatdata file
   * 
   * @return Zip Entry name
   */
  public abstract String getZipEntryName();
  
  /**
   * Return the version of the metadata structure
   * 
   * @return version
   */
  public abstract String getMetaDataVersion();
  
  /**
   * Add elements specific to a MetaData subclass
   * 
   * @param doc Document
   * @param root Root element
   */
  protected abstract void addElements(Document doc, Element root);
  
  /*************************************************************************
   * Utility Exception class, used to cancel SAX parsing
   *
   */
  class SAXEndException extends SAXException {
    private static final long serialVersionUID = 1L; 
  }
  
  /*************************************************************************
   * Utility class representing a Configurable attribute and its translations
   * 
   */
  class Attribute {
    protected String attributeName;
    protected String value;
    protected HashMap<String, String> translations =
      new HashMap<String, String>();
    
    /**
     * Build Attribute class based on atrribute value and translations
     * available in the current module
     * 
     * @param c Target configurable
     * @param name Attribute name
     */
    public Attribute(Configurable target, String name) {
      attributeName = name;
      value = target.getAttributeValueString(attributeName);
      String key = target.getI18nData().getFullPrefix();
      if (key.length()> 0) key += ".";
      key += attributeName;
      
      for (Translation t : GameModule.getGameModule().getAllDescendantComponentsOf(Translation.class)) {
        addTranslation(t.getLanguageCode(), t.translate(key));
      }
    }   
    
    public Attribute (String attributeName, String value) {
      this.attributeName = attributeName;
      this.value = value;
    }
    
    public void addTranslation(String language, String value) {
      if (value != null) {
        translations.put(language, value);
      }
    }
    
    /**
     * Return the untranslated value of this attribute
     * 
     * @return value
     */
    public String getValue() {
      return value;
    }
    
    /**
     * Return the value of this attribute translated into the local
     * language
     * 
     * @return translated value
     */
    public String getLocalizedValue() {
      String lang = Locale.getDefault().getLanguage();
      String tx = translations.get(lang);
      return tx == null ? getValue() : tx;
    }
    
    /**
     * Output metadata XML for this attribute, including translations
     * 
     * @param doc Root document
     * @param root Parent element
     * @param prefix Attribute prefix
     */
    public void generateXML(Document doc, Element root, String prefix) {
      
      if (value == null) {
        return;
      }
      
      Element e = null;
      
      e = doc.createElement(prefix);
      e.appendChild(doc.createTextNode(value));
      root.appendChild(e);
      
      for (String lang : translations.keySet()) {
        String tx = translations.get(lang);
        e = doc.createElement(prefix);
        e.setAttribute(LANG_ATTR, lang);
        e.appendChild(doc.createTextNode(tx));
        root.appendChild(e);
      }
    }
  }
  
  /*************************************************************************
   * Base XML Handler for all metadata classes
   *
   */
  class XMLHandler extends DefaultHandler {
    final StringBuilder accumulator = new StringBuilder();
    protected String language = "";
    
    @Override
    public void startElement(String uri, String localName,
                             String qName, Attributes attrs) {
      // clear the content accumulator
      accumulator.setLength(0);

      // Track language for localizable attributes
      language = getAttr(attrs, LANG_ATTR); 
    }
    
    protected String getAttr(Attributes attrs, String qName) {
      final String value = attrs.getValue(qName);
      return value == null ? "" : value;
    }
    
    public void endElement(String uri, String localName, String qName) {
      // handle all of the elements which have CDATA here
      
      String value = accumulator.toString().trim();
      
      if (VERSION_ELEMENT.equals(qName)) {
         setVersion(value);
      }
      else if (VASSAL_VERSION_ELEMENT.equals(qName)) {
        setVassalVersion(value);
      }
      else if (DESCRIPTION_ELEMENT.equals(qName)) {
        if (descriptionAttr == null) {
          setDescription(new Attribute(DESCRIPTION_ELEMENT, value));
        }
        else {
          descriptionAttr.addTranslation(language, value);
        }
      }
    }
    
    @Override
    public void characters(char[] ch, int start, int length) {
      accumulator.append(ch, start, length);
    }

    @Override
    public void warning(SAXParseException e) throws SAXException {
      ErrorLog.log(e);
    }

    @Override
    public void error(SAXParseException e) throws SAXException {
      ErrorLog.log(e);
    }

    @Override
    public void fatalError(SAXParseException e) throws SAXException {
      throw e;
    }
    
  }
  
  /*************************************************************************
   * XML Handler for parsing a buildFile. Used to read minimal data from
   * modules saved prior to 3.1.0. 
   */
  class BuildFileXMLHandler extends DefaultHandler {
    final StringBuilder accumulator = new StringBuilder();

    @Override
    public void startElement(String uri, String localName,
                             String qName, Attributes attrs) 
        throws SAXEndException {
      // clear the content accumulator
      accumulator.setLength(0);
    }

    protected String getAttr(Attributes attrs, String qName) {
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
      ErrorLog.log(e);
    }

    @Override
    public void error(SAXParseException e) throws SAXException {
      ErrorLog.log(e);
    }

    @Override
    public void fatalError(SAXParseException e) throws SAXException {
      throw e;
    }
  }
}
