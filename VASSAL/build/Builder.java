/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import javax.swing.JOptionPane;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;

/**
 * This class holds static convenience methods for building {@link Buildable}
 * objects
 */
public abstract class Builder {
  /**
   * General building algorithm.  For each subelement of the build
   * Element, this method creates an instance of the class (which
   * must implement Buildable) whose name matches the XML element
   * tag name, builds that instance with the subelement, and adds it
   * to the parent Buildable
   *
   * This algorithm calls a component's {@link Buildable#build} method
   * before calling its {@link Buildable#addTo} method
   *
   * @param parent the parent Buildable instance
   */
  public static void build(Element e, Buildable parent) {
    if (e != null) {
      for (Node child = e.getFirstChild(); child != null;
           child = child.getNextSibling()) {
        if (Node.ELEMENT_NODE == child.getNodeType()) {
          try {
            Buildable b = create((Element) child);
            if (parent != null) {
              b.addTo(parent);
              parent.add(b);
            }
          }
          catch (Throwable err) {
            String msg = err.getMessage();
            if (msg == null) {
              msg = err.getClass().getName().substring(err.getClass().getName().lastIndexOf(".") + 1); //$NON-NLS-1$
            }
            System.err.println(child.toString());
            err.printStackTrace();
            JOptionPane.showMessageDialog
              (null,
               Resources.getString("Builder.create_error",  //$NON-NLS-1$
                   new String[] {((Element) child).getTagName(),
                                 GameModule.getGameModule().getDataArchive().getName(),
                                 msg }), 
               Resources.getString("Builder.error"), //$NON-NLS-1$
               JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    }
  }

  /**
   * Create an instance of a class from an XML element and build it
   *
   * The <code>.class</code> file for the named class may be either
   * in the System's classpath or else within the {@link DataArchive}
   * of the {@link GameModule}
   */
  public static Buildable create(Element e) throws Exception {
    Buildable b;
    if (GameModule.getGameModule() == null) {
      b = (Buildable) Class.forName(e.getTagName()).newInstance();
    }
    else {
      b = (Buildable) GameModule
        .getGameModule().getDataArchive()
        .loadClass(e.getTagName()).newInstance();
    }
    b.build(e);
    return b;
  }

  /**
   * Read an XML document from an InputStream
   */
  public static Document createDocument
    (java.io.InputStream inStream) throws java.io.IOException {
    try {
      return javax.xml.parsers.DocumentBuilderFactory.newInstance()
        .newDocumentBuilder().parse(inStream);
    }
    catch (Exception e) {
      throw new java.io.IOException(e.getMessage());
    }
  }

  /**
   * Create a new XML document
   */
  public static Document createNewDocument() {
    try {
      return javax.xml.parsers.DocumentBuilderFactory.newInstance()
        .newDocumentBuilder().newDocument();
    }
    catch (Exception ex) {
      return null;
    }
  }

  /**
   * Write an XML document to a Writer
   */
  public static void writeDocument(Document doc, Writer writer) throws IOException {
    try {
      Source source = new DOMSource(doc);

      // Prepare the output file
      Result result = new StreamResult(writer);

      // Write the DOM document to the file
      Transformer xformer = TransformerFactory.newInstance().newTransformer();
      xformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
      xformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4"); //$NON-NLS-1$ //$NON-NLS-2$
      xformer.transform(source, result);
    }
    catch (TransformerException e) {
      throw new IOException(e.getMessage());
    }
    catch (TransformerFactoryConfigurationError e) {
      throw new IOException(e.getMessage());
    }
  }

  /**
   * Return the decoded text contents of an Element node
   */
  public static String getText(Element e) {
    StringBuffer buffer = new StringBuffer();
    org.w3c.dom.NodeList sub = e.getChildNodes();
    for (int i = 0; i < sub.getLength(); ++i) {
      if (sub.item(i).getNodeType()
        == Node.TEXT_NODE) {
        buffer.append(((org.w3c.dom.Text) sub.item(i)).getData());
      }
      else if (sub.item(i).getNodeType()
        == Node.ENTITY_REFERENCE_NODE) {
        buffer.append(sub.item(i).getFirstChild().toString());
      }
    }
    return buffer.toString().trim();
  }

  /**
   * @return a String representation of an XML Node
   */
  public static String toString(Document doc) {
    StringWriter w = new StringWriter();
    try {
      writeDocument(doc,w);
      return w.toString();
    }
    catch (IOException e) {
      e.printStackTrace();
      return ""; //$NON-NLS-1$
    }
  }

  public static void main(String args[]) {
    Document doc = createNewDocument();
    Element e = doc.createElement("test"); //$NON-NLS-1$
    Element e1 = doc.createElement("sub1"); //$NON-NLS-1$
    e.appendChild(e1);
    Element e2 = doc.createElement("sub2"); //$NON-NLS-1$
    e2.setAttribute("one", "1"); //$NON-NLS-1$ //$NON-NLS-2$
    e2.setAttribute("two", "2"); //$NON-NLS-1$ //$NON-NLS-2$
    e.appendChild(e2);
    Element e3 = doc.createElement("sub3"); //$NON-NLS-1$
    Element e4 = doc.createElement("sub4"); //$NON-NLS-1$
    e4.appendChild(doc.createTextNode("4 > 2")); //$NON-NLS-1$
    e3.appendChild(e4);
    e.appendChild(e3);
    doc.appendChild(e);
    System.err.println(toString(doc));
    System.err.println("StringBuffer"); //$NON-NLS-1$
    StringBuffer buf = new StringBuffer(300000);
    for (int i = 0; i < 500000; ++i) {
      buf.append("  "); //$NON-NLS-1$
      if (i % 10000 == 0) {
        System.err.println("" + buf.length()); //$NON-NLS-1$
      }
    }
  }
}




