/*
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
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;

/**
 * This class holds static convenience methods for building {@link Buildable} objects.
 */
public class Builder {

  private static final Logger logger = LoggerFactory.getLogger(Builder.class);

  private Builder() {
    // Helper class - not to be instantiated alone.
  }

  /**
   * General building algorithm.  For each sub-element of the build
   * Element, this method creates an instance of the class (which
   * must implement Buildable) whose name matches the XML element
   * tag name, builds that instance with the sub-element, and adds it
   * to the parent Buildable
   *
   * This algorithm calls a component's {@link Buildable#build} method
   * before calling its {@link Buildable#addTo} method
   *
   * @param parent the parent Buildable instance
   */
  public static void build(Element e, Buildable parent) {
    if (e == null) return;

    for (Node child = e.getFirstChild(); child != null;
         child = child.getNextSibling()) {
      if (Node.ELEMENT_NODE == child.getNodeType()) {
        try {
          final Buildable b = create((Element) child, parent);
          if (parent != null) {
            b.addTo(parent);
            parent.add(b);
          }
        }
        catch (IllegalBuildException ex) {
          ErrorDialog.bug(ex);
        }
        catch (RuntimeException | Error ex) {
          logger.error("Error building " + child.getNodeName()); //NON-NLS
          throw ex;
        }
      }
    }
  }

  public static Buildable create(Element e) throws IllegalBuildException {
    return create(e, null);
  }

    /**
     * Create an instance of a class from an XML element and build it.
     *
     * The <code>.class</code> file for the named class may be either
     * in the System's classpath or else within the {@link DataArchive}
     * of the {@link GameModule}.
     *
     * @throws IllegalBuildException if something goes wrong when loading
     * the class or creating an instance of it
     */
  public static Buildable create(Element e, Buildable parent) throws IllegalBuildException {
    final GameModule mod = GameModule.getGameModule();
    final String name = e.getTagName();

    try {
      final Buildable b = (Buildable) (mod == null ?  Class.forName(name) :
        mod.getDataArchive().loadClass(name)).getConstructor().newInstance();

      if (b instanceof AbstractBuildable) {
        ((AbstractBuildable)b).setAncestor(parent);
      }

      b.build(e);
      return b;
    }
    catch (Throwable t) {
      // find and rethrow causes which are not bugs
      ThrowableUtils.throwRecent(OutOfMemoryError.class, t);

      if (t instanceof ClassCastException ||
          t instanceof ClassNotFoundException ||
          t instanceof IllegalAccessException ||
          t instanceof IllegalArgumentException ||
          t instanceof InstantiationException ||
          t instanceof InvocationTargetException ||
          t instanceof NoSuchMethodException ||
          t instanceof SecurityException ||
          t instanceof ExceptionInInitializerError ||
          t instanceof LinkageError) {
        // one of the standard classloading problems occurred
        throw new IllegalBuildException("failed to load class " + name, t);
      }
      else if (t instanceof Error) {
        // some unusual problem occurred
        throw (Error) t;
      }
      else if (t instanceof RuntimeException) {
        // some unusual problem occurred
        throw (RuntimeException) t;
      }
      else {
        // this should never happen
        throw new IllegalStateException(t);
      }
    }
  }

  /**
   * Read an XML document from an InputStream
   * @return the XML document
   */
  public static Document createDocument(InputStream in)
                                        throws IOException {
    try (in) {
      return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
    }
    catch (ParserConfigurationException e) {
      ErrorDialog.bug(e);
      return null;
    }
    catch (SAXException e) {
      throw new IOException(e);
    }
  }

  /**
   * Create a new XML document
   * @return the Document
   */
  public static Document createNewDocument() {
    try {
      return DocumentBuilderFactory.newInstance()
                                   .newDocumentBuilder()
                                   .newDocument();
    }
    catch (ParserConfigurationException e) {
      ErrorDialog.bug(e);
      return null;
    }
  }

  /**
   * Write an XML document to a Writer
   * @param doc Document to be written
   * @param writer target XML Writer
   */
  public static void writeDocument(Document doc, Writer writer)
                                                          throws IOException {
    final Source source = new DOMSource(doc);

    // Prepare the output file
    final Result result = new StreamResult(writer);

    // Write the DOM document to the file
    try {
      final Transformer xformer =
        TransformerFactory.newInstance().newTransformer();
      xformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
      xformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4"); //$NON-NLS-1$ //$NON-NLS-2$
      xformer.transform(source, result);
    }
    catch (TransformerException e) {
      throw new IOException(e);
    }
  }

  /**
   * @return the decoded text contents of an Element node
   */
  public static String getText(Element e) {
    final StringBuilder buffer = new StringBuilder();
    final NodeList sub = e.getChildNodes();
    for (int i = 0; i < sub.getLength(); ++i) {
      if (sub.item(i).getNodeType() == Node.TEXT_NODE) {
        buffer.append(((org.w3c.dom.Text) sub.item(i)).getData());
      }
      else if (sub.item(i).getNodeType() == Node.ENTITY_REFERENCE_NODE) {
        buffer.append(sub.item(i).getFirstChild());
      }
    }
    return buffer.toString().trim();
  }

  /**
   * @return a String representation of an XML document
   */
  public static String toString(Document doc) {
    final StringWriter w = new StringWriter();
    try {
      writeDocument(doc, w);
      return w.toString();
    }
    // FIXME: review error message
    catch (IOException e) {
//      IOErrorDialog.error(e);
      return ""; //$NON-NLS-1$
    }
  }

  public static void main(String[] args) {
    final Document doc = createNewDocument();
    final Element e = doc.createElement("test"); //$NON-NLS-1$
    final Element e1 = doc.createElement("sub1"); //$NON-NLS-1$
    e.appendChild(e1);
    final Element e2 = doc.createElement("sub2"); //$NON-NLS-1$
    e2.setAttribute("one", "1"); //$NON-NLS-1$ //$NON-NLS-2$
    e2.setAttribute("two", "2"); //$NON-NLS-1$ //$NON-NLS-2$
    e.appendChild(e2);
    final Element e3 = doc.createElement("sub3"); //$NON-NLS-1$
    final Element e4 = doc.createElement("sub4"); //$NON-NLS-1$
    e4.appendChild(doc.createTextNode("4 > 2")); //$NON-NLS-1$
    e3.appendChild(e4);
    e.appendChild(e3);
    doc.appendChild(e);
    System.err.println(toString(doc));
    System.err.println("StringBuilder"); //$NON-NLS-1$
    final StringBuilder buf = new StringBuilder(300000);
    for (int i = 0; i < 500000; ++i) {
      buf.append("  "); //$NON-NLS-1$
      if (i % 10000 == 0) {
        System.err.println(buf.length()); //$NON-NLS-1$
      }
    }
  }
}
