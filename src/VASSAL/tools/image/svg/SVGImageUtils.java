/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
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

package VASSAL.tools.image.svg;

import java.awt.Dimension;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.apache.batik.dom.GenericDOMImplementation;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.dom.util.DOMUtilities;
import org.apache.batik.dom.util.SAXDocumentFactory;
import org.apache.batik.dom.util.XLinkSupport;
import org.apache.batik.dom.util.XMLSupport;
import org.apache.batik.util.XMLResourceDescriptor;

import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;
import VASSAL.tools.io.IOUtils;

/**
 * Utility methods for manipulating SVG images.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class SVGImageUtils {
  // NB: SAXSVGDocumentFactory isn't thread-safe, we have to synchronize on it.
  protected static final SAXSVGDocumentFactory factory =
    new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName());

  private SVGImageUtils() { }

  /**
   * Returns the default dimensions of the SVG image.
   *
   * @return the image dimensions
   * @throws IOException if the image cannot be read
   */
  public static Dimension getImageSize(InputStream in) throws IOException {
    return getImageSize("", in);
  }

  /**
   * Returns the default dimensions of the SVG image.
   *
   * @return the image dimensions
   * @throws IOException if the image cannot be read
   */
  public static Dimension getImageSize(String name, InputStream in)
                                                          throws IOException {
    // get the SVG
    final Document doc;
    try {
      synchronized (factory) {
        doc = factory.createDocument(null, in);
      }
      in.close();
    }
    catch (DOMException e) {
      throw new ImageIOException(name, e);
    }
    catch (FileNotFoundException e) {
      throw new ImageNotFoundException(name, e);
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    // get the default image width and height
    final Element root = doc.getDocumentElement();
    try {
      final int width = (int) (Float.parseFloat(
        root.getAttributeNS(null, "width").replaceFirst("px", ""))+0.5);
      final int height = (int) (Float.parseFloat(
        root.getAttributeNS(null, "height").replaceFirst("px", ""))+0.5);

      return new Dimension(width, height);
    }
    catch (NumberFormatException e) {
      throw new ImageIOException(name, e);
    }
  }

  /**
   * Conducts a recursive depth-first search for external references
   * in the given SVG file.
   *
   * @param path the path of the file to check for external references
   */
  public static List<String> getExternalReferences(String path)
                                                           throws IOException {
    final ArrayList<String> reflist = new ArrayList<String>();
    reflist.add(path);
    return getExternalReferences(path, reflist);
  }

  /**
   * Conducts a recursive depth-first search for external references
   * in the SVG file named by path. This is a helper function for
   * {@link #getExternalReferences}.
   *
   * @param path the path of the file to check for external references
   * @param known the list of references already found
   */
  protected static List<String> getExternalReferences(
                          String path, List<String> known) throws IOException {

    final HashSet<String> follow = new HashSet<String>();
    final URL here = new URL("file", null, new File(path).getCanonicalPath());

    Document doc = null;
    try {
      synchronized (factory) {
        doc = factory.createDocument(here.toString());
      }
    }
    catch (DOMException e) {
      throw (IOException) new IOException().initCause(e);
    }

    final NodeList usenodes = doc.getElementsByTagName("use");
    for (int i = 0; i < usenodes.getLength(); ++i) {
      final Element e = (Element) usenodes.item(i);
      final URL url = new URL(new URL(e.getBaseURI()),
                              XLinkSupport.getXLinkHref(e));
      // balk (for now) unless file is available on our filesystem
      if (url.getProtocol().equals("file")) {
        final String refpath = url.getPath();
        if (!known.contains(refpath)) {
          follow.add(refpath);
          known.add(refpath);
        }
      }
      else {
        throw new IOException("unsupported protocol '" +
                              url.getProtocol() + "' in xlink:href");
      }
    }

    for (String s : follow) {
      known.addAll(getExternalReferences(s, known));
    }

    return known;
  }

  /**
   * Rewrites external references contained in SVG files.
   *
   * @param path the path of the file to be processed
   */
  public static byte[] relativizeExternalReferences(String path)
                                                           throws IOException {
    // use the GenericDOMImplementation here because
    // SVGDOMImplementation adds unwanted attributes to SVG elements
    final SAXDocumentFactory fac = new SAXDocumentFactory(
      new GenericDOMImplementation(),
      XMLResourceDescriptor.getXMLParserClassName());

    final URL here = new URL("file", null, new File(path).getCanonicalPath());
    final StringWriter sw = new StringWriter();

    try {
      final Document doc = fac.createDocument(here.toString());
      relativizeElement(doc.getDocumentElement());
      DOMUtilities.writeDocument(doc, sw);
    }
    catch (DOMException e) {
      throw (IOException) new IOException().initCause(e);
    }

    sw.flush();
    return sw.toString().getBytes();
  }

  protected static void relativizeElement(Element e) {
    // work from leaves to root in each subtree
    final NodeList children = e.getChildNodes();
    for (int i = 0; i < children.getLength(); ++i) {
      final Node n = children.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE)
        relativizeElement((Element) n);
    }

    // relativize the xlink:href attribute if there is one
    if (e.hasAttributeNS(XLinkSupport.XLINK_NAMESPACE_URI, "href")) {
      try {
        final URL url = new URL(new URL(e.getBaseURI()),
                                XLinkSupport.getXLinkHref(e));
        final String anchor = url.getRef();
        final String name = new File(url.getPath()).getName();
        XLinkSupport.setXLinkHref(e, name + '#' + anchor);
      }
      // FIXME: review error message
      catch (MalformedURLException ex) {
//        ErrorLog.warn(ex);
      }
    }

    // remove xml:base attribute if there is one
    e.removeAttributeNS(XMLSupport.XML_NAMESPACE_URI, "base");
  }
}
