/*
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
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.apache.batik.anim.dom.SAXSVGDocumentFactory;
import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.BridgeException;
import org.apache.batik.bridge.UnitProcessor;
import org.apache.batik.bridge.UserAgent;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.bridge.ViewBox;
import org.apache.batik.dom.GenericDOMImplementation;
import org.apache.batik.dom.util.DOMUtilities;
import org.apache.batik.dom.util.SAXDocumentFactory;
import org.apache.batik.dom.util.XLinkSupport;
import org.apache.batik.dom.util.XMLSupport;
import org.apache.batik.util.SVGConstants;
import org.apache.batik.util.XMLResourceDescriptor;

import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.svg.SVGDocument;
import org.w3c.dom.svg.SVGSVGElement;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;

/**
 * Utility methods for manipulating SVG images.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class SVGImageUtils {
  private SVGImageUtils() { }

  // NB: SAXSVGDocumentFactory isn't thread-safe, we have to synchronize on it.
  private static final SAXSVGDocumentFactory FACTORY =
    new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName());

  public static SVGDocument getDocument(String file, InputStream in) throws IOException {
    try (in) {
      // We synchronize on FACTORY becuase it does internal caching
      // of the Documents it produces. This ensures that a Document is
      // being modified on one thread only.
      synchronized (FACTORY) {
        return FACTORY.createSVGDocument(file, in);
      }
    }
    catch (DOMException e) {
      throw new IOException(e);
    }
  }

  /**
   * Returns the default dimensions of the SVG image.
   *
   * Closes the {@link InputStream}.
   *
   * @return the image dimensions
   * @throws IOException if the image cannot be read
   */
  public static Dimension getImageSize(String file, InputStream in) throws IOException {
    try {
      return getImageSize(getDocument(file, in));
    }
    catch (FileNotFoundException e) {
      throw new ImageNotFoundException(file, e);
    }
    catch (DOMException | IOException e) {
      throw new ImageIOException(file, e);
    }
  }

  public static Dimension getImageSize(SVGDocument doc) throws IOException {
    final SVGSVGElement root = doc.getRootElement();
    final BridgeContext bctx = new BridgeContext(new UserAgentAdapter());
    final UnitProcessor.Context uctx = UnitProcessor.createContext(bctx, root);

    // try to parse the width
    float w = -1.0f;
    boolean wIsPct = false;
    final String ws = root.getAttributeNS(null, SVGConstants.SVG_WIDTH_ATTRIBUTE);
    if (!ws.isEmpty()) {
      try {
        w = UnitProcessor.svgHorizontalLengthToUserSpace(ws, SVGConstants.SVG_WIDTH_ATTRIBUTE, uctx);
      }
      catch (BridgeException e) {
        // the width was invalid
        throw new IOException(e);
      }
      wIsPct = ws.contains("%");
    }

    // try to parse the height
    float h = -1.0f;
    boolean hIsPct = false;
    final String hs = root.getAttributeNS(null, SVGConstants.SVG_HEIGHT_ATTRIBUTE);
    if (!hs.isEmpty()) {
      try {
        h = UnitProcessor.svgVerticalLengthToUserSpace(hs, SVGConstants.SVG_HEIGHT_ATTRIBUTE, uctx);
      }
      catch (BridgeException e) {
        // the height was invalid
        throw new IOException(e);
      }
      hIsPct = hs.contains("%");
    }

    // try to parse the viewBox
    float[] vb = null;
    final String vbs = root.getAttributeNS(null, SVGConstants.SVG_VIEW_BOX_ATTRIBUTE);
    if (!vbs.isEmpty()) {
      try {
        vb = ViewBox.parseViewBoxAttribute(root, vbs, bctx);
      }
      catch (BridgeException e) {
        // the viewBox was invalid
        throw new IOException(e);
      }
    }

    if (w < 0.0f || h < 0.0f) {
      if (!vbs.isEmpty()) {
        // the viewBox array will be null if it had 0 height or width
        if (vb != null) {
          // we have a nonempty viewBox
          if (w < 0.0f) {
            // no width given; use the width of the viewBox
            w = vb[2];
          }
          else if (wIsPct) {
            // width is a percentage of the viewBox width
            w *= vb[2];
          }

          if (h < 0.0f) {
            // no height given; use the height of the viewBox
            h = vb[3];
          }
          else if (hIsPct) {
            // width is a percentage of the viewBox height 
            h *= vb[3];
          }
        }
      }
      else {
        // no viewBox
        if (h >= 0.0f) {
          // height but no width; make it square
          w = h;
        }
        else if (w >= 0.0f) {
          // width but no height; make it square
          h = w;
        }
        else {
          // no dimensions specified
          w = h = 0;
        }
      }
    }
    else if (!vbs.isEmpty() && vb != null) {
      // we have a viewBox; adjust width, height if they are percentages of it
      if (wIsPct) {
        w *= vb[2];
      }

      if (hIsPct) {
        h *= vb[3];
      }
    }
    
    return new Dimension((int)(w + 0.5f), (int)(h + 0.5f));
  }

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
   * Conducts a recursive depth-first search for external references
   * in the given SVG file.
   *
   * @param path the path of the file to check for external references
   */
  public static List<String> getExternalReferences(String path)
                                                           throws IOException {
    final ArrayList<String> reflist = new ArrayList<>();
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

    final HashSet<String> follow = new HashSet<>();
    final URL here = new URL("file", null, new File(path).getCanonicalPath());

    Document doc = null;
    try {
      synchronized (FACTORY) {
        doc = FACTORY.createDocument(here.toString());
      }
    }
    catch (DOMException e) {
      throw new IOException(e);
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
      XMLResourceDescriptor.getXMLParserClassName()
    );

    final URL here = new URL("file", null, new File(path).getCanonicalPath());
    final StringWriter sw = new StringWriter();

    try {
      final Document doc = fac.createDocument(here.toString());
      relativizeElement(doc.getDocumentElement());
      DOMUtilities.writeDocument(doc, sw);
    }
    catch (DOMException e) {
      throw new IOException(e);
    }

    sw.flush();
    return sw.toString().getBytes(StandardCharsets.UTF_8);
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
