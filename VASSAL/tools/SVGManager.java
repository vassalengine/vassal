/*
 * $Id$
 *
 * Copyright (c) 2006 by Joel Uckelman
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
package VASSAL.tools;

import java.awt.AlphaComposite;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.DocumentLoader;
import org.apache.batik.bridge.UserAgent;
import org.apache.batik.css.engine.CSSImportedElementRoot;
import org.apache.batik.dom.GenericDOMImplementation;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.dom.svg.XMLBaseSupport;
import org.apache.batik.dom.util.DOMUtilities;
import org.apache.batik.dom.util.SAXDocumentFactory;
import org.apache.batik.dom.util.XLinkSupport;
import org.apache.batik.ext.awt.image.GraphicsUtil;
import org.apache.batik.gvt.renderer.ConcreteImageRendererFactory;
import org.apache.batik.gvt.renderer.ImageRenderer;
import org.apache.batik.gvt.renderer.ImageRendererFactory;
import org.apache.batik.transcoder.SVGAbstractTranscoder;
import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.TranscodingHints;
import org.apache.batik.transcoder.image.ImageTranscoder;
import org.apache.batik.transcoder.keys.BooleanKey;
import org.apache.batik.transcoder.keys.PaintKey;
import org.apache.batik.util.ParsedURL;
import org.apache.batik.util.XMLResourceDescriptor;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Loads SVG images and ensures that referenced SVG documents are available.
 *
 * @author Joel Uckelman
 */
public class SVGManager {
   protected final DataArchive archive;
   protected final SAXSVGDocumentFactory factory; 
   protected final BufferedImageTranscoder trans;
   protected final ImageRendererFactory rendFactory;   

   public SVGManager(DataArchive archive) {
      this.archive = archive;
      factory = new SAXSVGDocumentFactory(
         XMLResourceDescriptor.getXMLParserClassName()); 
      trans = new BufferedImageTranscoder();
      rendFactory = new ConcreteImageRendererFactory();
   }

   /** 
    * Loads the untransformed base image.
    */
   public BufferedImage loadSVGImage(String file, InputStream fileStream)
      throws IOException {
          
      // get the SVG
      Document doc = factory.createDocument(file, fileStream);

      // get the default image width and height
      Element root = doc.getDocumentElement();
      int width =
         (int)(Float.parseFloat(root.getAttributeNS(null, "width"))+0.5);
      int height =
         (int)(Float.parseFloat(root.getAttributeNS(null, "height"))+0.5);

      // render the image
      return new SVGBufferedImage(doc, width, height,
                                  BufferedImage.TYPE_4BYTE_ABGR);
   }

   public Dimension getImageSize(String file, InputStream fileStream)
      throws IOException {
      
      // get the SVG
      Document doc = factory.createDocument(file, fileStream);

      // get the default image width and height
      Element root = doc.getDocumentElement();
      int width =
         (int)(Float.parseFloat(root.getAttributeNS(null, "width"))+0.5);
      int height =
         (int)(Float.parseFloat(root.getAttributeNS(null, "height"))+0.5);

      return new Dimension(width, height);
   }

   /**
    * Conducts a recursive depth-first search for external references
    * in the SVG file named by path.
    * 
    * @param path
    *          the path of the file to check for external references 
    */
   public List getExternalReferences(String path) {
      List reflist = new ArrayList();
      reflist.add(path);
      return getExternalReferences(path, reflist);
   }
   
   /**
    * Conducts a recursive depth-first search for external references
    * in the SVG file named by path. This is a helper function for
    * {@link #getExternalReferences}.
    *
    * @param path
    *          the path of the file to check for external references
    * @param known
    *          the list of references already found
    */
   protected List getExternalReferences(String path, List known) {
      Set follow = new HashSet();
      try {
         URL here = new URL("file", null, (new File(path)).getCanonicalPath());
         Document doc = factory.createDocument(here.toString());
         NodeList usenodes = doc.getElementsByTagName("use");
         for (int i = 0; i < usenodes.getLength(); ++i) { 
            Element e = (Element) usenodes.item(i);
            URL url = new URL(new URL(XMLBaseSupport.getCascadedXMLBase(e)),
                                        XLinkSupport.getXLinkHref(e));
            // balk (for now) unless file is available on our filesystem
            if (url.getProtocol().equals("file")) {
               String refpath = url.getPath();
               if (!known.contains(refpath)) {
                  follow.add(refpath);
                  known.add(refpath);
               }
            }
            else {
               throw new IOException("unsupported protocol '"
                  + url.getProtocol() + "' in xlink:href");
            }
         }
      }
      catch (IOException ex) {
// FIXME: Should we really ignore this? It (probably) means that we can't
// find some xlinked SVG file...
//         System.err.print(ex.getClass() + ": " + ex.getCause()
//                                        + ", " + ex.getMessage() + "\n");
//         ex.printStackTrace();
      }

      for (Iterator i = follow.iterator(); i.hasNext(); )
         known.addAll(getExternalReferences((String) i.next(), known));
      
      return known;
   }

   protected URL doc_url;  // hack to work around XMLBaseSupport problem

   /**
    * Rewrites external references contained in SVG files.
    *
    * @param path the path of the file to be processed
    */
   public byte[] relativizeExternalReferences(String path)
    throws IOException {
      // use the GenericDOMImplementation here because
      // SVGDOMImplementation adds unwanted attributes to SVG elements
      SAXDocumentFactory fac = new SAXDocumentFactory(
         new GenericDOMImplementation(),
         XMLResourceDescriptor.getXMLParserClassName());
      URL here = new URL("file", null, (new File(path)).getCanonicalPath());
      Document doc = fac.createDocument(here.toString());
      doc_url = here;
      relativizeElement(doc.getDocumentElement());

      StringWriter sw = new StringWriter();
      DOMUtilities.writeDocument(doc, sw);
      sw.flush();
      return sw.toString().getBytes();
   }

   protected void relativizeElement(Element e) {
      // work from leaves to root in each subtree
      NodeList children = e.getChildNodes();
      for (int i = 0; i < children.getLength(); ++i) {
         Node n = children.item(i);
         if (n.getNodeType() == Node.ELEMENT_NODE)
            relativizeElement((Element) n);
      }

      // relativize the xlink:href attribute if there is one
      if (e.hasAttributeNS(XLinkSupport.XLINK_NAMESPACE_URI, "href")) {
         try {
//   FIXME: XMLBaseSupport requires a SVGDocument, which we don't want;
//   this is fixed Batik 1.7:
//            URL url = new URL(new URL(XMLBaseSupport.getCascadedXMLBase(e)),
//                                        XLinkSupport.getXLinkHref(e));
            URL url = new URL(doc_url, XLinkSupport.getXLinkHref(e));
            String anchor = url.getRef();
            String name = new File(url.getPath()).getName();
            XLinkSupport.setXLinkHref(e, name + '#' + anchor);
         }
         catch (MalformedURLException ex) {                 
            System.err.print(ex.getClass() + ": " + ex.getCause()
                                           + ", " + ex.getMessage() + "\n");
            ex.printStackTrace();
         }
      }

      // remove xml:base attribute if there is one
      e.removeAttributeNS(XMLBaseSupport.XML_NAMESPACE_URI, "base");
   }
 
   /**
    * Lifted from org.apache.batik.dom.svg.XMLBaseSupport.getCascadedXMLBase().
    * Pitch this and use the version from Batik 1.7 once it's released.
    */ 
   public String getCascadedXMLBase(Element elt) {
      String base = null;
      Node n = elt.getParentNode();
      while (n != null) {
         if (n.getNodeType() == Node.ELEMENT_NODE) {
            base = getCascadedXMLBase((Element)n);
            break;
         }
         if (n instanceof CSSImportedElementRoot) {
            n = ((CSSImportedElementRoot)n).getCSSParentElement();
         } else {
            n = n.getParentNode();
         }
      }
      if (base == null) {
         if (doc_url != null) {
            base = doc_url.toString();
         }
      }
      Attr attr =
         elt.getAttributeNodeNS(XMLBaseSupport.XML_NAMESPACE_URI, "base");
      if (attr != null) {
         if (base == null) {
            base = attr.getNodeValue();
         } else {
            base = new ParsedURL(base, attr.getNodeValue()).toString();
         }
      }
      return base;
   }

   /**
    * Renders SVG to a BufferedImage.
    */
   protected class BufferedImageTranscoder extends SVGAbstractTranscoder {
      protected BufferedImage image;
      protected DocumentLoader documentLoader;
      protected AffineTransform xform;

      public BufferedImageTranscoder() {
         documentLoader = new SVGManagerDocumentLoader(userAgent);
      }

      public BufferedImage createImage(int width, int height) {
         return new BufferedImage(width, height,
        		                  BufferedImage.TYPE_4BYTE_ABGR);    	      	  
      }
      
      public void writeImage(BufferedImage image, TranscoderOutput output) {
         this.image = image;
      }

      public BufferedImage getBufferedImage() {
         return image;
      }
      
      protected BridgeContext createBridgeContext() {
         return new BridgeContext(userAgent, documentLoader);
      }

      public void setTransform(AffineTransform px) {
         xform = px;
      }

      protected void transcode(Document document,
                               String uri,
                               TranscoderOutput output)
                     throws TranscoderException {
         // Sets up root, curTxf & curAoi
         super.transcode(document, uri, output);

         // prepare the image to be painted
         int w = (int)(width+0.5);
         int h = (int)(height+0.5);

         // paint the SVG document using the bridge package
         // create the appropriate renderer
         ImageRenderer renderer = rendFactory.createStaticImageRenderer();
         renderer.updateOffScreen(w, h);
         renderer.setTransform(xform == null ? curTxf : xform);
         renderer.setTree(this.root);
         this.root = null; // We're done with it...

         try {
            // now we are sure that the aoi is the image size
            Shape raoi = new Rectangle2D.Float(0, 0, width, height);
            // Warning: the renderer's AOI must be in user space
            renderer.repaint(curTxf.createInverse().
                             createTransformedShape(raoi));
            BufferedImage rend = renderer.getOffScreen();
            renderer = null; // We're done with it...

            BufferedImage dest = createImage(w, h);

            Graphics2D g2d = GraphicsUtil.createGraphics(dest);
            if (hints.containsKey(KEY_BACKGROUND_COLOR)) {
                Paint bgcolor = (Paint)hints.get(KEY_BACKGROUND_COLOR);
                g2d.setComposite(AlphaComposite.SrcOver);
                g2d.setPaint(bgcolor);
                g2d.fillRect(0, 0, w, h);
            }
            if (rend != null) { // might be null if the svg document is empty
                g2d.drawRenderedImage(rend, new AffineTransform());
            }
            g2d.dispose();
            rend = null; // We're done with it...
            writeImage(dest, output);
         }
         catch (Exception ex) {
            throw new TranscoderException(ex);
         }
      }
   }

   public static final TranscodingHints.Key KEY_BACKGROUND_COLOR
      = new PaintKey();

   public static final TranscodingHints.Key KEY_FORCE_TRANSPARENT_WHITE
      = new BooleanKey();
 
   protected class SVGManagerDocumentLoader extends DocumentLoader {
      public SVGManagerDocumentLoader(UserAgent userAgent) {
         super(userAgent);
      }

      public Document loadDocument(String uri)
       throws MalformedURLException, IOException {
         String file = DataArchive.IMAGE_DIR +
            (new File((new URL(uri)).getPath())).getName();
         return loadDocument(uri, archive.getFileStream(file));
      }
   }

   /**
    * A BufferedImage backed by SVG. When a scaled instance is requested,
    * the image is rendered from SVG at the requested size.
    */
   public class SVGBufferedImage extends BufferedImage {
      protected TranscoderInput input;

      public SVGBufferedImage(Document doc, int width, int height, int type) {
         super(width, height, type);
         input = new TranscoderInput(doc);
         BufferedImage trans = getTransformedInstance(1,0);
         if (trans != null) setData(trans.getData());
      }

      public BufferedImage getTransformedInstance(double zoom, double theta) {
         if (zoom != 1 || theta != 0) {
            AffineTransform px = AffineTransform.getScaleInstance(zoom, zoom);
            px.rotate(Math.PI/180*theta, getWidth()/2.0, getHeight()/2.0);

            Rectangle b = new Rectangle(getWidth(), getHeight());
            Rectangle tb = px.createTransformedShape(b).getBounds();

            px = AffineTransform.getTranslateInstance(-tb.x, -tb.y);
            px.scale(zoom, zoom);
            px.rotate(Math.PI/180*theta, getWidth()/2.0, getHeight()/2.0);
            px.translate(b.x, b.y);

            trans.setTransform(px);
         }
         else trans.setTransform(null);

         BufferedImage image = null;
         try {
            trans.addTranscodingHint(ImageTranscoder.KEY_WIDTH,
           	                         new Float(getWidth()));
       		trans.addTranscodingHint(ImageTranscoder.KEY_HEIGHT,
           	                         new Float(getHeight()));
            trans.transcode(input, null);
            image = trans.getBufferedImage();
       	}
         catch (TranscoderException te) {
            Exception ex = te.getException();
            System.err.print(ex.getClass() + ": " + ex.getCause()
                                           + ", " + ex.getMessage() + "\n");
            ex.printStackTrace();
         }
       	    	 
       	return image;
      }

      public Image getScaledInstance(int width, int height, int hints) {
         return getTransformedInstance(width / (double) getWidth(), 0);
      }
   }
}
