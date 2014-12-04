/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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

import java.awt.AlphaComposite;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.BridgeException;
import org.apache.batik.bridge.DocumentLoader;
import org.apache.batik.bridge.UserAgent;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.apache.batik.ext.awt.image.GraphicsUtil;
import org.apache.batik.gvt.renderer.ConcreteImageRendererFactory;
import org.apache.batik.gvt.renderer.ImageRenderer;
import org.apache.batik.gvt.renderer.ImageRendererFactory;
import org.apache.batik.transcoder.SVGAbstractTranscoder;
import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.TranscodingHints;
import org.apache.batik.transcoder.keys.BooleanKey;
import org.apache.batik.transcoder.keys.PaintKey;
import org.apache.batik.util.XMLResourceDescriptor;

import org.apache.commons.lang.SystemUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import VASSAL.build.GameModule;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.io.IOUtils;

/**
 * Render an SVG image to a {@link BufferedImage}.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class SVGRenderer {
  private static final Logger logger =
    LoggerFactory.getLogger(SVGRenderer.class);

  private static final SAXSVGDocumentFactory docFactory =
    new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName());
  private static final ImageRendererFactory rendFactory =
    new ConcreteImageRendererFactory();

  private final Document doc;
  private final float defaultW, defaultH;
  private final Rasterizer r = new Rasterizer();

  public SVGRenderer(URL file, InputStream in) throws IOException {
    this(file.toString(), in);
  }

  public SVGRenderer(String file, InputStream in) throws IOException {
    // load the SVG
    try {
      // We synchronize on docFactory becuase it does internal caching
      // of the Documents it produces. This ensures that a Document is
      // being modified on one thread only.
      synchronized (docFactory) {
        doc = docFactory.createDocument(file, in);
      }
      in.close();
    }
    catch (DOMException e) {
      throw (IOException) new IOException().initCause(e);
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    // get the default image size
    final Element root = doc.getDocumentElement();

    defaultW = Float.parseFloat(
      root.getAttributeNS(null, "width").replaceFirst("px", ""));
    defaultH = Float.parseFloat(
      root.getAttributeNS(null, "height").replaceFirst("px", ""));
  }

  private static final double DEGTORAD = Math.PI/180.0;

  public BufferedImage render() {
    return render(0.0, 1.0);
  }

  public BufferedImage render(double angle, double scale) {
    // The renderer needs the bounds unscaled---scaling comes from the
    // width and height hints.
    AffineTransform px = AffineTransform.getRotateInstance(
      angle*DEGTORAD, defaultW/2.0, defaultH/2.0);
    r.setTransform(px);

    px = new AffineTransform(px);
    px.scale(scale, scale);

    final Rectangle2D rect = new Rectangle2D.Float(0, 0, defaultW, defaultH);
    final Rectangle2D b = px.createTransformedShape(rect).getBounds2D();

    r.addTranscodingHint(Rasterizer.KEY_WIDTH, (float) b.getWidth());
    r.addTranscodingHint(Rasterizer.KEY_HEIGHT, (float) b.getHeight());

    try {
      r.transcode(new TranscoderInput(doc), null);
      return r.getBufferedImage();
    }
    // FIXME: review error message
    catch (BridgeException e) {
      logger.error("", e);
    }
    catch (TranscoderException e) {
      logger.error("", e);
    }

    return null;
  }

  public BufferedImage render(double angle, double scale, Rectangle2D aoi) {
    // The renderer needs the bounds unscaled---scaling comes from the
    // width and height hints.
    AffineTransform px = AffineTransform.getRotateInstance(
      angle*DEGTORAD, defaultW/2.0, defaultH/2.0);
    r.setTransform(px);

    px = new AffineTransform(px);
    px.scale(scale, scale);

    final Rectangle2D rect = new Rectangle2D.Float(0, 0, defaultW, defaultH);

    r.addTranscodingHint(Rasterizer.KEY_WIDTH, (float) aoi.getWidth());
    r.addTranscodingHint(Rasterizer.KEY_HEIGHT, (float) aoi.getHeight());
    r.addTranscodingHint(Rasterizer.KEY_AOI, aoi);

    try {
      r.transcode(new TranscoderInput(doc), null);
      return r.getBufferedImage();
    }
    // FIXME: review error message
    catch (BridgeException e) {
      logger.error("", e);
    }
    catch (TranscoderException e) {
      logger.error("", e);
    }

    return null;
  }

  private static class DataArchiveDocumentLoader extends DocumentLoader {
    public DataArchiveDocumentLoader(UserAgent userAgent) {
      super(userAgent);
    }

    @Override
    public Document loadDocument(String uri)
        throws MalformedURLException, IOException {
      final String file = new File((new URL(uri)).getPath()).getName();

      BufferedInputStream in = null;
      try {
        in = new BufferedInputStream(
          GameModule.getGameModule()
                    .getDataArchive()
                    .getInputStream(file));
        final Document doc = loadDocument(uri, in);
        in.close();
        return doc;
      }
      catch (DOMException e) {
        throw (IOException) new IOException().initCause(e);
      }
      finally {
        IOUtils.closeQuietly(in);
      }
    }
  }

  private static class Rasterizer extends SVGAbstractTranscoder {
    private DocumentLoader docLoader;
    private BufferedImage image;
    private AffineTransform xform;

    public Rasterizer() {
      docLoader = new DataArchiveDocumentLoader(userAgent);
    }

    @Override
    protected BridgeContext createBridgeContext() {
      return new BridgeContext(userAgent, docLoader);
    }

    @Override
    protected void transcode(Document document,
                             String uri,
                             TranscoderOutput output)
                             throws TranscoderException {
      if (SystemUtils.IS_OS_MAC_OSX) {
        final Element g = document.createElementNS(
          SVGDOMImplementation.SVG_NAMESPACE_URI, "g"
        );
        g.setAttributeNS(null, "transform", "rotate(0.000001)");
      
        // interpose this <g> element between <svg> and its children
        final Element svg = document.getDocumentElement();
        Node n = null;
        while ((n = svg.getFirstChild()) != null) {
          g.appendChild(n);
        }

        svg.appendChild(g);
      }

      // Sets up root, curTxf & curAoi
      super.transcode(document, uri, output);

       // prepare the image to be painted
      int w = (int)(width+0.5);
      int h = (int)(height+0.5);

      // paint the SVG document using the bridge package
      // create the appropriate renderer
      ImageRenderer renderer = rendFactory.createStaticImageRenderer();
      renderer.updateOffScreen(w, h);
      if (xform != null) curTxf.concatenate(xform);
      renderer.setTransform(curTxf);
      renderer.setTree(this.root);
      this.root = null; // We're done with it...

      // now we are sure that the aoi is the image size
      final Shape raoi = new Rectangle2D.Float(0, 0, width, height);
      // Warning: the renderer's AOI must be in user space
      try {
        renderer.repaint(curTxf.createInverse().
                         createTransformedShape(raoi));
      }
      catch (NoninvertibleTransformException e) {
        throw new TranscoderException(e);
      }

// FIXME: is this the image we want to use?
      BufferedImage rend = renderer.getOffScreen();
      renderer = null; // We're done with it...

      // produce an opaque image if our background color is set
      final BufferedImage dest = ImageUtils.createCompatibleImage(
        w, h, !hints.containsKey(KEY_BACKGROUND_COLOR)
      );

      final Graphics2D g2d = GraphicsUtil.createGraphics(dest);
      if (hints.containsKey(KEY_BACKGROUND_COLOR)) {
        final Paint bgcolor = (Paint) hints.get(KEY_BACKGROUND_COLOR);
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

    private void writeImage(BufferedImage image, TranscoderOutput output) {
      this.image = image;
    }

    public BufferedImage getBufferedImage() {
      return image;
    }

    public void setTransform(AffineTransform px) {
      xform = px;
    }
  }

  public static final TranscodingHints.Key KEY_BACKGROUND_COLOR =
    new PaintKey();

  public static final TranscodingHints.Key KEY_FORCE_TRANSPARENT_WHITE =
    new BooleanKey();
}
