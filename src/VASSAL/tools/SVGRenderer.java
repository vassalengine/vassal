/*
 * $Id: SVGManager.java 3245 2008-03-08 23:18:52Z uckelman $
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

package VASSAL.tools;

import java.awt.AlphaComposite;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.DocumentLoader;
import org.apache.batik.bridge.UserAgent;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
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
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.GameModule;

/**
 * Render an SVG image to a {@link BufferedImage}.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */ 
public class SVGRenderer {
  private static final SAXSVGDocumentFactory docFactory =
    new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName());
  private static final ImageRendererFactory rendFactory =
    new ConcreteImageRendererFactory();
 
  private final Document doc;
  private final float defaultW, defaultH;
  private final Rasterizer r = new Rasterizer();

  public SVGRenderer(String file, InputStream in) throws IOException {
    // load the SVG
    try {
      doc = docFactory.createDocument(file, in);
    }
    finally {
      if (in != null) in.close();
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
    catch (TranscoderException e) {
      e.printStackTrace();
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
    @SuppressWarnings("unused")
    final Rectangle2D b = px.createTransformedShape(rect).getBounds2D();

//    r.addTranscodingHint(Rasterizer.KEY_WIDTH, (float) b.getWidth());
//    r.addTranscodingHint(Rasterizer.KEY_HEIGHT, (float) b.getHeight());
    r.addTranscodingHint(Rasterizer.KEY_WIDTH, (float) aoi.getWidth());
    r.addTranscodingHint(Rasterizer.KEY_HEIGHT, (float) aoi.getHeight());
    r.addTranscodingHint(Rasterizer.KEY_AOI, aoi);

    try {
      r.transcode(new TranscoderInput(doc), null);
      return r.getBufferedImage();
    }
    catch (TranscoderException e) {
      e.printStackTrace();
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
      final String file = DataArchive.IMAGE_DIR +
        (new File((new URL(uri)).getPath())).getName();

      final InputStream in = GameModule.getGameModule()
                                       .getDataArchive()
                                       .getFileStream(file);
      try {
        return loadDocument(uri, in);
      }
      finally {
        try {
          in.close();
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
  }

  private class Rasterizer extends SVGAbstractTranscoder {
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

      try {
        // now we are sure that the aoi is the image size
        final Shape raoi = new Rectangle2D.Float(0, 0, width, height);
        // Warning: the renderer's AOI must be in user space
        renderer.repaint(curTxf.createInverse().
                         createTransformedShape(raoi));
        BufferedImage rend = renderer.getOffScreen();
        renderer = null; // We're done with it...

        final BufferedImage dest = createImage(w, h);

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
      catch (Exception ex) {
        throw new TranscoderException(ex);
      }
    }

    private BufferedImage createImage(int w, int h) {
      return new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
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
