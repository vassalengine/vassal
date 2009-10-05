/*
 * $Id$
 *
 * Copyright (c) 2007-2009 by Joel Uckelman
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

package VASSAL.tools.image;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.color.CMMException;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;

import VASSAL.build.BadDataReport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.RereadableInputStream;


public class ImageLoader {

  public static BufferedImage getImage(String name, InputStream in)
                                                      throws ImageIOException {
    return INSTANCE.load(name, in);
  }

  public static Dimension getImageSize(String name, InputStream in)
                                                      throws ImageIOException {
    return INSTANCE.size(name, in);
  }

  protected static final ImageLoader INSTANCE = new ImageLoader(); 

  protected ImageLoader() {}

  // Used to indicate whether this version of Java has the PNG iTXt bug.
  // This can be removed once we no longer support Java 1.5.
  protected static final boolean iTXtBug;
  
  static {
    final String jvmver = System.getProperty("java.version");
    iTXtBug = jvmver == null || jvmver.startsWith("1.5");
  }

  protected static final Set<Integer> skip_iTXt =
    Collections.singleton(PNGDecoder.iTXt);

  protected BufferedImage loadImageIO(String name, InputStream in)
                                                           throws IOException {
    BufferedImage img = null;
    try {
      img = ImageIO.read(new MemoryCacheImageInputStream(in));
    }
    catch (CMMException e) {
      // Note: ImageIO can throw a CMMException for JPEGs which have
      // broken color profiles. This problem is noted in Sun Bug 6444360,
      //
      // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6444360
      //
      ErrorDialog.dataError(new BadDataReport("Broken image", name));
      throw (IOException) new IOException().initCause(e);
    }
    catch (IllegalArgumentException e) {
      // Note: ImageIO can throw IllegalArgumentExceptions for certain
      // kinds of broken images, e.g., JPEGs which are in the RGB color
      // space but have non-RGB color profiles (see Bug 2673589 for an
      // example of this). This problem is noted in Sun Bug 6404011,
      //
      // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6404011
      //
      ErrorDialog.dataError(new BadDataReport("Broken image", name));
      throw (IOException) new IOException().initCause(e);
    }

    if (img == null) throw new UnrecognizedImageTypeException();
    return img;
  }

  protected BufferedImage load(String name, InputStream in)
                                                      throws ImageIOException {
    //
    // ImageIO fails on the following types of images:
    //
    // Sun Bug 6788458: 8-bit/channel color type 2 (RGB) PNGs with tRNS chunks
    // Sun Bug 6541476: PNGs with iTXt chunks on Java 1.5
    // Sun Bug 6444360: JPEGs with corrupt color profiles
    // Sun Bug 6404011: JPEGs with corrupt color profiles on Java 1.5 
    // 
    // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6788458
    // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6541476
    // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6444360
    // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6404011
    //
    // Someday, when both ImageIO is fixed and everyone's JRE contains
    // that fix, we can do this the simple way:
    //
    // final BufferedImage img =
    //   ImageIO.read(new MemoryCacheImageInputStream(in));
    // if (img == null) throw new UnrecognizedImageTypeException();
    // return toCompatibleImage(img);
    //

    BufferedImage img = null;
    RereadableInputStream rin = null;
    try {
      rin = new RereadableInputStream(in);
      rin.mark(512);

      DataInputStream din = new DataInputStream(rin);
      PNGDecoder.Chunk ch; 

      boolean fix_tRNS = false;
      int tRNS = 0x00000000;

      // Is this a PNG?
      if (PNGDecoder.decodeSignature(din)) {        
        // The PNG chunks refered to here are defined in the PNG
        // standard, found at http://www.w3.org/TR/PNG/
        ch = PNGDecoder.decodeChunk(din);

        // Sanity check: This is not a PNG if IHDR is not the first chunk.
        if (ch.type == PNGDecoder.IHDR) {
          // At present, ImageIO does not honor the tRNS chunk in 8-bit color
          // type 2 (RGB) PNGs. This is not a bug per se, as the PNG standard
          // the does not require compliant decoders to use ancillary chunks.
          // However, every other PNG decoder we can find *does* honor the
          // tRNS chunk for this type of image, and so the appearance for
          // users is that VASSAL is broken when their 8-bit RGB PNGs don't
          // show the correct transparency.
          
          // We check for type-2 8-bit PNGs with tRNS chunks.
          if (ch.data[8] == 8 && ch.data[9] == 2) {
            // This is an 8-bit-per-channel Truecolor image; we must check
            // whether there is a tRNS chunk, and if so, record the color
            // so that we can manually set transparency later.
            //
            // IHDR is required to be first, and tRNS is required to appear
            // before the first IDAT chunk; therefore, if we find an IDAT
            // we're done.

            DONE: for (;;) {
              ch = PNGDecoder.decodeChunk(din);

              switch (ch.type) {
              case PNGDecoder.tRNS: fix_tRNS = true;  break DONE;
              case PNGDecoder.IDAT: fix_tRNS = false; break DONE;
              default:
              }
            }

            if (fix_tRNS) {
              if (ch.data.length != 6) { 
                // There is at least one piece of software (SplitImage) which
                // writes tRNS chunks for type 2 images which are only 3 bytes
                // long, and because this kind of thing is used by module
                // designers for slicing up scans of countersheets, we can
                // expect to see such crap from time to time.
                ErrorDialog.dataError(new BadDataReport("Broken image", name));
                return null;
              }
              
              //
              // tRNS chunk: PNG Standard, 11.3.2.1
              //
              // tRNS data is stored as three 2-byte samples, but the high
              // byte of each sample is empty because we are dealing with
              // 8-bit-per-channel images.
              tRNS = 0xff000000 |
                     ((ch.data[1] & 0xff) << 16) |
                     ((ch.data[3] & 0xff) <<  8) |
                      (ch.data[5] & 0xff);
            }
          }

          if (iTXtBug) {
            // Filter out iTXt chunks on JVMs with the iTXt bug.
            rin.reset();
            rin = new RereadableInputStream(
              new PNGChunkSkipInputStream(skip_iTXt, rin));
            rin.mark(1);
          }
        }
      }

      // Load the image
      rin.reset();
      img = loadImageIO(name, rin);
      rin.close();

      // Fix up transparency in type 2 Truecolor images.
      if (fix_tRNS) img = fix_tRNS(img, tRNS); 
    
      return toCompatibleImage(img);
    }
    catch (UnrecognizedImageTypeException e) {
      throw new UnrecognizedImageTypeException(name, e);
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
    finally {
      IOUtils.closeQuietly(rin);
    }
  }

  protected BufferedImage toCompatibleImage(BufferedImage img)
                                                           throws IOException {
    return ImageUtils.toCompatibleImage(img);
  }

  protected BufferedImage fix_tRNS(BufferedImage img, int tRNS) {
    // Ensure that we are working with integer ARGB data. Whether it's
    // premultiplied doesn't matter, since fully transparent black pixels
    // are the same in both.
    if (img.getType() != BufferedImage.TYPE_INT_ARGB &&
        img.getType() != BufferedImage.TYPE_INT_ARGB_PRE) {
    
      // Convert to the compatible type if it's an integer type; otherwise,
      // convert to unpremultiplied integer.
      final int compat = ImageUtils.getCompatibleTranslucentImageType();
      img = ImageUtils.toType(img,
              compat == BufferedImage.TYPE_INT_ARGB || 
              compat == BufferedImage.TYPE_INT_ARGB_PRE ? compat :
              BufferedImage.TYPE_INT_ARGB);
    }

    // Set all pixels of the transparent color to have alpha 0.
    final WritableRaster r = img.getRaster();    
    final int w = img.getWidth();
    final int h = img.getHeight();

    // FIXME: using get/setDataElements() causes us to copy the image data
    // at most three times (get, set, toCompatibleImage). Working on the
    // DataBufferInt directly unmanages the image, but will let us do a
    // single copy. Implementing this is complicated while we still use
    // memory-mapped images. Change this once memory-mapped images are gone.
    final int[] data = (int[]) r.getDataElements(0, 0, w, h, new int[w*h]);
        
    for (int i = 0; i < data.length; ++i) {
      if (data[i] == tRNS) data[i] = 0x00000000;
    }

    r.setDataElements(0, 0, w, h, data); 

    return img;
  }

  protected Dimension size(String name, InputStream in)
                                                      throws ImageIOException {
    try {
      final ImageInputStream stream = new MemoryCacheImageInputStream(in);

      final Iterator<ImageReader> i = ImageIO.getImageReaders(stream);
      if (!i.hasNext()) throw new UnrecognizedImageTypeException(name);

      final ImageReader reader = i.next();
      try {
        reader.setInput(stream);

        Dimension size = null;
        try {
          size = new Dimension(reader.getWidth(0), reader.getHeight(0));
        }
        catch (IllegalArgumentException e) {
          // Note: ImageIO can throw IllegalArgumentExceptions for certain
          // kinds of broken images, e.g., JPEGs which are in the RGB color
          // space but have non-RGB color profiles (see Bug 2673589 for an
          // example of this). This problem is noted in Sun Bug 6404011,
          //
          // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6404011
          //
          // and supposedly is fixed in 1.6.0 JVMs. Once we move to Java 6,
          // this will no longer be a problem, and we can remove this catch.
          //
          // We catch IllegalArgumentException here in a last-ditch effort
          // to prevent broken images---which are bad data, not bugs---from
          // causing an uncaught exception and raising a Bug Dialog.
          ErrorDialog.dataError(new BadDataReport("Broken image", name));
          throw (IOException) new IOException().initCause(e);
        }
        
        in.close();
        return size;
      }
      finally {
        reader.dispose();
      }
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }
}
