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
import java.awt.Toolkit;
import java.awt.color.CMMException;
import java.awt.image.BufferedImage;
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
    return ImageUtils.toCompatibleImage(img);
  }

  protected BufferedImage loadToolkit(String name, InputStream in)
                                                           throws IOException {
    // Load as an Image; note that we forceLoad() to ensure that the
    // subsequent calls to getWidth() and getHeight() return the
    // actual width and height of the Image.
    final Image i = ImageUtils.forceLoad(
      Toolkit.getDefaultToolkit().createImage(IOUtils.toByteArray(in))
    );

    // Toolkit.createImage() is unforgiving about malformed images but
    // instead of throwing an exception or returning null, it returns a
    // useless Image with negative width and height. (It might also
    // print a stack trace to the log.) There is at least one piece of
    // software (SplitImage) which writes tRNS chunks for type 2 images
    // which are only 3 bytes long, and because this kind of thing is
    // used by module designers for slicing up scans of countersheets,
    // we can expect to see such crap from time to time.
    if (i.getWidth(null) > 0 && i.getHeight(null) > 0) {
      return ImageUtils.toBufferedImage(i);
    }
    else {
      // Toolkit failed for some reason. Probably this means that
      // we have a broken image, so gently notify the user and
      // fallback to ImageIO.read().
      ErrorDialog.dataError(new BadDataReport("Broken image", name));
      return null;
    }
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

      final boolean useImageIO;

      final boolean isPNG;
      PNGDecoder.Chunk ch; 

      // Is this a PNG?
      if (PNGDecoder.decodeSignature(din)) {        
        // The PNG chunks refered to here are defined in the PNG
        // standard, found at http://www.w3.org/TR/PNG/
        ch = PNGDecoder.decodeChunk(din);

        // Sanity check: This is not a PNG if IHDR is not the first chunk.
        if (ch.type == PNGDecoder.IHDR) {
          //
          // PNGs
          //

          // At present, ImageIO does not honor the tRNS chunk in 8-bit color
          // type 2 (RGB) PNGs. This is not a bug per se, as the PNG standard
          // the does not require compliant decoders to use ancillary chunks.
          // However, every other PNG decoder we can find *does* honor the
          // tRNS chunk for this type of image, and so the appearance for
          // users is that VASSAL is broken when their 8-bit RGB PNGs don't
          // show the correct transparency.
          //
          // We check for type-2 8-bit PNGs with tRNS chunks and use
          // Toolkit.createImage() for loading them, instead of ImageIO.

          if (ch.data[8] != 8 || ch.data[9] != 2) {
            // This is not an 8-bit-per-channel Truecolor image, use ImageIO
            useImageIO = true;
          }
          else {
            // This is an 8-bit-per-channel Truecolor image; we must check
            // whether there is a tRNS chunk, and use Toolkit if there is.
            //
            // IHDR is required to be first, and tRNS is required to appear
            // before the first IDAT chunk; therefore, if we find an IDAT
            // we're done.

            boolean iio = false;
            boolean done = false;
            do {
              ch = PNGDecoder.decodeChunk(din);

              switch (ch.type) {
              case PNGDecoder.tRNS: iio = false; done = true; break;
              case PNGDecoder.IDAT: iio = true;  done = true; break;
              default:
              }
            } while (!done);

            useImageIO = iio;
          }

          if (useImageIO && iTXtBug) {
            // Filter out iTXt chunks on JVMs with the iTXt bug.
            rin.reset();
            rin = new RereadableInputStream(
              new PNGChunkSkipInputStream(skip_iTXt, rin));
            rin.mark(1);
          }
        }
        else {
          //
          // Non-PNGs
          //
          useImageIO = true;
        }
      }
      else {
        //
        // Non-PNGs
        //
        useImageIO = true;
      }

      rin.reset();

      if (!useImageIO) {
        rin.mark(4096);

        img = loadToolkit(name, rin);

        if (img == null) {
          // Toolkit failed. Reset the stream so ImageIO can have a go.
          rin.reset();
        }
      }

      if (img == null) {
        img = loadImageIO(name, rin);
      }

      rin.close();
      return img;
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
