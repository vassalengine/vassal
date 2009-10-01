/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

package VASSAL.tools.image.memmap;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.color.CMMException;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.SampleModel;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.stream.FileCacheImageInputStream;
import javax.imageio.stream.ImageInputStream;

import VASSAL.build.BadDataReport;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageLoader;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.image.UnrecognizedImageTypeException;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.RereadableInputStream;
import VASSAL.tools.io.TempFileManager;

public class MappedImageLoader extends ImageLoader {

  public static MappedBufferedImage getImage(String name, InputStream in)
                                                      throws ImageIOException {
    return INSTANCE.load(name, in);
  }

  protected static final MappedImageLoader INSTANCE = new MappedImageLoader(); 

  protected MappedImageLoader() {}

  @Override
  protected MappedBufferedImage loadImageIO(String name, InputStream in)
                                                           throws IOException { 
    final ImageInputStream iis = new FileCacheImageInputStream(in,
      TempFileManager.getInstance().getSessionRoot());

    final Iterator<ImageReader> i = ImageIO.getImageReaders(iis);
    if (!i.hasNext()) throw new UnrecognizedImageTypeException();

    final ImageReader reader = i.next();
    if (reader == null) return null;

    try {
      reader.setInput(iis);

      final int w = reader.getWidth(0);
      final int h = reader.getHeight(0);

      final ImageTypeSpecifier type = reader.getImageTypes(0).next();

      // get our ColorModel and SampleModel
      final ColorModel cm = type.getColorModel();
      final SampleModel sm =
        type.getSampleModel().createCompatibleSampleModel(w,h);
      
      MappedBufferedImage img = new MappedBufferedImage(cm, sm);
      
      final ImageReadParam param = reader.getDefaultReadParam();
      param.setDestination(img);

      BufferedImage src = null;
      try {
        img = (MappedBufferedImage) reader.read(0, param);
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
    finally {
      reader.dispose();
    }
  }

  @Override
  protected MappedBufferedImage load(String name, InputStream in)
                                                      throws ImageIOException {
    final MappedBufferedImage img = (MappedBufferedImage) super.load(name, in);

    try {
      return MappedImageUtils.toCompatibleImage(img);
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
  }
}
