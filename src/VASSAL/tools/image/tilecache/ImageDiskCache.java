/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman 
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
package VASSAL.tools.image.tilecache;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.zip.GZIPInputStream;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;
import VASSAL.tools.image.ImageUtils;

public class ImageDiskCache {
  private ImageDiskCache() {}

  public static BufferedImage getImage(String name, int tileX, int tileY,
                                        double scale) throws ImageIOException {
    final String cname = getCacheName(name, tileX, tileY, scale); 

    InputStream in = null;
    try {
      in = new GZIPInputStream(
             new BufferedInputStream(
               new FileInputStream(cname)));

      final ByteBuffer bb = ByteBuffer.wrap(IOUtils.toByteArray(in));
      in.close();

      final BufferedImage img = ImageUtils.createCompatibleImage(
        bb.getInt(), bb.getInt(), bb.get() > 0
      );

// FIXME: this might decelerate the image?
      final int[] data =
        ((DataBufferInt) img.getRaster().getDataBuffer()).getData();

      bb.slice().asIntBuffer().get(data);

      return img;
    }
    catch (IOException e) {
      throw new ImageIOException(cname, e);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  public static Dimension getImageSize(String name, int tileX, int tileY, double scale) throws ImageIOException {
    final String cname = getCacheName(name, tileX, tileY, scale); 

    DataInputStream in = null;
    try {
      in = new DataInputStream(
             new GZIPInputStream(
               new BufferedInputStream(
                 new FileInputStream(cname))));

      return new Dimension(in.readInt(), in.readInt());
    }
    catch (FileNotFoundException e) {
      throw new ImageNotFoundException(cname, e);
    }
    catch (IOException e) {
      throw new ImageIOException(cname, e);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  protected static String getCacheName(String name,
                                       int tileX, int tileY, double scale) {
    final GameModule g = GameModule.getGameModule();
    final String cpath = new File(Info.getConfDir(), "tiles/" +
      g.getGameName() + "_" + g.getGameVersion()).getAbsolutePath();

// FIXME: should name have "images" in it to start with?
    final String iname = name.startsWith("images/") ? name.substring(7) : name;

    final double div = 1.0/scale;

    return String.format(
      "%s/%s(%d,%d)@1:%d", cpath, iname, tileX, tileY, (int) div);
  }
}
