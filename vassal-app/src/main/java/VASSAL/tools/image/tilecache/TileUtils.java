/*
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
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.util.Arrays;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.codec.digest.DigestUtils;

import VASSAL.tools.image.ImageIOException;

/**
 * A class for reading and writing image tiles.
 *
 * The VASSAL tile format consists of the 18-byte header, followed by gzipped
 * 4-bpp image data. The header is the signature 'VASSAL' (6 bytes), the tile
 * width (4 bytes), the tile height (4 bytes), and the image type (4 bytes).
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class TileUtils {

  private TileUtils() {}

  /**
   * Reads an image tile file.
   *
   * @param src the path of the tile file
   * @return the tile image
   *
   * @throws ImageIOException if the read fails
   * @throws TileNotFoundException if the file isn't found
   */
  public static BufferedImage read(String src) throws ImageIOException {
    return read(new File(src));
  }

  /**
   * Reads an image tile file.
   *
   * @param src the path of the tile file
   * @return the tile image
   *
   * @throws ImageIOException if the read fails
   * @throws TileNotFoundException if the file isn't found
   */
  public static BufferedImage read(File src) throws ImageIOException {
    try (InputStream fin = Files.newInputStream(src.toPath());
         InputStream in = new BufferedInputStream(fin)) {
      return read(in);
    }
    catch (NoSuchFileException e) {
      throw new TileNotFoundException(src, e);
    }
    catch (IOException e) {
      throw new ImageIOException(src, e);
    }
  }

  /**
   * Reads an image tile.
   *
   * @param in a stream containing the tile data
   * @return the tile image
   *
   * @throws IOException if the read fails
   */
  public static BufferedImage read(InputStream in) throws IOException {
    ByteBuffer bb;

    // read the header
    final byte[] header = readHeader(in);
    bb = ByteBuffer.wrap(header);

    // validate the signature
    final byte[] sig = new byte[6];
    bb.get(sig);
    checkSignature(sig);

    // get the dimensions and type
    final int w = bb.getInt();
    final int h = bb.getInt();
    final int type = bb.getInt();

    // read the image data
    final byte[] cdata = in.readAllBytes();

    // decompress the image data
    try (InputStream bin = new ByteArrayInputStream(cdata);
         InputStream zin = new GZIPInputStream(bin)) {
      bb = ByteBuffer.wrap(zin.readAllBytes());
    }

    // build the image
    final BufferedImage img = new BufferedImage(w, h, type);

    // FIXME: This might decelerate the image? If so, then we should
    // make a copy.
    final DataBufferInt db = (DataBufferInt) img.getRaster().getDataBuffer();
    final int[] data = db.getData();

    final IntBuffer ib = bb.asIntBuffer();
    ib.get(data);

/*
    if (ib.hasRemaining()) {
      // buffer contains garbage at the end!
      throw new IOException("found " + (4*ib.remaining()) + " more bytes!");
    }
*/

    return img;
  }

  /**
   * Reads the tile header from the stream.
   *
   * @param in the stream
   * @return the header
   *
   * @throws IOException if the read fails or there is too little data
   */
  static byte[] readHeader(InputStream in) throws IOException {
    // read the header
    final byte[] header = new byte[18];
    if (in.readNBytes(header, 0, header.length) != header.length) {
      throw new IOException("header too short!");
    }

    return header;
  }

  /**
   * Checks that the given byte array equals the tile signature.
   *
   * @param sig the byte array to check
   *
   * @throws IOException if the byte array is not the tile signature
   */
  static void checkSignature(byte[] sig) throws IOException {
    if (!Arrays.equals(sig, "VASSAL".getBytes(StandardCharsets.UTF_8))) { //NON-NLS
      throw new IOException(
        "bad signature: got \"" + new String(sig, StandardCharsets.UTF_8) +
        "\", expected \"VASSAL\""
      );
    }
  }

  /**
   * Reads the dimensions of the tile in an image tile file.
   *
   * @param src the path of the tile file
   * @return the dimensions
   *
   * @throws ImageIOException if the read fails
   * @throws TileNotFoundException if the file isn't found
   */
  public static Dimension size(String src) throws ImageIOException {
    return size(new File(src));
  }

  /**
   * Reads the dimensions of the tile in an image tile file.
   *
   * @param src the path of the tile file
   * @return the dimensions
   *
   * @throws ImageIOException if the read fails
   * @throws TileNotFoundException if the file isn't found
   */
  public static Dimension size(File src) throws ImageIOException {
    try (InputStream in = Files.newInputStream(src.toPath())) {
      // NB: We don't buffer here because we're reading only 18 bytes.
      return size(in);
    }
    catch (NoSuchFileException e) {
      throw new TileNotFoundException(src, e);
    }
    catch (IOException e) {
      throw new ImageIOException(src, e);
    }
  }

  /**
   * Reads the dimensions of the tile from a stream.
   *
   * @param in the stream
   * @return the dimensions
   *
   * @throws IOException if the read fails
   */
  public static Dimension size(InputStream in) throws IOException {
    final ByteBuffer bb;

    // read the header
    final byte[] header = readHeader(in);
    bb = ByteBuffer.wrap(header);

    // validate the signature
    final byte[] sig = new byte[6];
    bb.get(sig);
    checkSignature(sig);

    // get the dimensions
    return new Dimension(bb.getInt(), bb.getInt());
  }

  /**
   * Write a tile image to a tile file.
   *
   * @param tile the image
   * @param dst the tile file
   *
   * @throws ImageIOException if the write fails
   */
  public static void write(BufferedImage tile, String dst)
                                                      throws ImageIOException {
    write(tile, new File(dst));
  }

  /**
   * Write a tile image to a tile file.
   *
   * @param tile the image
   * @param dst the tile file
   *
   * @throws ImageIOException if the write fails
   */
  public static void write(BufferedImage tile, File dst)
                                                      throws ImageIOException {
    try (OutputStream fout = Files.newOutputStream(dst.toPath());
         OutputStream out = new BufferedOutputStream(fout)) {
      write(tile, out);
    }
    catch (IOException e) {
      throw new ImageIOException(dst, e);
    }
  }

  /**
   * Write a tile image to a stream.
   *
   * @param tile the image
   * @param out the stream
   *
   * @throws ImageIOException if the write fails
   */
  public static void write(BufferedImage tile, OutputStream out)
                                                           throws IOException {
    ByteBuffer bb;

    // write the header
    bb = ByteBuffer.allocate(18);

    bb.put("VASSAL".getBytes(StandardCharsets.UTF_8)) //NON-NLS
      .putInt(tile.getWidth())
      .putInt(tile.getHeight())
      .putInt(tile.getType());

    out.write(bb.array());

    // write the tile data
    final DataBufferInt db = (DataBufferInt) tile.getRaster().getDataBuffer();
    final int[] data = db.getData();

    bb = ByteBuffer.allocate(4 * data.length);
    bb.asIntBuffer().put(data);

    final GZIPOutputStream zout = new GZIPOutputStream(out);
    zout.write(bb.array());
    zout.finish();
  }

  /**
   * Calculates the number of tiles needed to cover an image, summed over
   * all sizes from 1:1 to the vanishing point.
   *
   * @param i the image dimensions
   * @param t the tile dimensions
   * @return the number of tiles needed to cover the image
   *
   * @throws IllegalArgumentException if any argument is nonpositive
   */
  public static int tileCount(Dimension i, Dimension t) {
    return tileCount(i.width, i.height, t.width, t.height);
  }

  /**
   * Calculates the number of tiles needed to cover an image, summed over
   * all sizes from 1:1 to the vanishing point.
   *
   * @param iw the image width
   * @param ih the image height
   * @param tw the tile width
   * @param th the tile height
   * @return the number of tiles needed to cover the image
   *
   * @throws IllegalArgumentException if any argument is nonpositive
   */
  public static int tileCount(int iw, int ih, int tw, int th) {
    // TODO: Find a closed-form expression for this, if there is one.
    int tcount = 0;
    for (int div = 1; iw / div > 0 && ih / div > 0; div <<= 1) {
      tcount += tileCountAtScale(iw, ih, tw, th, div);
    }
    return tcount;
  }

  /**
   * Calculates the number of tiles needed to cover an image at a given
   * scale.
   *
   * @param i the image dimensions
   * @param t the tile dimensions
   * @param div the scale divisor
   * @return the number of tiles needed to cover the image
   *
   * @throws IllegalArgumentException if any argument is nonpositive
   */
  public static int tileCountAtScale(Dimension i, Dimension t, int div) {
    return tileCountAtScale(i.width, i.height, t.width, t.height, div);
  }

  /**
   * Calculates the number of tiles needed to cover an image at a given
   * scale.
   *
   * @param iw the image width
   * @param ih the image height
   * @param tw the tile width
   * @param th the tile height
   * @param div the scale divisor
   * @return the number of tiles needed to cover the image
   *
   * @throws IllegalArgumentException if any argument is nonpositive
   */
  public static int tileCountAtScale(int iw, int ih, int tw, int th, int div) {
    if (iw < 1) throw new IllegalArgumentException("iw = " + iw + " < 1");
    if (ih < 1) throw new IllegalArgumentException("ih = " + ih + " < 1");
    if (tw < 1) throw new IllegalArgumentException("tw = " + tw + " < 1");
    if (th < 1) throw new IllegalArgumentException("th = " + th + " < 1");
    if (div < 1) throw new IllegalArgumentException("div = " + div + " < 1");

    final int cols = (int) Math.ceil((double) (iw / div) / tw);
    final int rows = (int) Math.ceil((double) (ih / div) / th);
    return cols * rows;
  }

  /**
   * Gets the name of a tile file.
   *
   * @param iname the image name
   * @param tileX the X coordinate of the tile
   * @param tileY the Y coordinate of the tile
   * @param div the scale divisor
   * @return the name of the tile file
   */
  public static String tileName(String iname, int tileX, int tileY, int div) {
    final String sha = DigestUtils.sha1Hex(
      iname + "(" + tileX + "," + tileY + "@1:" + div
    );

    return sha.substring(0, 1) + '/' + sha.substring(0, 2) + '/' + sha;
  }
}
