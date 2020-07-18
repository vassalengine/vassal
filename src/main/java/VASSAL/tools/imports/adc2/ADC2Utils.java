/*
 * Copyright (c) 2007 by Michael Kiefte
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

package VASSAL.tools.imports.adc2;

import java.awt.Color;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;

import VASSAL.tools.imports.FileFormatException;

/**
 * Common utilities for importing ADC2 modules to VASSAL.
 *
 * @author Michael Kiefte
 *
 */

public class ADC2Utils {

  public static class NoMoreBlocksException extends EOFException {
    private static final long serialVersionUID = 1L;

    NoMoreBlocksException(String name) {
      super(name);
    }
  }

  public static final String MODULE_EXTENSION = ".ops";
  public static final String MAP_EXTENSION = ".map";
  public static final String SET_EXTENSION = ".set";

  public static final String MODULE_DESCRIPTION = "ADC2 Game Module";
  public static final String MAP_DESCRIPTION = "ADC2 Map Board";
  public static final String SET_DESCRIPTION = "ADC2 Symbol Set";
  static final String TYPE = "Type";

  // ADCs default pallet which is referenced by index.
  public static final Color[] defaultColorPallet = { new Color(0x000000),
    new Color(0x808080), new Color(0x800000), new Color(0x808000),
    new Color(0x008000), new Color(0x008080), new Color(0x000080),
    new Color(0x800080), new Color(0x808040), new Color(0x004040),
    new Color(0x0080ff), new Color(0x004080), new Color(0x4000ff),
    new Color(0x804000), new Color(0xffffff), new Color(0xc0c0c0),
    new Color(0xff0000), new Color(0xffff00), new Color(0x00ff00),
    new Color(0x00ffff), new Color(0x0000ff), new Color(0xff00ff),
    new Color(0xffff80), new Color(0x00ff80), new Color(0x80ffff),
    new Color(0x8080ff), new Color(0xff0080), new Color(0xff8040),
    new Color(0x010101), new Color(0x0e0e0e), new Color(0x1c1c1c),
    new Color(0x2a2a2a), new Color(0x383838), new Color(0x464646),
    new Color(0x545454), new Color(0x626262), new Color(0x707070),
    new Color(0x7e7e7e), new Color(0x8c8c8c), new Color(0x9a9a9a),
    new Color(0xa8a8a8), new Color(0xb6b6b6), new Color(0xc4c4c4),
    new Color(0xd2d2d2), new Color(0xe0e0e0), new Color(0xeeeeee),
    new Color(0x330000), new Color(0x660000), new Color(0x990000),
    new Color(0xcc0000), new Color(0xc1441a), new Color(0x003300),
    new Color(0x333300), new Color(0x663300), new Color(0x993300),
    new Color(0xcc3300), new Color(0xff3300), new Color(0x006600),
    new Color(0x336600), new Color(0x666600), new Color(0x996600),
    new Color(0xcc6600), new Color(0xff6600), new Color(0x009900),
    new Color(0x339900), new Color(0x669900), new Color(0x999900),
    new Color(0xcc9900), new Color(0xff9900), new Color(0x00cc00),
    new Color(0x33cc00), new Color(0x66cc00), new Color(0x99cc00),
    new Color(0xcccc00), new Color(0xffcc00), new Color(0x00ea00),
    new Color(0x33ff00), new Color(0x66ff00), new Color(0x99ff00),
    new Color(0xccff00), new Color(0xffff00), new Color(0x000033),
    new Color(0x330033), new Color(0x660033), new Color(0x990033),
    new Color(0xcc0033), new Color(0xff0033), new Color(0x003333),
    new Color(0x663333), new Color(0x993333), new Color(0xcc3333),
    new Color(0xff3333), new Color(0x006633), new Color(0x336633),
    new Color(0x666633), new Color(0x996633), new Color(0xcc6633),
    new Color(0xff6633), new Color(0x009933), new Color(0x339933),
    new Color(0x669933), new Color(0x999933), new Color(0xcc9933),
    new Color(0xff9933), new Color(0x00cc33), new Color(0x33cc33),
    new Color(0x66cc33), new Color(0x99cc33), new Color(0xcccc33),
    new Color(0xffcc33), new Color(0x00ff33), new Color(0x33ff33),
    new Color(0x66ff33), new Color(0x99ff33), new Color(0xccff33),
    new Color(0xffff33), new Color(0x000066), new Color(0x330066),
    new Color(0x660066), new Color(0x990066), new Color(0xcc0066),
    new Color(0xff0066), new Color(0x003366), new Color(0x333366),
    new Color(0x663366), new Color(0x993366), new Color(0xcc3366),
    new Color(0xff3366), new Color(0x006666), new Color(0x336666),
    new Color(0x996666), new Color(0xcc6666), new Color(0xff6666),
    new Color(0x009966), new Color(0x339966), new Color(0x669966),
    new Color(0x999966), new Color(0xcc9966), new Color(0xff9966),
    new Color(0x00cc66), new Color(0x33cc66), new Color(0x66cc66),
    new Color(0x99cc66), new Color(0xcccc66), new Color(0xffcc66),
    new Color(0x00ff66), new Color(0x33ff66), new Color(0x66ff66),
    new Color(0x99ff66), new Color(0xccff66), new Color(0xffff66),
    new Color(0x000099), new Color(0x330099), new Color(0x660099),
    new Color(0x990099), new Color(0xcc0099), new Color(0xff0099),
    new Color(0x003399), new Color(0x333399), new Color(0x663399),
    new Color(0x993399), new Color(0xcc3399), new Color(0xff3399),
    new Color(0x006699), new Color(0x336699), new Color(0x666699),
    new Color(0x996699), new Color(0xcc6699), new Color(0xff6699),
    new Color(0x009999), new Color(0x339999), new Color(0x669999),
    new Color(0xcc9999), new Color(0xff9999), new Color(0x00cc99),
    new Color(0x33cc99), new Color(0x66cc99), new Color(0x99cc99),
    new Color(0xcccc99), new Color(0xffcc99), new Color(0x00ff99),
    new Color(0x33ff99), new Color(0x66ff99), new Color(0x99ff99),
    new Color(0xccff99), new Color(0xffff99), new Color(0x0000cc),
    new Color(0x3300cc), new Color(0x6600cc), new Color(0x9900cc),
    new Color(0xcc00cc), new Color(0xff00cc), new Color(0x0033cc),
    new Color(0x3333cc), new Color(0x6633cc), new Color(0x9933cc),
    new Color(0xcc33cc), new Color(0xff33cc), new Color(0x0066cc),
    new Color(0x3366cc), new Color(0x6666cc), new Color(0x9966cc),
    new Color(0xcc66cc), new Color(0xff66cc), new Color(0x0099cc),
    new Color(0x3399cc), new Color(0x6699cc), new Color(0x9999cc),
    new Color(0xcc99cc), new Color(0xff99cc), new Color(0x00cccc),
    new Color(0x33cccc), new Color(0x66cccc), new Color(0x99cccc),
    new Color(0xffcccc), new Color(0x00ffcc), new Color(0x33ffcc),
    new Color(0x66ffcc), new Color(0x99ffcc), new Color(0xccffcc),
    new Color(0xffffcc), new Color(0x0000ff), new Color(0x3300ff),
    new Color(0x6600ff), new Color(0x9900ff), new Color(0xcc00ff),
    new Color(0xff00ff), new Color(0x0033ff), new Color(0x3333ff),
    new Color(0x6633ff), new Color(0x9933ff), new Color(0xcc33ff),
    new Color(0xff33ff), new Color(0x0066ff), new Color(0x3366ff),
    new Color(0x6666ff), new Color(0x9966ff), new Color(0xcc66ff),
    new Color(0xff66ff), new Color(0x0099ff), new Color(0x3399ff),
    new Color(0x6699ff), new Color(0x9999ff), new Color(0xcc99ff),
    new Color(0xff99ff), new Color(0x00ccff), new Color(0x33ccff),
    new Color(0x66ccff), new Color(0x99ccff), new Color(0xccccff),
    new Color(0xffccff), new Color(0x00ffff), new Color(0x33ffff),
    new Color(0x66ffff), new Color(0x99ffff), new Color(0xccffff) };

  // can never be instantiated
  private ADC2Utils() {}

  private static final long serialVersionUID = 1L;

  /**
   * Read a base-250 2-byte big-endian word from a <code>DataInputStream</code>.
   * This is the default (and only) encoding for words imported modules.
   */
  static int readBase250Word(DataInputStream in) throws IOException {
    return in.readUnsignedByte() * 250 + in.readUnsignedByte();
  }

  /**
   * Read a base-250 4-byte big-endian word from a <code>DataInputStream</code>.
   * This is the default encoding for integers in ADC2 modules (why?).
   */
  static int readBase250Integer(DataInputStream in) throws IOException {
    return readBase250Word(in) * 62500 + readBase250Word(in);
  }

  final static int BLOCK_SEPARATOR = -2;

  /**
   * Read a block separator byte from an import module file and throw an exception if it
   * doesn't match.
   */
  static void readBlockHeader(DataInputStream in, String string) throws IOException {
    try {
      int header = in.readByte();
      if (header != BLOCK_SEPARATOR)
        throw new FileFormatException("Invalid " + string + " block header.");
    }
    // FIXME: review error message
    catch (EOFException e) {
      throw new NoMoreBlocksException(string);
    }
    if (in.available() == 0)
      throw new NoMoreBlocksException(string);
  }

  /**
   * Returns a color from the default ADC pallet
   */
  public static Color getColorFromIndex(int index) {
    assert (index >= 0 && index < defaultColorPallet.length);
    return defaultColorPallet[index];
  }
}
