/*
 * $Id$
 *
 * Copyright (c) 2008 by Michael Kiefte
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
package VASSAL.tools.filechooser;

/**
 * A FileFilter for BMP images. Used by file choosers
 * to filter out files which aren't Windows bitmaps.
 *
 * @author Michael Kiefte
 * @since 3.1.0
 */
public class BMPFileFilter extends ExtensionFileFilter {
  public static final String[] types = { ".bmp" };

  public BMPFileFilter() {
    super("Windows Bitmap files", types);
  }
}
