/*
 * $Id: ImageFileFilter.java 2893 2008-01-27 20:15:23Z uckelman $
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

import VASSAL.tools.ExtensionFileFilter;

/**
 * A FileFilter for BMP images. Used by file choosers
 * to filter out files which aren't bitmaps.
 *
 * @author Michael Kiefte
 */
public class BitmapFileFilter extends ExtensionFileFilter {
   public static final String[] types = { ".bmp" };

   public BitmapFileFilter() {
      super("Bitmap files", types);
   }
}
