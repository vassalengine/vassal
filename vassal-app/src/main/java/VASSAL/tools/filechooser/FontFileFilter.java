/*
 *
 * Copyright (c) 2006-2023 by Joel Uckelman, The VASSAL Development Team
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

import VASSAL.i18n.Resources;

/**
 * A FileFilter for OTF and TTF files. Used by file choosers to
 * filter out files which aren't font files.
 *
 * @author Joel Uckelman
 */
public class FontFileFilter extends ExtensionFileFilter {
  public static final String[] types = {
    ".otf", ".ttf"  //NON-NLS
  };

  public FontFileFilter() {
    super(Resources.getString("Editor.FileFilter.font"), types);
  }
}
