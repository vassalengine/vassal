/*
 * $Id$
 *
 * Copyright (c) 2006-2008 by Joel Uckelman
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

import java.io.File;

import org.apache.commons.lang.StringUtils;

import VASSAL.tools.ArrayUtils;

/**
 * A generic by-extension FileFilter.
 *
 * @author Joel Uckelman
 */
public class ExtensionFileFilter extends FileFilter {
  private final String[] types;
  private final String desc;

  /**
   * @param desc The description of this filter.
   * @param types A list of the extensions accepted by this filter.
   */
  public ExtensionFileFilter(String desc, String[] types) {
    super();
    this.desc = desc + " (*" + StringUtils.join(types, ", *") + ")";
    this.types = ArrayUtils.copyOf(types);
  }

  /**
   * @return Whether the given file is accepted by this filter.
   */
  public boolean accept(File f) {
    if (f.isDirectory()) return true;
    final String name = f.getName().toLowerCase();
    for (String type : types) {
      if (name.endsWith(type)) return true;
    }
    return false;
  }

  /**
   * @return The description of this filter.
   */
  public String getDescription() {
    return desc;
  }
}
