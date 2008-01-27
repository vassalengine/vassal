/*
 * $Id$
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

import java.io.File;

import VASSAL.tools.FileFilter;

/**
 * A generic by-extension FileFilter.
 *
 * @author uckelman
 */
public class ExtensionFileFilter extends FileFilter {
   private String[] types;
   private String desc;

   /**
    * @param desc The description of this filter.
    * @param types A list of the extensions accepted by this filter.
    */
   public ExtensionFileFilter(String desc, String[] types) {
      super();
      this.desc = desc;
      this.types = new String[types.length];
      System.arraycopy(types, 0, this.types, 0, types.length);
   }

   /**
    * @return Whether the given file is accepted by this filter.
    */
   public boolean accept(File f) {
      if (f.isDirectory()) return true;
      String name = f.getName().toLowerCase();
      for (int i = 0; i < types.length; i++) {
        if (name.endsWith(types[i])) return true;
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
