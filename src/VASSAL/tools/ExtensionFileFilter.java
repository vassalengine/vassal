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

/**
 * A generic by-extension FileFilter.
 *
 * @author Joel Uckelman
 * @deprecated Moved to {@link VASSAL.tools.filechooser.ExtensionFileFilter}.
 */
@Deprecated
public class ExtensionFileFilter extends VASSAL.tools.filechooser.ExtensionFileFilter {
   /**
    * @param desc The description of this filter.
    * @param types A list of the extensions accepted by this filter.
    */
   public ExtensionFileFilter(String desc, String[] types) {
      super(desc, types);
   }
}
