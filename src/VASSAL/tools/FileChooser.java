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

import java.awt.Component;

import VASSAL.configure.DirectoryConfigurer;

/**
 * FileChooser provides a wrapper for {@link javax.swing.JFileChooser} and
 * {@link java.awt.FileDialog}, selecting whichever is preferred on the
 * user's OS. <code>FileChooser</code>'s methods mirror those of
 * <code>JFileChooser</code>.
 *
 * @author Joel Uckelman
 * @deprecated Moved to {@link VASSAL.tools.filechooser.FileChooser}.
 */
@Deprecated
public abstract class FileChooser extends VASSAL.tools.filechooser.FileChooser {
  private FileChooser(Component parent, DirectoryConfigurer pref) {
    super(parent, pref);
  }
}
