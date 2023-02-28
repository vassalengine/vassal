/*
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

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

/**
 * The file filter abstract base class for {link VASSAL.tools.FileChooser}.
 * This class joins together the {@link javax.swing.filechooser.FileFilter}
 * class and the {link java.awt.FilenameFilter} interface so that a
 * uniform file chooser may be used  with {link VASSAL.tools.FileChooser},
 * regardless of the actual (i.e., naive or Swing) file chooser displayed.
 *
 * @author uckelman
 */
@SuppressFBWarnings(value = "NM_SAME_SIMPLE_NAME_AS_SUPERCLASS")
public abstract class FileFilter extends javax.swing.filechooser.FileFilter
                                 implements java.io.FilenameFilter {
  /**
   * The accept test used by Swing file choosers.
   *
   * @return Whether the given file is accepted by this filter.
   */
  @Override
  public abstract boolean accept(File f);

  /**
   * The accept test used by AWT file choosers.
   *
   * @return Whether the given file is accepted by this filter.
   */
  @Override
  public boolean accept(File dir, String name) {
    return accept(new File(dir, name));
  }

  /**
   * @return A description of this filter to be displayed in the file chooser.
   */
  @Override
  public abstract String getDescription();
}
