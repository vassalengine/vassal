/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
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
 * A {@link FileFilter} for VASSAL logs and saved games. Used by file
 * choosers to filter out files which aren't logs or saved games.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LogAndSaveFileFilter extends ExtensionFileFilter {
  public static final String[] types = { ".vlog", ".vsav" };

  public LogAndSaveFileFilter() {
    super("VASSAL Logs and Saved Games", types);
  }
}
