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
package VASSAL.tools;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class URLUtils {
  private URLUtils() {}

  /**
   * @param f the file for which the URL is wanted
   * @return the URL of the file
   * @throws MalformedURLException if the URL can't be created
   */ 
  public static URL toURL(File f) throws MalformedURLException {
    String path = f.getAbsolutePath();

    if (File.separatorChar != '/') {
      path = path.replace(File.separatorChar, '/');
    }

    if (!path.startsWith("/")) { //$NON-NLS-1$
      path = "/" + path;         //$NON-NLS-1$
    }
    
    if (!path.endsWith("/") && f.isDirectory()) { //$NON-NLS-1$
      path += "/";                                //$NON-NLS-1$
    }

    return new URL("file", "", path); //$NON-NLS-1$ //$NON-NLS-2$
  }
}
