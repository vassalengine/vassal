/*
 * $Id$
 *
 * Copyright (c) 2006 by Rodney Kinney
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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;


public class JarArchive extends DataArchive {

  protected String prefix;

  public JarArchive() {
    this(null);
  }
  
  public JarArchive(String prefix) {
    super();
    this.prefix = prefix;
  }

  @Override
  public URL getURL(String fileName) throws IOException {
    URL url = getClass().getResource(getAbsolutePath(fileName));
    if (url == null) {
      url = getURLFromExtension(fileName);
    }
    return url;
  }

  @Override
  public InputStream getFileStream(String file) throws IOException {
    InputStream in = getClass().getResourceAsStream(getAbsolutePath(file));
  
    if (in == null) {
      in = getFileStreamFromExtension(file);
    }
    return in;
  }

  protected String getAbsolutePath(String file) {
    String resource = prefix != null ? "/" + prefix + "/" + file : "/" + file;
    return resource;
  }

  @Override
  public String getName() {
    return prefix != null ? prefix : super.getName();
  }
}
