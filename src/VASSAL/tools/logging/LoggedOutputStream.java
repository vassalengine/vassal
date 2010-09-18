/*
 * $Id$
 *
 * Copyright (c) 2008-2010 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.logging;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

public class LoggedOutputStream extends OutputStream {
  private static final Logger logger =
    LoggerFactory.getLogger(LoggedOutputStream.class);

  private final ByteArrayOutputStream buf = new ByteArrayOutputStream();

  @Override
  public synchronized void write(int b) {
    // don't write trailing newlines, logger adds those
    if (b == '\n') flush();
    else buf.write(b);
  }

  @Override
  public synchronized void write(byte b[], int off, int len) {
    // don't write trailing newlines, logger adds those
    if (b[off+len-1] == '\n') --len;

    buf.write(b, off, len);
    flush();
  }

  @Override
  public synchronized void flush() {
    if (buf.size() > 0) {
      logger.warn("STDERR: " + new String(buf.toByteArray())); 
      buf.reset();
    }
  } 
}
