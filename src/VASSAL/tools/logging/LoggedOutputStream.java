/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

public class LoggedOutputStream extends OutputStream {
  private final long pid;

  private final ByteArrayOutputStream buf = new ByteArrayOutputStream();

  public LoggedOutputStream(long pid) {
    this.pid = pid;
  }

  public synchronized void write(int b) {
    buf.write(b);
    if (b == '\n') flush(); 
  }

  public synchronized void write(byte b[], int off, int len) {
    flush();
    LogManager.enqueue(
      new LogEntry(pid, LogEntry.SYSTEM, null, new String(b, off, len)));
  }

  public synchronized void flush() {
    if (buf.size() > 0) {
      LogManager.enqueue(new LogEntry(pid, LogEntry.SYSTEM,
                                      null, new String(buf.toByteArray())));
      buf.reset();
    }
  } 
} 
