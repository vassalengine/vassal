/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
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

package VASSAL.tools.io;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.Future;

/**
 * A wrapper for returning processes from a {@link ProcessLauncher}.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ProcessWrapper {
  public final Future<Integer> future;
  public final InputStream stdout;
  public final InputStream stderr;
  public final OutputStream stdin;

  /**
   * Create a <code>ProcessWrapper</code>.
   *
   * @param future the future for the process
   * @param stdout the process' STDOUT
   * @param stderr the process' STDERR
   * @param stdin  the process' STDIN
   */
  ProcessWrapper(
    Future<Integer> future,
    InputStream stdout,
    InputStream stderr,
    OutputStream stdin)
  {
    this.future = future;
    this.stdout = stdout;
    this.stderr = stderr;
    this.stdin  = stdin;
  }
}
