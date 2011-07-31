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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.concurrent.listener;

import java.io.PrintStream;

/**
 * Prints dots to a stream in response to events.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class DotPrinter<T> implements EventListener<T> {
  protected final PrintStream out;

  /**
   * Creates a <code>DotPrinter</code>.
   *
   * @param out the <code>PrintStream</code> which receives the dots
   */
  public DotPrinter(PrintStream out) {
    this.out = out;
  }

  /** {@inheritDoc} */
  public void receive(Object src, T event) {
    out.print('.');
  }
}
