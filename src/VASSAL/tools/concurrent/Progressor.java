/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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

package VASSAL.tools.concurrent;

/**
 * A progress handler.
 *
 * @author Joel Uckelman
 * @since 3.1.11
 * @deprecated Moved to {@link VASSAL.tools.swing.Progressor}.
 */
@Deprecated
public abstract class Progressor extends VASSAL.tools.swing.Progressor {
  /**
   * Creates a <code>Progressor</code> with the given bounds.
   *
   * @param init the initial progress value
   * @param max the maximum progress value
   *
   * @throws IllegalArgumentException if {@code init} is not in {@code [0,max]}
   * @throws IllegalArgumentException if {@code max &lt; 0}
   */
  public Progressor(int init, int max) {
    super(init, max);
  }
}
