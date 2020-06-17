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

package VASSAL.tools.lang;

/**
 * Holds a mutable reference.
 *
 * This class is useful in cases where a reference to an object must be
 * passed to a method without the caller retaining a reference to that
 * object itself.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class Reference<T> {
  // FIXME: Replace this class with Mutable<T> from Apache Commons lang
  // once we move from 2.5 to 3.0.

  public T obj;

  /**
   * Creates a <code>Reference</code>.
   *
   * @param obj the referenced object
   */
  public Reference(T obj) {
    this.obj = obj;
  }
}
