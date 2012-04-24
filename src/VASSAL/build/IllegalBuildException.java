/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build;

/**
 * An Exception indicating that an illegal operation was performed
 * when building a Buildable component.
 */
public class IllegalBuildException extends RuntimeException {
  private static final long serialVersionUID = 1L;

  public IllegalBuildException(String s) {
    super(s);
  }

  public IllegalBuildException(Throwable t) {
    super(t);
  }

  public IllegalBuildException(String s, Throwable t) {
    super(s, t);
  }
}
