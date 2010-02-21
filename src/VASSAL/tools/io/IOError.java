/*
 * $Id: Info.java 5960 2009-08-24 06:58:38Z swampwallaby $
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

package VASSAL.tools.io;

/**
 * Thrown when a serious I/O error has occurred. This class exists in Java
 * 1.6, so may be removed once we stop supporting Java 1.5.
 *
 * @author Joel Uckelman
 * @since 3.2
 */
public class IOError extends Error {
  private static final long serialVersionUID = 1L;

  public IOError(Throwable cause) {
    super(cause); 
  }
}
