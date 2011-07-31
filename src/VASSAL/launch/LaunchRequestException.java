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

package VASSAL.launch;

import VASSAL.i18n.Resources;

/**
 * The {@link Exception} thrown by {@link LaunchRequest} when command line
 * arguments cannot be parsed.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LaunchRequestException extends Exception {
  private static final long serialVersionUID = 1L;

  /**
   * Constructs a new exception with the argument list interpolated
   * into the i18n string specified by the key.
   *
   * @param key {@link Resources} key
   * @param vals {@link Resources} arguments
   */
  public LaunchRequestException(String key, String... vals) {
    super(Resources.getString(key, (Object[]) vals));
  }
}
