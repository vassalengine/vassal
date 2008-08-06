/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
 * General-purpose exception indicating that VASSAL has encountered data that's inconsistent with the current module.
 * A typical example would be failing to find a map/board/image/prototype from the supplied name.  Covers a variety of 
 * situations where the most likely cause is a module version compatibility issue.
 * 
 * This exception handled errors that occur during game play, as opposed to {@link IllegalBuildException}, 
 * which covers errors when building a module
 * @author rodneykinney
 *
 */
public class BadDataException extends RuntimeException {

  public BadDataException() {
    super();
  }

  public BadDataException(String message, Throwable cause) {
    super(message, cause);
  }

  public BadDataException(String message) {
    super(message);
  }

  public BadDataException(Throwable cause) {
    super(cause);
  }

}
