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
 * General-purpose condition indicating that VASSAL has encountered data that's inconsistent with the current module.
 * A typical example would be failing to find a map/board/image/prototype from the supplied name.  Covers a variety of 
 * situations where the most likely cause is a module version compatibility issue.
 * 
 * This is for recoverable errors that occur during game play, as opposed to {@link IllegalBuildException}, 
 * which covers errors when building a module
 * @see ErrorDialog.dataError()
 * @author rodneykinney
 *
 */
public class BadDataReport {
  private String message;
  private String data;
  private Throwable cause;

  public BadDataReport() {
  }

  public BadDataReport(String message, String data, Throwable cause) {
    this.message = message;
    this.cause = cause;
    this.data = data;
  }

  public String getMessage() {
    return message;
  }

  public Throwable getCause() {
    return cause;
  }

  public String getData() {
    return data;
  }

}
