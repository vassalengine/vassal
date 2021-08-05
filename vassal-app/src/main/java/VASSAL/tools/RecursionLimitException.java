/*
 *
 * Copyright (c) 2009 by Brent Easton
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
package VASSAL.tools;

import VASSAL.tools.RecursionLimiter.Loopable;

public class RecursionLimitException extends Exception {
  private static final long serialVersionUID = 1L;
  protected Loopable looper;

  protected String additionalErrorMessage;

  public RecursionLimitException(Loopable l) {
    looper = l;
    additionalErrorMessage = "";
  }

  public String getComponentTypeName() {
    return looper == null ? "" : looper.getComponentTypeName();
  }

  public String getComponentName() {
    return looper == null ? "" : looper.getComponentName();
  }

  public String getAdditionalErrorMessage() {
    return additionalErrorMessage;
  }

  public void setAdditionalErrorMessage(String additionalErrorMessage) {
    this.additionalErrorMessage = additionalErrorMessage;
  }
}
