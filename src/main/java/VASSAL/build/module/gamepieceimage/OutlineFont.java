/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.gamepieceimage;

import java.awt.Font;

public class OutlineFont extends Font {
  private static final long serialVersionUID = 1L;

  protected boolean outline;

  public OutlineFont(String name, int style, int size) {
    super(name, style, size);
  }

  public OutlineFont(String name, int style, int size, boolean outline) {
    super(name, style, size);
    this.outline = outline;
  }

  public boolean isOutline() {
    return outline;
  }

}
