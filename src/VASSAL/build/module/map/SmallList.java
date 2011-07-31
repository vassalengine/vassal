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
package VASSAL.build.module.map;

import java.awt.Graphics;
import java.awt.List;

/**
 * Bogus extension of list that takes up less space.
 */
public class SmallList extends List {
  private static final long serialVersionUID = 1L;

  public void paint(Graphics g) {
    while (getItemCount() > 0 &&
//     getSize().width > g.getFontMetrics().stringWidth(getItem(0))+2)
     getSize().width > 135)
      setSize(getSize().width-2,getSize().height);
    super.paint(g);
  }
}

