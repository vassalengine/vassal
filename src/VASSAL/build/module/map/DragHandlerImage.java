/*
 * $Id$
 *
 * Copyright (c) 2011 by Pieter Geerkens
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

import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragSourceDragEvent;

/** Implementation of AbstractDragHandler when DragImage is supported by JRE
 *
 * @Author Pieter Geerkens
 */
public class DragHandlerImage extends DragHandler {
  public void dragGestureRecognized(DragGestureEvent dge) {
    if (dragGestureRecognizedPrep(dge) == null) return;
    super.dragGestureRecognized(dge);
  }

  protected int getOffsetMult() {
    return -1;
  }

  public void dragMouseMoved(DragSourceDragEvent e) {}
}
