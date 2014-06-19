/*
 * $Id$
 *
 * Copyright (c) 2000-2011 by Rodney Kinney, Jim Urbas, Pieter Geerkens
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

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Point;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import javax.swing.SwingUtilities;

import VASSAL.build.module.Map;

 /**
* Implements a psudo-cursor that follows the mouse cursor when user
* drags gamepieces. Supports map zoom by resizing cursor when it enters
* a drop target of type Map.View.
*
* @author Jim Urbas
* @version 0.4.2
*
*/
public class DragHandlerNoImage extends DragHandler {
  @Override
  public void dragGestureRecognized(DragGestureEvent dge) {
    final Point mousePosition = dragGestureRecognizedPrep(dge);
    if (mousePosition == null) return;

    makeDragCursor(dragPieceOffCenterZoom);
    setDrawWinToOwnerOf(dragWin);
    SwingUtilities.convertPointToScreen(mousePosition, drawWin);
    moveDragCursor(mousePosition.x, mousePosition.y);

    super.dragGestureRecognized(dge);
  }

  protected int getOffsetMult() {
    return 1;
  }

  @Override
  public void dragDropEnd(DragSourceDropEvent e) {
    removeDragCursor();
    super.dragDropEnd(e);
  }

  public void dragMouseMoved(DragSourceDragEvent e) {
    if (!e.getLocation().equals(lastDragLocation)) {
      lastDragLocation = e.getLocation();
      moveDragCursor(e.getX(), e.getY());
      if (dragCursor != null && !dragCursor.isVisible()) {
        dragCursor.setVisible(true);
      }
    }
  }

  public void dragEnter(DropTargetDragEvent e) {
    final Component newDropWin = e.getDropTargetContext().getComponent();
    if (newDropWin != dropWin) {
      final double newZoom = newDropWin instanceof Map.View
        ? ((Map.View) newDropWin).getMap().getZoom() : 1.0;
      if (Math.abs(newZoom - dragCursorZoom) > 0.01) {
        makeDragCursor(newZoom);
      }
      setDrawWinToOwnerOf(e.getDropTargetContext().getComponent());
      dropWin = newDropWin;
    }
    super.dragEnter(e);
  }

  public void drop(DropTargetDropEvent e) {
    removeDragCursor();
    super.drop(e);
  }
}
