/*
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
package VASSAL.counters;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import javax.swing.JFrame;

import VASSAL.build.module.Map;

public class DragBuffer {
  private static final DragBuffer THE_BUFFER = new DragBuffer();

  private final List<GamePiece> pieces = new ArrayList<>();
  private MouseEvent lastRelease;
  private Component dropTarget;
  private MouseListener dropHandler;
  private Map dragFromMap;

  private DragBuffer() {

  }

  public static DragBuffer getBuffer() {
    return THE_BUFFER;
  }

  public void add(GamePiece p) {
    if (p != null &&
        !pieces.contains(p) &&
        !Boolean.TRUE.equals(p.getProperty(Properties.RESTRICTED_MOVEMENT))) {
      if (p instanceof Stack) {
        for (final GamePiece gamePiece : ((Stack) p).asList()) {
          if (Boolean.TRUE.equals(
                gamePiece.getProperty(Properties.RESTRICTED_MOVEMENT))) {
            return;
          }
        }
      }
      pieces.add(p);
      dragFromMap = p.getMap();
    }
  }

  public Map getFromMap() {
    return dragFromMap;
  }

  public void clear() {
    pieces.clear();
  }

  public void addDragSource(Component c) {
    c.addMouseListener(new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent e) {
        lastRelease = null;
        dropTarget = null;
        dropHandler = null;
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        e.getComponent().setCursor(null);
        final Component source = (Component) e.getSource();

        e.translatePoint(source.getLocationOnScreen().x,
                         source.getLocationOnScreen().y);

        if (dropTarget == null) {
          lastRelease = e;
        }
        else {
          e.translatePoint(-dropTarget.getLocationOnScreen().x,
                           -dropTarget.getLocationOnScreen().y);
          dropHandler.mouseReleased(e);
        }
      }
    });
  }

  public void addDropTarget(final Component c, final MouseListener l) {
    c.addMouseListener(new MouseAdapter() {
      @Override
      public void mouseEntered(MouseEvent e) {
        final Component source = (Component) e.getSource();
        if (source.isShowing()) {
          if (lastRelease != null) {
            e.translatePoint(source.getLocationOnScreen().x,
                             source.getLocationOnScreen().y);
            if (isCloseEnough(e.getPoint(), lastRelease.getPoint())) {
              e.translatePoint(-source.getLocationOnScreen().x,
                               -source.getLocationOnScreen().y);
              l.mouseReleased(e);
            }
          }
          else {
            dropTarget = source;
            dropHandler = l;
          }
        }
      }
    });
  }

  private boolean isCloseEnough(Point p1, Point p2) {
    return Math.abs(p1.x - p2.x) < 3
        && Math.abs(p1.y - p2.y) < 3;
  }

  public void remove(GamePiece p) {
    pieces.remove(p);
  }

  public boolean contains(GamePiece p) {
    return pieces.contains(p);
  }

  /**
   * @return true if the DragBuffer contains all members of Stack s
   * @param s Stack to test.
   */
  public boolean containsAllMembers(Stack s) {
    final List<GamePiece> members = s.asList();
    for (final GamePiece p : members) {
      if (!contains(p)) {
        return false;
      }
    }
    return true;
  }

  /**
   * @return an unmodifiable {@link List} of {@link GamePiece}s contained in
   * this {@link DragBuffer}
   */
  public List<GamePiece> asList() {
    return Collections.unmodifiableList(pieces);
  }

  public PieceIterator getIterator() {
    return new PieceIterator(pieces.iterator());
  }

  public boolean isEmpty() {
    return pieces.isEmpty();
  }

  public String contents() {
    String s = "";
    for (final Iterator<GamePiece> i = pieces.iterator(); i.hasNext(); ) {
      s = s.concat(i.next().getName());
      if (i.hasNext())
        s = s.concat(",");
    }
    return s;
  }

  public static void main(String[] args) {
    final JFrame f1 = new JFrame();
    f1.setSize(200, 200);
    f1.setVisible(true);
    final JFrame f2 = new JFrame();
    f2.setSize(200, 200);
    f2.setLocation(200, 0);
    f2.setVisible(true);
    final MouseListener l = new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent evt) {
        evt.translatePoint(((JFrame) evt.getSource()).getLocationOnScreen().x, ((JFrame) evt.getSource()).getLocationOnScreen().y);
        System.err.println("Press at " + evt.getPoint()); //$NON-NLS-1$//
      }

      @Override
      public void mouseReleased(MouseEvent evt) {
        //        evt.translatePoint(((JFrame)evt.getSource()).getLocationOnScreen().x,((JFrame)evt.getSource()).getLocationOnScreen().y);
        System.err.println("Release at " + evt.getPoint()); //$NON-NLS-1$//
      }

      @Override
      public void mouseEntered(MouseEvent evt) {
        evt.translatePoint(((JFrame) evt.getSource()).getLocationOnScreen().x, ((JFrame) evt.getSource()).getLocationOnScreen().y);
        System.err.println("Enter at " + evt.getPoint()); //$NON-NLS-1$//
      }
    };
    DragBuffer.getBuffer().addDragSource(f1);
    DragBuffer.getBuffer().addDropTarget(f2, l);
  }

  public Cursor createDragCursor(@SuppressWarnings("unused") Component comp) {
    Cursor c = null;
    if (!pieces.isEmpty()) {
      c = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
    }
    return c;
  }

  public void sort(Comparator<GamePiece> comp) {
    pieces.sort(comp);
  }
}
