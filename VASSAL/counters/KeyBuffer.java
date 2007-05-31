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
package VASSAL.counters;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;


public class KeyBuffer {
  private static KeyBuffer theBuffer;
  private ArrayList<GamePiece> pieces;
  private BoundsTracker bounds;
  private Comparator pieceSorter = new PieceSorter();

  private KeyBuffer() {
    pieces = new ArrayList<GamePiece>();
    bounds = new BoundsTracker();
  }

  public static void init(KeyBuffer kb) {
    if (theBuffer == null)
      theBuffer = kb;
  }

  public static KeyBuffer getBuffer() {
    if (theBuffer == null) {
      theBuffer = new KeyBuffer();
    }
    return theBuffer;
  }

  public void add(GamePiece p) {
// FIXME: should we use a HashSet or LinkedHashSet instead to make contains()
// checks faster? Is insertion order important?
    if (p != null && !pieces.contains(p)) {
      pieces.add(p);
      p.setProperty(Properties.SELECTED, Boolean.TRUE);
    }
  }

  public void clear() {
// FIXME: Is this inefficient? Better to call clear()?
    while (!pieces.isEmpty()) {
      remove(pieces.get(pieces.size() - 1));
    }
  }

  public void remove(GamePiece p) {
    if (p != null) {
      pieces.remove(p);
      p.setProperty(Properties.SELECTED, null);
    }
  }

  public boolean contains(GamePiece p) {
    if (p instanceof Stack) {
      for (Enumeration e = ((Stack) p).getPieces(); e.hasMoreElements();) {
        if (!pieces.contains(e.nextElement())) {
          return false;
        }
      }
      return true;
    }
    else {
      return pieces.contains(p);
    }
  }

  public boolean isEmpty() {
    return pieces.isEmpty();
  }

  public Command keyCommand(javax.swing.KeyStroke stroke) {
    sort(pieceSorter);
    Command comm = new NullCommand();

    bounds.clear();

    // Copy contents into new list, because contents may change
    // as a result of key commands
    ArrayList<GamePiece> targets = new ArrayList<GamePiece>(pieces);
    for (GamePiece p : targets) {
      bounds.addPiece(p);
      p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p)); // save state prior to command
      Command c2 = p.keyEvent(stroke);
      comm = comm.append(c2);
      bounds.addPiece(p);
    }
    bounds.repaint();
    return comm;
  }

  public Enumeration getPieces() {
    return Collections.enumeration(pieces);
  }

  public void sort(Comparator comp) {
    Collections.sort(pieces, comp);
  }

  /**
   *
   * @param stack
   * @return true if a child of the specified Stack is selected
   */
  public boolean containsChild(Stack stack) {
    for (Enumeration e = stack.getPieces(); e.hasMoreElements();) {
      if (contains((GamePiece) e.nextElement())) {
        return true;
      }
    }
    return false;
  }
}
