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

import java.awt.Point;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.property.PersistentPropertyContainer;

/**
 * The KeyBuffer is the list of "currently selected pieces" in the VASSAL UI (map windows). Its somewhat confusing name
 * derives from the idea that if the player then presses a key, a key command will be sent to all of the pieces in the
 * buffer. KeyBuffer is a "singleton", so there is one for the whole app, across all map windows.
 */
public class KeyBuffer {
  private static KeyBuffer theBuffer;    // Our singleton buffer instance
  private final List<GamePiece> pieces;  // Our list of currently selected pieces
  private final BoundsTracker bounds;    // Visual boundaries tracker
  private final Comparator<GamePiece> pieceSorter = new PieceSorter(); // Used to sort pieces in visual order
  private final Point clickPoint;        // Most recent click point on the map (used to make this information
                                         // available to traits of pieces)

  private final List<PieceSlot> slots;   // Piece Slots in palettes that we have selected

  private boolean fromPalette = false; // True if we're selecting things from a Piece Palette

  private KeyBuffer() {
    pieces = new ArrayList<>();
    bounds = new BoundsTracker();
    clickPoint = new Point();

    slots  = new ArrayList<>();
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


  public PieceSlot getSlotForPiece(GamePiece piece) {
    if (!fromPalette) {
      return null;
    }
    final int index = pieces.indexOf(piece);
    return (index >= 0) ? slots.get(index) : null;
  }


  public void setClickPoint(Point p) {
    clickPoint.setLocation(p);
  }

  public Point getClickPoint() {
    return clickPoint;
  }

  /**
   * Adding a piece to the {@link KeyBuffer} "selects the piece" (and lets it know about in its SELECTED property)
   * @param p Piece to select
   */
  public void add(GamePiece p) {
// FIXME: should we use a HashSet or LinkedHashSet instead to make contains()
// checks faster? Is insertion order important?
    if (fromPalette) {
      clear();
    }
    if (p != null && !pieces.contains(p)) {
      pieces.add(p);
      p.setProperty(Properties.SELECTED, Boolean.TRUE);
    }
  }

  /**
   *
   * @param p piece to select
   * @param slot PieceSlot it comes from (so we can repaint it when selection later cleared)
   */
  public void addFromPalette(GamePiece p, PieceSlot slot) {
    if (!fromPalette) {
      clear();
    }
    if (p != null && !pieces.contains(p)) {
      pieces.add(p);
      p.setProperty(Properties.SELECTED, Boolean.TRUE);

      fromPalette = true;
      if ((slot != null) && !slots.contains(slot)) {
        slots.add(slot);
      }
    }
  }

  /**
   * If we had items selected from a palette, repaint them now that they aren't selected, and switch out of palette mode
   */
  public void cleansePalette() {
    fromPalette = false;
    for (final PieceSlot slot : slots) {
      slot.getComponent().repaint();
    }
    slots.clear();
  }

  /**
   * Deselects all pieces (removes them all from the {@link KeyBuffer})
   */
  public void clear() {
    for (final GamePiece p : pieces) {
      p.setProperty(Properties.SELECTED, null);
    }
    pieces.clear();
    cleansePalette();
  }

  /**
   * Deselect the specified piece -- removes it from the {@link KeyBuffer}
   * @param p piece to deselect
   */
  public void remove(GamePiece p) {
    if (p != null) {
      p.setProperty(Properties.SELECTED, null);
      pieces.remove(p);
    }
  }

  /**
   * Deselects a palette piece, repainting its palette slot
   * @param p piece
   * @param slot pieceSlot
   */
  public void removeFromPalette(GamePiece p, PieceSlot slot) {
    remove(p);
    if ((slot != null) && slots.contains(slot)) {
      slot.getComponent().repaint();
      slots.remove(slot);
      if (slots.isEmpty()) {
        fromPalette = false;
      }
    }
  }

  /**
   * Tells if a particular piece is selected (i.e. present in the KeyBuffer)
   * @param p piece to check
   * @return True if the piece is in the {@link KeyBuffer}, i.e. is selected
   */
  public boolean contains(GamePiece p) {
    if (p instanceof Stack) {
      return pieces.containsAll(((Stack) p).asList());
    }
    else {
      return pieces.contains(p);
    }
  }

  /**
   * @return true if the {@link KeyBuffer} is empty - i.e. no pieces are selected
   */
  public boolean isEmpty() {
    return pieces.isEmpty();
  }

  /**
   * Applies a key command to every selected piece (i.e. to piece in the {@link KeyBuffer})
   * @param stroke Keystroke to apply
   * @return Command that encapsulates any changes to the game state made while processing the key command, for replay on other clients or in logfile.
   */
  public Command keyCommand(javax.swing.KeyStroke stroke) {
    sort(pieceSorter);
    Command comm = new NullCommand();

    bounds.clear();

    // Copy contents into new list, because contents may change
    // as a result of key commands
    final ArrayList<GamePiece> targets = new ArrayList<>(pieces);
    // Reverse the order if this is a "Move Up" or "Move to Bottom" keystroke
    if (!targets.isEmpty()) {
      final GamePiece top = targets.get(0);
      if (top.getMap() != null) {
        if (stroke.equals(top.getMap().getStackMetrics().getMoveBottomKey())
            || stroke.equals(top.getMap().getStackMetrics().getMoveUpKey())) {
          Collections.reverse(targets);
        }
      }
    }
    for (final GamePiece p : targets) {
      bounds.addPiece(p);
      p.setProperty(Properties.SNAPSHOT, ((PropertyExporter) p).getProperties()); // save state prior to command

      // Send most recent click point location
      if (p instanceof PersistentPropertyContainer) {
        comm = comm.append(((PersistentPropertyContainer) p).setPersistentProperty(BasicPiece.CLICKED_X, String.valueOf(clickPoint.x)))
                   .append(((PersistentPropertyContainer) p).setPersistentProperty(BasicPiece.CLICKED_Y, String.valueOf(clickPoint.y)));
      }
      comm = comm.append(p.keyEvent(stroke));
    }
    bounds.repaint();
    return comm;
  }

  /**
   * Returns a list of all selected pieces.
   * @return an unmodifiable {@link List} of {@link GamePiece}s contained in
   * this {@link KeyBuffer}
   */
  public List<GamePiece> asList() {
    return Collections.unmodifiableList(pieces);
  }

  /**
   * Returns an iterator for all selected pieces.
   * @return an iterator for the {@link GamePiece}s contained in the {@link KeyBuffer}
   */
  public Iterator<GamePiece> getPiecesIterator() {
    return pieces.iterator();
  }

  /** @deprecated Use {@link #getPiecesIterator()} instead. */
  @Deprecated(since = "2021-12-01", forRemoval = true)
  public Enumeration<GamePiece> getPieces() {
    return Collections.enumeration(pieces);
  }

  /**
   * Sorts the selected pieces based on a particular Comparator
   * @param comp Comparator to use
   */
  public void sort(Comparator<GamePiece> comp) {
    pieces.sort(comp);
  }

  /**
   * Check if any member of the specified Stack is currently selected
   * @param stack Stack to check
   * @return true if a child of the specified Stack is selected
   */
  public boolean containsChild(Stack stack) {
    return stack.asList().stream().anyMatch(this::contains);
  }
}
