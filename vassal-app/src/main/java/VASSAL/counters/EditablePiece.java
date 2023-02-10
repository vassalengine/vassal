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

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;

import static VASSAL.counters.Decorator.putOldProperties;

/**
 * If class implementing GamePiece also implements the EditablePiece interface, then
 * it can be manipulated from the Editor's Configuration Tree via the {@link PieceDefiner} dialog.
 *
 * All {@link Decorator} (Trait) classes with a no-arg constructor will appear in the
 * Available Traits list even if they don't implement EditablePiece. */
public interface EditablePiece extends GamePiece {

  /** A plain-English description of this type of trait/piece - includes data from fields where appropriate */
  String getDescription();

  /** Sets the information for this piece.  See {@link Decorator#myGetType}
   *  @param type a serialized configuration string to
   *              set the "type information" of this piece, which is
   *              information that doesn't change during the course of
   *              a single game (e.g. Image Files, Context Menu strings,
   *              etc). Typically ready to be processed e.g. by
   *              SequenceEncoder.decode() */
  void mySetType(String type);

  /** @return the configurer for this trait - the dialog which allows the editing the piece's type information */
  PieceEditor getEditor();

  /** @return the help file for this trait  */
  HelpFile getHelpFile();

  /**
   * Support for a basic-name-only description introduced later, so this default retrofits it from the full description
   * if an explicit one is not defined.
   * @return name of trait/piece type, w/o additional data
   */
  default String getBaseDescription() {
    final String desc = getDescription();
    if (desc.contains(" - ")) {
      return desc.substring(0, desc.indexOf(" - "));
    }
    return desc;
  }

  /**
   * Centralized method for preparing a piece to move. Writes the "Old Map" properties based on its current location,
   * optionally marks the piece as moved, and tells any deck its in that it is leaving.
   * @param c Command to which will be appended a command for recreating anything this method does
   * @param mark_moved If true the piece will be mark moved
   * @return A command for recreating anything this method does, appended to the command passed
   */
  @Override
  default Command prepareMove(Command c, boolean mark_moved) {
    final GamePiece outer = Decorator.getOutermost(this);
    if (mark_moved) {
      final ChangeTracker tracker = new ChangeTracker(outer);
      outer.setProperty(Properties.MOVED, Boolean.TRUE);
      c = c.append(tracker.getChangeCommand());
    }
    c = c.append(putOldProperties(this));

    final Stack parent = outer.getParent();
    if (parent != null) {
      c = c.append(parent.pieceRemoved(outer));
    }
    return c;
  }

  /**
   * Centralized method for finishing up after a piece moves. Optionally finds a new mat if needed,
   * and optionally applies any afterburner apply-on-move key for the piece's map.
   * @param c Command to which will be appended a command for recreating anything this method does
   * @param afterburner if true, apply the afterburner apply-on-move key for the piece's map
   * @param findmat if true, find a new mat for this piece if needed (if this piece is cargo)
   * @return A command for recreating anything this method does, appended to the command passed
   */
  @Override
  default Command finishMove(Command c, boolean afterburner, boolean findmat) {
    final GamePiece outer = Decorator.getOutermost(this);
    if (findmat) {
      // If a cargo piece has been moved, find it a new Mat if needed.
      c = MatCargo.findNewMat(c, outer);
    }

    // Apply "afterburner" apply-on-move key command
    if (afterburner) {
      final Map map = outer.getMap();
      if (map.getMoveKey() != null) {
        c = c.append(outer.keyEvent(map.getMoveKey()));
      }
    }

    return c;
  }
}
