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

import VASSAL.build.module.documentation.HelpFile;

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
}
