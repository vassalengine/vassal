/*
 * $Id$
 *
 * Copyright (c) 2011 by Bob Davison
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

import org.junit.Test;

import static org.junit.Assert.*;

public class LabelerTest {

  @Test
  public void testPieceNameInLabelBug3463() {
    // Putting piece name in label was causing infinite recursion
    // as labeler sets a new name which may be based on the label
    final String pieceName = "Test Piece";
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;" + pieceName + ";");
    final Labeler labeler = new Labeler(Labeler.ID, piece);

    labeler.setLabel("$" + BasicPiece.PIECE_NAME + "$");

    assertEquals(pieceName, labeler.getLabel());
  }

}
