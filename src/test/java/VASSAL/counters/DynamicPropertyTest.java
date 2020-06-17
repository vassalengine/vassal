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


public class DynamicPropertyTest {

  @Test
  public void testFormatInitialValueRFE3472(){
    // Initial value of DynamicProperty was not getting evaluated
    final String pieceName = "Test Piece";
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;" + pieceName + ";");
    final DynamicProperty dp = new DynamicProperty(DynamicProperty.ID, piece);

    dp.mySetState("$" + BasicPiece.PIECE_NAME + "$");

    assertEquals(pieceName, dp.getValue());
  }

  @Test
  public void testNonStringSetPropertyBug3479() {
    // If a dynamic property has a name of "Moved" then setProperty can be
    // called with a Boolean
    final String propName = "Moved";
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;");
    final DynamicProperty dp = new DynamicProperty(DynamicProperty.ID + propName, piece);

    dp.setProperty(propName, Boolean.TRUE);

    assertEquals(Boolean.TRUE.toString(), dp.getProperty(propName));
  }

  @Test
  public void testNullSetProperty() {
    // setProperty of null should result in getProperty of empty string
    final String propName = "Name";
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;");
    final DynamicProperty dp = new DynamicProperty(DynamicProperty.ID + propName, piece);

    dp.setProperty(propName, null);

    assertEquals("", dp.getProperty(propName));
  }
}
