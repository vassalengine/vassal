/*
 *
 * Copyright (c) 2026 by the VASSAL Development Team
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
package VASSAL.build.widget;

import java.awt.event.MouseEvent;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;

public class PieceSlotTest {

  /**
   * On platforms where the popup trigger is the press rather than the release,
   * right clicking a slot with no piece passed null to MenuDisplayer.createPopup:
   *
   * <pre>
   * java.lang.NullPointerException: Cannot invoke "VASSAL.counters.GamePiece.getProperty(Object)" because "target" is null
   *   at VASSAL.build.module.map.MenuDisplayer.createPopup(MenuDisplayer.java:138)
   *   at VASSAL.build.module.map.MenuDisplayer.createPopup(MenuDisplayer.java:126)
   *   at VASSAL.build.widget.PieceSlot.doPopup(PieceSlot.java:384)
   *   at VASSAL.build.widget.PieceSlot.mousePressed(PieceSlot.java:409)
   * </pre>
   */
  @Test
  public void testPopupTriggerPressOnEmptySlotDoesNotThrow() {
    final PieceSlot slot = new PieceSlot();
    assertNull(slot.getPiece());

    final MouseEvent press = new MouseEvent(slot.getComponent(), MouseEvent.MOUSE_PRESSED, 1L, 0,
        10, 10, 1, true, MouseEvent.BUTTON3);
    assertDoesNotThrow(() -> slot.mousePressed(press));
  }
}
