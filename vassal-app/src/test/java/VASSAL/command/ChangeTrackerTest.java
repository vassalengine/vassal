/*
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
package VASSAL.command;

import java.awt.Point;

import VASSAL.build.GameModule;
import VASSAL.counters.BasicPiece;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

public class ChangeTrackerTest {

  /** BasicPiece that counts full-state snapshots taken of the piece */
  private static class CountingPiece extends BasicPiece {
    int stateCalls = 0;

    CountingPiece() {
      super(BasicPiece.ID + ";;;");
    }

    @Override
    public String getState() {
      stateCalls++;
      return super.getState();
    }
  }

  @Test
  public void testUnchangedReturnsNullWithOneMoreSnapshot() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final CountingPiece piece = new CountingPiece();
      final ChangeTracker tracker = new ChangeTracker(piece);

      assertNull(tracker.getChangeCommandIfChanged());
      // One snapshot when the tracker was created, one to compare
      assertEquals(2, piece.stateCalls);
    }
  }

  @Test
  public void testChangedReturnsChangePieceWithOneMoreSnapshot() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final CountingPiece piece = new CountingPiece();
      final ChangeTracker tracker = new ChangeTracker(piece);
      final String before = piece.getState();
      piece.stateCalls = 0;

      piece.setPosition(new Point(5, 7));
      final Command c = tracker.getChangeCommandIfChanged();

      assertTrue(c instanceof ChangePiece);
      assertEquals(before, ((ChangePiece) c).getOldState());
      // Only one snapshot taken to build the command
      assertEquals(1, piece.stateCalls);
      assertEquals(piece.getState(), ((ChangePiece) c).getNewState());
    }
  }
}
