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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.counters;

import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.KeyEvent;
import java.awt.Graphics;

import javax.swing.KeyStroke;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;

import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.tools.ErrorDialog;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * A map's apply-on-move key is dispatched back into the trait chain of the piece that just moved,
 * so a trait which moves the piece in response to that key arrives back where it started. Nothing
 * bounded that, and a module which closed the loop -- a Mat rotating its Cargo, a Send to Location
 * fired by a Trigger Action -- ran until the stack gave out.
 */
public class ApplyOnMoveRecursionTest {

  private static final KeyStroke MOVE_KEY = KeyStroke.getKeyStroke(KeyEvent.VK_M, 0);

  /** A trait which moves itself whenever it sees the map's apply-on-move key. */
  private static class SelfMover extends Decorator {
    private final Map map;
    int entries;

    SelfMover(Map map, GamePiece inner) {
      this.map = map;
      setInner(inner);
    }

    @Override
    public Map getMap() {
      return map;
    }

    @Override
    public Command myKeyEvent(KeyStroke stroke) {
      if (!MOVE_KEY.equals(stroke)) {
        return null;
      }
      ++entries;
      // What a moving trait does: let the move's post-processing run, afterburner included.
      return finishMove(new NullCommand(), true, false, false);
    }

    @Override public String getDescription() { return "Self Mover"; }
    @Override public String getName() { return "self mover"; }
    @Override public String myGetState() { return ""; }
    @Override public void mySetState(String s) { }
    @Override public String myGetType() { return ""; }
    @Override public KeyCommand[] myGetKeyCommands() { return new KeyCommand[0]; }
    @Override public Rectangle boundingBox() { return new Rectangle(); }
    @Override public Shape getShape() { return new Rectangle(); }
    @Override public void draw(Graphics g, int x, int y, java.awt.Component obs, double zoom) { }
    @Override public VASSAL.build.module.documentation.HelpFile getHelpFile() { return null; }
    @Override public void mySetType(String type) { }
  }

  @Test
  public void testApplyOnMoveCannotRecurseWithoutEnd() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class);
         MockedStatic<ErrorDialog> staticEd = Mockito.mockStatic(ErrorDialog.class)) {

      final DeckManager decks = mock(DeckManager.class);
      when(decks.checkEmptyDecks(any())).thenAnswer(inv -> inv.getArgument(0));

      final GameModule gm = mock(GameModule.class);
      when(gm.getDeckManager()).thenReturn(decks);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final Map map = mock(Map.class);
      when(map.getMoveKey()).thenReturn(MOVE_KEY);

      final SelfMover mover = new SelfMover(map, mock(GamePiece.class));

      // Without a limit this recurses until the stack is exhausted; the error the player sees is
      // then a StackOverflowError naming whatever happened to be executing at the bottom of it.
      mover.keyEvent(MOVE_KEY);

      assertTrue(mover.entries > 1, "the apply-on-move key should re-enter the trait at all");
      assertTrue(mover.entries <= 64,
        "but it must stop at the recursion limit, not at the end of the stack; was " + mover.entries);

      // The loop is reported as one, rather than crashing.
      staticEd.verify(() -> ErrorDialog.showDetails(
        any(Throwable.class), any(String.class), any(String.class), any(Object[].class)
      ));
    }
  }

  @Test
  public void testASingleApplyOnMoveStillRuns() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final DeckManager decks = mock(DeckManager.class);
      when(decks.checkEmptyDecks(any())).thenAnswer(inv -> inv.getArgument(0));

      final GameModule gm = mock(GameModule.class);
      when(gm.getDeckManager()).thenReturn(decks);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final Map map = mock(Map.class);
      // A map with no apply-on-move key: nothing is dispatched, and nothing is limited.
      when(map.getMoveKey()).thenReturn(null);

      final SelfMover mover = new SelfMover(map, mock(GamePiece.class));
      mover.keyEvent(MOVE_KEY);

      assertTrue(mover.entries == 1, "the trait runs once");
      assertFalse(VASSAL.tools.RecursionLimiter.isReportingInfiniteLoop());
    }
  }
}
