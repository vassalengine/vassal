/*
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
package VASSAL.build.module.map;

import java.awt.Dimension;
import java.awt.Point;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.tools.ErrorDialog;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * A Deck (DrawPile) or At-Start Stack reaches its Map through its ancestor in
 * the build tree, and that ancestor can be absent --- most plainly in the
 * extension Editor, where the Deck's parent is an ExtensionElement and the Map
 * it names lives in the module rather than in the tree being edited. Three
 * places dereferenced the Map without asking.
 *
 * See vassalengine/vassal#13610, #12869, #14185 and #13667.
 */
public class DeckWithNoMapTest {

  private static void set(Object target, Class<?> owner, String field, Object value) {
    try {
      final Field f = owner.getDeclaredField(field);
      f.setAccessible(true);
      f.set(target, value);
    }
    catch (ReflectiveOperationException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * A DrawPile made without its constructor, which needs a GameModule for the
   * Deck it keeps type information in.
   */
  private static DrawPile drawPile(Map map, Deck deck) {
    final DrawPile dp = mock(DrawPile.class, CALLS_REAL_METHODS);
    set(dp, SetupStack.class, "pos", new Point(100, 200));
    set(dp, DrawPile.class, "myDeck", deck);
    dp.setAncestor(map);
    return dp;
  }

  private static SetupStack setupStack(Map map, String owningBoardName) {
    final SetupStack s = new SetupStack();
    s.setAttribute(SetupStack.OWNING_BOARD, owningBoardName);
    s.setAncestor(map);
    return s;
  }

  // ---------------------------------------------------------------- #13610

  /**
   * The crash in #13610 and #12869: the map check sat in the branch which does
   * not dereference the map, so a Stack with no owning board and no map said
   * it was active and setup() then called map.getEdgeBuffer().
   */
  @Test
  public void aStackWithNoMapIsNotActiveBug13610() {
    assertThat(setupStack(null, null).isOwningBoardActive(), is(false));
  }

  @Test
  public void aStackWithNoMapAndAnOwningBoardIsNotActive() {
    assertThat(setupStack(null, "Board 1").isOwningBoardActive(), is(false));
  }

  /** The cases which were already right stay right. */
  @Test
  public void aStackOnAMapWithNoOwningBoardIsActive() {
    assertThat(setupStack(mock(Map.class), null).isOwningBoardActive(), is(true));
  }

  @Test
  public void aStackWhoseOwningBoardIsInPlayIsActive() {
    final Map map = mock(Map.class);
    when(map.getBoardByName("Board 1")).thenReturn(mock(Board.class));
    assertThat(setupStack(map, "Board 1").isOwningBoardActive(), is(true));
  }

  @Test
  public void aStackWhoseOwningBoardIsNotInPlayIsNotActive() {
    final Map map = mock(Map.class);
    when(map.getBoardByName("Board 1")).thenReturn(null);
    assertThat(setupStack(map, "Board 1").isOwningBoardActive(), is(false));
  }

  // ---------------------------------------------------------------- #14185

  /**
   * The crash in #14185 and #13667: Send to Deck ended in
   * getMap().placeOrMerge(...) with no check.
   */
  @Test
  public void sendingToADeckWithNoMapIsReportedNotThrownBug14185() {
    final Deck deck = mock(Deck.class);
    when(deck.indexOf(any())).thenReturn(-1);
    when(deck.isAccessible()).thenReturn(true);
    when(deck.getPosition()).thenReturn(new Point(0, 0));

    final DrawPile dp = drawPile(null, deck);
    set(dp, AbstractConfigurable.class, "name", "Discard");

    final List<BadDataReport> reports = new ArrayList<>();
    final Command c;

    try (MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {
      ed.when(() -> ErrorDialog.dataWarning(any(BadDataReport.class)))
        .thenAnswer(inv -> reports.add(inv.getArgument(0)));

      c = dp.addToContents(mock(GamePiece.class), true);
    }

    assertThat(c, instanceOf(NullCommand.class));
    assertThat(reports.size(), is(1));
    assertThat(reports.get(0).getData(), containsString("Discard"));
  }

  /** A Deck which does have a map is unaffected. */
  @Test
  public void sendingToADeckOnAMapStillPlacesThePiece() {
    final Deck deck = mock(Deck.class);
    when(deck.indexOf(any())).thenReturn(-1);
    when(deck.isAccessible()).thenReturn(true);
    when(deck.getPosition()).thenReturn(new Point(7, 9));

    final Map map = mock(Map.class);
    final Command placed = new NullCommand();
    when(map.placeOrMerge(any(), any())).thenReturn(placed);

    final DrawPile dp = drawPile(map, deck);
    final GamePiece p = mock(GamePiece.class);

    try (MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {
      assertThat(dp.addToContents(p, true), is(placed));
      ed.verifyNoInteractions();
    }
  }

  /** getPosition() reaches for the map too, on the same Send to Deck path. */
  @Test
  public void theDefaultPositionOfADeckWithNoMapIsItsOwnPosition() {
    assertThat(drawPile(null, null).getPosition(), is(new Point(100, 200)));
  }

  @Test
  public void theDefaultPositionOfADeckOnAMapIsOffsetByTheEdgeBuffer() {
    final Map map = mock(Map.class);
    when(map.getBoardByName(null)).thenReturn(null);
    when(map.getEdgeBuffer()).thenReturn(new Dimension(5, 11));

    assertThat(drawPile(map, null).getPosition(), is(new Point(105, 211)));
  }
}
