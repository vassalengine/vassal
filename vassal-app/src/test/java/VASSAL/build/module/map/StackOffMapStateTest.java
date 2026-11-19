/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.index.IndexManager;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.preferences.Prefs;
import VASSAL.tools.SequenceEncoder;

import java.awt.Point;
import java.util.Arrays;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

/**
 * A Stack whose state says it is on no map must leave the map it was on.
 *
 * <p>Undo replays recorded states, and a Stack's state records which map it is
 * on — or that it is on none. Setting the map to null without leaving the map's
 * piece collection first left the Stack drawn, hit-tested and handed out by
 * {@link Map#findPiece}, while every piece in it answered {@code getMap() ==
 * null}: {@link Map#positionOf} then threw <i>Piece is not on this map</i> for a
 * piece the map itself had just supplied. That is vassalengine/vassal#12033 and
 * the five reports that followed it.</p>
 *
 * <p>The test harness is the one {@link StackMergeUndoTest} uses.</p>
 */
public class StackOffMapStateTest {

  private static class TestMap extends Map {
    void register() {
      idMgr.add(this);
    }

    void unregister() {
      idMgr.remove(this);
    }
  }

  private MockedStatic<GameModule> staticGm;
  private MockedStatic<GlobalOptions> staticGo;
  private GameModule gm;
  private TestMap map;
  private final java.util.Map<String, GamePiece> pieces = new HashMap<>();

  @BeforeEach
  public void setUp() {
    staticGo = Mockito.mockStatic(GlobalOptions.class);
    staticGo.when(GlobalOptions::getInstance).thenReturn(mock(GlobalOptions.class));

    final GameState gs = mock(GameState.class);
    when(gs.getPieceForId(any())).thenAnswer(
      i -> i.getArgument(0) == null ? null : pieces.get((String) i.getArgument(0)));
    doAnswer(i -> {
      pieces.remove(((GamePiece) i.getArgument(0)).getId());
      return null;
    }).when(gs).removePiece(any(GamePiece.class));

    gm = mock(GameModule.class);
    when(gm.getGameState()).thenReturn(gs);
    when(gm.getIndexManager()).thenReturn(mock(IndexManager.class));
    final Prefs prefs = mock(Prefs.class);
    when(prefs.getValue(anyString())).thenReturn(1);
    when(gm.getPrefs()).thenReturn(prefs);
    staticGm = Mockito.mockStatic(GameModule.class);
    staticGm.when(GameModule::getGameModule).thenReturn(gm);

    map = new TestMap();
    map.register();
    new StackMetrics().addTo(map);
  }

  @AfterEach
  public void tearDown() {
    map.unregister();
    staticGm.close();
    staticGo.close();
  }

  private BasicPiece newPiece(String id, Point pos) {
    final BasicPiece p = new BasicPiece();
    p.setId(id);
    p.setPosition(pos);
    pieces.put(id, p);
    return p;
  }

  private Stack newStack(String id, Point pos, GamePiece... contents) {
    final Stack s = new Stack();
    s.setId(id);
    pieces.put(id, s);
    s.setPosition(pos);
    map.addPiece(s);
    for (final GamePiece p : contents) {
      s.add(p);
    }
    return s;
  }

  /** The state a Stack encodes for itself when it is on no map. */
  private static String offMapState(Stack s) {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append("null").append(s.getPosition().x).append(s.getPosition().y);
    for (final GamePiece p : s.asList()) {
      se.append(p.getId());
    }
    se.append(Stack.HAS_LAYER_MARKER + 0);
    return se.getValue();
  }

  @Test
  public void aStackWhoseStateHasNoMapLeavesTheMapBug12033() {
    final Point pos = new Point(100, 200);
    final Stack stack = newStack("stack", pos, newPiece("piece", pos)); // NON-NLS
    assertTrue(Arrays.asList(map.getPieces()).contains(stack));

    stack.setState(offMapState(stack));

    assertNull(stack.getMap(), "the Stack still thinks it is on a map");
    assertFalse(Arrays.asList(map.getPieces()).contains(stack),
      "the Stack has no map but the map still holds it");
  }

  /**
   * The reported crash. The map hands out a piece that it then refuses to
   * locate: {@code IllegalArgumentException: Piece is not on this map}.
   */
  @Test
  public void everyPieceTheMapHoldsCanBeLocatedBug12033() {
    final Point gone = new Point(100, 200);
    final Stack leaving = newStack("leaving", gone, newPiece("adrift", gone)); // NON-NLS

    // A second Stack that keeps its map, so the check below is not vacuous
    final Point kept = new Point(300, 400);
    newStack("staying", kept, newPiece("settled", kept)); // NON-NLS

    leaving.setState(offMapState(leaving));

    int located = 0;
    for (final GamePiece p : map.getPieces()) {
      for (final GamePiece inner : ((Stack) p).asList()) {
        assertDoesNotThrow(() -> map.positionOf(inner),
          "the map holds a piece it refuses to locate");
        located++;
      }
    }
    assertTrue(located > 0, "nothing was checked");
  }

  /** Deck.setState is a copy of Stack.setState and had the same hole. */
  @Test
  public void aDeckWhoseStateHasNoMapLeavesTheMapBug12033() {
    final Deck deck = new Deck(gm);
    deck.setId("deck"); // NON-NLS
    pieces.put(deck.getId(), deck);
    deck.setPosition(new Point(100, 200));
    map.addPiece(deck);
    assertTrue(Arrays.asList(map.getPieces()).contains(deck));

    deck.setState("null;100;200;false"); // NON-NLS

    assertNull(deck.getMap(), "the Deck still thinks it is on a map");
    assertFalse(Arrays.asList(map.getPieces()).contains(deck),
      "the Deck has no map but the map still holds it");
  }
}
