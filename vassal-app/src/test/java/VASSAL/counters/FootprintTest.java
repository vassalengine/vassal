/*
 * Copyright 2020 Vassal Development Team
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

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.index.IndexManager;
import VASSAL.build.module.map.PieceMover;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.SquareGridNumbering;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.NamedKeyStroke;

import java.awt.Color;
import java.awt.Point;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static VASSAL.counters.Properties.IGNORE_GRID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

public class FootprintTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Footprint trait = new Footprint();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Complex test
    trait = new Footprint();
    trait.trailKey = NamedKeyStroke.of("testkey"); // NON-NLS
    trait.menuCommand = "xyzzy"; // NON-NLS
    trait.initiallyVisible = true;
    trait.globallyVisible = true;
    trait.keepLastPositionOnly = true;
    trait.circleRadius = 10;
    trait.fillColor = Color.blue;
    trait.lineColor = Color.cyan;
    trait.selectedTransparency = 42;
    trait.unSelectedTransparency = 84;
    trait.edgePointBuffer = 3;
    trait.edgeDisplayBuffer = 14;
    trait.lineWidth = 2;
    trait.trailKeyOn = NamedKeyStroke.of("keyOn"); // NON-NLS
    trait.trailKeyOff = NamedKeyStroke.of("keyOff"); // NON-NLS
    trait.trailKeyClear = NamedKeyStroke.of("keyClear"); // NON-NLS
    trait.description = "plover"; // NON-NLS

    trait.globalVisibility = true;
    trait.startMapId = "map1"; // NON-NLS
    trait.pointList = new ArrayList<>();
    trait.pointList.add(new Point(10, 20));
    trait.pointList.add(new Point(110, 120));
    trait.pointList.add(new Point(210, 220));

    serializeTest("Complex test", trait); // NON-NLS


  }

  // Common setup for tests requiring a GameModule object.
  private void runWithGameModule(Runnable testFunc) {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      GameModule gm = mock(GameModule.class);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      // Map.setUpView requires Prefs
      Prefs prefs = new Prefs(new PrefsEditor(), "");
      staticGm.when(gm::getPrefs).thenReturn(prefs);
      // PieceMover.addTo requires GameState
      GameState gameState = new GameState();
      staticGm.when(gm::getGameState).thenReturn(gameState);
      // required by PieceMover.movedPiece
      DeckManager deckManager = new DeckManager();
      staticGm.when(gm::getDeckManager).thenReturn(deckManager);
      // Required by Map.addPiece
      IndexManager indexManager = new IndexManager();
      staticGm.when(gm::getIndexManager).thenReturn(indexManager);

      // Turn off auto reporting since Map.idMgr is not initialized.
      GlobalOptions.getInstance().setAttribute("autoReport", "Never");

      testFunc.run();
    }
  }

  // Create a board with a square grid.
  private Board makeBoard(String name) {
    final Board b = new Board();
    b.setConfigureName(name);
    SquareGrid grid = new SquareGrid();
    SquareGridNumbering numbering = new SquareGridNumbering();
    numbering.addTo(grid);
    b.setGrid(grid);
    return b;
  }

  // Check for non-movement where drag operation drops piece on its original location.
  @Test
  public void dragToSameLocationExpectNoTrail() {
    runWithGameModule(() -> {
      final Map map = new Map();
      final Board b = makeBoard("board");
      b.setMap(map);
      map.setBoards(new ArrayList<>(List.of(b)));

      Footprint trait = new Footprint();
      // Initialize with movement trail enabled (i.e. the field after the description)..
      trait.mySetType("footprint;;Name;true;false;10;255,255,255;0,0,0;100;50;20;30;1.0;;;;description;true");
      trait.setInner(new DummyPiece());
      trait.setId("footprint");
      trait.setMap(map);
      trait.setProperty(IGNORE_GRID, true);

      Point p = trait.getPosition();
      PieceMover pm = new PieceMover();
      pm.addTo(map);
      DragBuffer.getBuffer().clear();
      DragBuffer.getBuffer().add(trait);
      // Drag and drop the piece into the exact same spot.
      pm.movePieces(map, p);
      assertEquals(0, trait.pointList.size());
    });
  }

  // Move the piece within the same location.
  // Movement trail consists of only the initial location.
  @Test
  public void moveWithinSameLocationExpectEmptyTrail() {
    runWithGameModule(() -> {
      final Map map = new Map();
      final Board b = makeBoard("board");
      b.setMap(map);
      map.setBoards(new ArrayList<>(List.of(b)));

      Footprint trait = new Footprint();
      // Initialize with movement trail enabled (i.e. the field after the description)..
      trait.mySetType("footprint;;Name;true;false;10;255,255,255;0,0,0;100;50;20;30;1.0;;;;description;true");
      trait.setInner(new DummyPiece());
      trait.setId("footprint");
      trait.setMap(map);
      trait.setMoved(false);    // Reset movement trail
      trait.setProperty(IGNORE_GRID, true);

      Point p = trait.getPosition();
      PieceMover pm = new PieceMover();
      pm.addTo(map);
      DragBuffer.getBuffer().clear();
      DragBuffer.getBuffer().add(trait);
      Point first = new Point(p);
      // Multiple drag/drop actions without leaving the initial Location Name.
      for (int i = 0; i < 4; ++i) {
        p = new Point(p.x + 1, p.y + 1);
        pm.movePieces(map, p);
        trait.setPosition(p);
      }
      assertEquals(0, trait.pointList.size());
    });
  }

  // Move in a circular path returning over the start location.

  @Test
  public void moveCircularPathExpectKeepAllPoints() {
    runWithGameModule(() -> {
      final Map map = new Map();
      final Board b = makeBoard("board");
      b.setMap(map);
      map.setBoards(new ArrayList<>(List.of(b)));
      int deltaX = (int) ((SquareGrid) b.getGrid()).getDx();
      int deltaY = (int) ((SquareGrid) b.getGrid()).getDy();

      Footprint trait = new Footprint();
      trait.mySetType("footprint;;Name;true;false;10;255,255,255;0,0,0;100;50;20;30;1.0;;;;description;true");
      trait.setInner(new DummyPiece());
      trait.setId("footprint");
      trait.setMap(map);
      trait.setProperty(IGNORE_GRID, true);

      // Define positions centered within each square.
      List<Point> points = new ArrayList<>();
      points.add(new Point(deltaX / 2, deltaY / 2));
      points.add(new Point(deltaX * 3 / 2, deltaY / 2));
      points.add(new Point(deltaX * 3 / 2, deltaY * 3 / 2));
      points.add(new Point(deltaX / 2, deltaY / 2));
      points.add(new Point(deltaX * 3 / 2, deltaY * 3 / 2));

      PieceMover pm = new PieceMover();
      pm.addTo(map);
      DragBuffer.getBuffer().clear();
      DragBuffer.getBuffer().add(trait);

      trait.setPosition(points.get(0));
      trait.setMoved(false);    // Reset movement trail
      for (Point p : points) {
        pm.movePieces(map, p);
        trait.setPosition(p);
      }
      assertEquals(4, trait.pointList.size());
      for (int i = 0; i < trait.pointList.size(); ++i) {
        assertEquals(points.get(i), trait.pointList.get(i));
      }
    });
  }

  // Move in a circular path returning to the start location.
  @Test
  public void expectToKeepOffBoardMovement() {
    runWithGameModule(() -> {
      final Map map = new Map();
      final Board b = makeBoard("board");
      b.setMap(map);
      map.setBoards(new ArrayList<>(List.of(b)));
      int deltaX = (int) ((SquareGrid) b.getGrid()).getDx();
      int deltaY = (int) ((SquareGrid) b.getGrid()).getDy();

      Footprint trait = new Footprint();
      trait.mySetType("footprint;;Name;true;false;10;255,255,255;0,0,0;100;50;20;30;1.0;;;;description;true");
      trait.setInner(new DummyPiece());
      trait.setId("footprint");
      trait.setMap(map);
      trait.setProperty(IGNORE_GRID, true);

      // Define positions centered within each square.
      List<Point> points = new ArrayList<>();
      points.add(new Point(deltaX / 2, deltaY / 2));
      points.add(new Point( b.getSize().width + 1, deltaY / 2));    // off-board
      points.add(new Point(b.getSize().width + 1, deltaY * 3 / 2)); // off-board
      points.add(new Point(deltaX / 2, deltaY * 3 / 2));

      PieceMover pm = new PieceMover();
      pm.addTo(map);
      DragBuffer.getBuffer().clear();
      DragBuffer.getBuffer().add(trait);

      trait.setPosition(points.get(0));
      trait.setMoved(false);    // Reset movement trail
      for (Point p : points) {
        pm.movePieces(map, p);
        trait.setPosition(p);
      }
      assertEquals(3, trait.pointList.size());
      for (int i = 0; i < trait.pointList.size(); ++i) {
        assertEquals(points.get(i), trait.pointList.get(i));
      }
    });
  }

  static class DummyPiece extends BasicPiece {
  }
}
