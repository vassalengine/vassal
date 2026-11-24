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
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.index.IndexManager;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.SquareGridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.command.Command;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.NamedKeyStroke;

import java.awt.Point;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.KeyStroke;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class ReportStateTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    ReportState trait = new ReportState();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    //
    trait = new ReportState();
    trait.keys = new NamedKeyStroke[] { NamedKeyStroke.of("key1"), NamedKeyStroke.of("key2") };
    trait.reportFormat = "xyzzy";
    trait.cycleDownKeys = new NamedKeyStroke[] { NamedKeyStroke.of("cycle1"), NamedKeyStroke.of("cycle2") };
    trait.cycleReportFormat = new String[] { "format1", "format2" };
    trait.description = "plover";

    serializeTest("Complex trait", trait); // NON-NLS

  }

  // Create and add a report trait to the GamePiece.
  private ReportState buildReportState(String keyCommand, String expression, GamePiece piece) {
    ReportState report = new ReportState();
    report.setInner(piece);
    // Build a type string for a report trait using the UNKNOWN key code.
    String type = String.format("%s57358\\,0\\,%s;%s;", ReportState.ID, keyCommand, expression); // NON-NLS
    report.mySetType(type);
    return report;
  }

  @Test
  public void testReportTraitSumAndCount() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      // PlaceAt requires GameState
      final GameState gameState = new GameState();
      staticGm.when(gm::getGameState).thenReturn(gameState);
      final Chatter chatter = mock(Chatter.class);
      staticGm.when(gm::getChatter).thenReturn(chatter);
      // Map.setUpView requires Prefs
      final Prefs prefs = new Prefs(new PrefsEditor(), "");
      staticGm.when(gm::getPrefs).thenReturn(prefs);
      // Required by Map.addPiece
      final IndexManager indexManager = new IndexManager();
      staticGm.when(gm::getIndexManager).thenReturn(indexManager);

      final Map map = new Map();
      map.setConfigureName("myMap");
      final List<Map> mapList = List.of(map);
      when(gm.getComponentsOf(Map.class)).thenReturn(mapList);

      final Board b = new Board();
      b.setConfigureName("board");
      SquareGrid grid = new SquareGrid();
      SquareGridNumbering numbering = new SquareGridNumbering();
      numbering.addTo(grid);
      b.setGrid(grid);

      b.setMap(map);
      map.setBoards(new ArrayList<>(List.of(b)));
      // Approximate center of top-left square of the grid.
      final int x = (int) grid.getDx() / 2;
      final int y = (int) grid.getDy() / 2;

      GamePiece piece1 = new BasicPiece();
      GamePiece piece2 = new BasicPiece();
      GamePiece piece3 = new BasicPiece();
      GamePiece piece4 = new BasicPiece();

      piece1.setId("1");
      piece1.setProperty(Properties.PIECE_ID, "1"); //NON-NLS
      piece1.setProperty("Strength", "10"); //NON-NLS

      final List<ReportState> reports = new ArrayList<>();
      reports.add( buildReportState("CountMap", "{CountMap()}", piece1) );
      reports.add( buildReportState("SumMap", "{SumMap(\""+Properties.PIECE_ID+"\")};", piece1) );
      reports.add( buildReportState("CountStack","{CountStack()};", piece1) );
      reports.add( buildReportState("SumStack", "{SumStack(\"Strength\")};", piece1) );
      reports.add( buildReportState("CountLocation", "{CountLocation()};", piece1) );
      reports.add( buildReportState("SumLocation", "{SumLocation(\""+Properties.PIECE_ID+"\")};", piece1) );

      // Make sure this piece refers to the outermost trait.
      piece1 = Decorator.getOutermost(piece1);
      map.placeAt(piece1, new Point(x, y));

      // Piece 1 and 2 are in a stack
      piece2.setId("2");
      piece2.setProperty(Properties.PIECE_ID, "2"); //NON-NLS
      piece2.setProperty("Strength", "200"); //NON-NLS
      reports.add( buildReportState("CountRange", "{CountRange(0,2)}", piece2) );
      piece2 = Decorator.getOutermost(piece2);
      map.placeAt(piece2, new Point(x, y));

      Stack s = new Stack(piece1);
      s.add(piece2);
      map.addPiece(s);

      // Piece 3 is two squares right
      piece3.setId("3");
      piece3.setProperty(Properties.PIECE_ID, "3"); //NON-NLS
      piece3.setProperty("Strength", "3000"); //NON-NLS
      java.util.Map<String, Object> m = ((PropertyExporter) piece3).getProperties();
      // Create an "old" property
      m.put("Strength", "3001");
      piece3.setProperty(Properties.SNAPSHOT, m);
      reports.add( buildReportState("SumRange", "{SumRange(0,1,\"Strength\",\"{Strength>$oldStrength$}\")}", piece3) );
      piece3 = Decorator.getOutermost(piece3);
      map.placeAt(piece3, new Point((int) (x + grid.getDx()*2), y));

      // Piece 4 is one square right of Piece 3
      piece4.setId("4");
      piece4.setProperty(Properties.PIECE_ID, "4"); //NON-NLS
      piece4.setProperty("Strength", "4000"); //NON-NLS
      map.placeAt(piece4, new Point((int) (x + grid.getDx()*3), y));

      // Run the reports
      for (ReportState report : reports) {
        KeyStroke stroke = report.getNamedKeyStrokeList().get(0).getStroke();
        report.myKeyEvent(stroke);
      }

      // Capture the chatter output. The "* " precedes all output.
      ArgumentCaptor<String> chatterCapture = ArgumentCaptor.forClass(String.class);
      verify(chatter, times(reports.size())).show(chatterCapture.capture());
      List<String> messages = chatterCapture.getAllValues();

      assertEquals("* 4", messages.get(0), reports.get(0).toString());
      assertEquals("* 10", messages.get(1), reports.get(1).toString());
      assertEquals("* 2", messages.get(2), reports.get(2).toString());
      assertEquals("* 210", messages.get(3), reports.get(3).toString());
      assertEquals("* 2", messages.get(4), reports.get(4).toString());
      assertEquals("* 3", messages.get(5), reports.get(5).toString());
      assertEquals("* 3", messages.get(6), reports.get(6).toString());
      assertEquals("* 4000", messages.get(7), reports.get(7).toString());
    }
  }

  @Test
  public void testGetZonePropertyFromReportTrait() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final Chatter chatter = mock(Chatter.class);
      staticGm.when(gm::getChatter).thenReturn(chatter);
      // Map.setUpView requires Prefs
      final Prefs prefs = new Prefs(new PrefsEditor(), "");
      staticGm.when(gm::getPrefs).thenReturn(prefs);
      // Required by Map.addPiece
      final IndexManager indexManager = new IndexManager();
      staticGm.when(gm::getIndexManager).thenReturn(indexManager);

      final Map map = new Map();
      map.setConfigureName("myMap");
      final List<Map> mapList = List.of(map);
      when(gm.getComponentsOf(Map.class)).thenReturn(mapList);

      final Board b = new Board();
      b.setConfigureName("board");

      ZonedGrid zoneGrid = new ZonedGrid();
      Zone zone = new Zone();
      zone.setConfigureName("zone1");
      zone.addMutableProperty("prop", new MutableProperty() {
        @Override
        public String getPropertyValue() {
          return "mutablePropertyValue";
        }

        @Override
        public Command setPropertyValue(String newValue) {
          return null;
        }

        @Override
        public void addMutablePropertyChangeListener(PropertyChangeListener l) {

        }

        @Override
        public void removeMutablePropertyChangeListener(PropertyChangeListener l) {

        }

        @Override
        public MutablePropertiesContainer getParent() {
          return null;
        }
      });

      zoneGrid.addZone(zone);
      b.add(zoneGrid);

      b.setMap(map);
      map.setBoards(new ArrayList<>(List.of(b)));

      BasicPiece piece = new BasicPiece();
      ReportState trait = buildReportState("GetZoneProperty", "{GetZoneProperty(\"prop\",\"zone1\")}", piece);
      piece.setMap(map);

      KeyStroke stroke = trait.getNamedKeyStrokeList().get(0).getStroke();
      Command c = trait.myKeyEvent(stroke);
      assertEquals("DisplayText[* mutablePropertyValue]", c.toString());
    }
  }
}
