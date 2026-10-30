package VASSAL.build.module.map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
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
import VASSAL.command.Command;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.preferences.Prefs;

import java.awt.Point;
import java.util.Arrays;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

/**
 * Undoing a move that merged a piece onto a piece that was not in a Stack
 * must leave the fixed piece on the map.
 */
public class StackMergeUndoTest {

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
  private TestMap map;
  private final java.util.Map<String, GamePiece> pieces = new HashMap<>();
  private int nextId = 0;

  @BeforeEach
  public void setUp() {
    staticGo = Mockito.mockStatic(GlobalOptions.class);
    staticGo.when(GlobalOptions::getInstance).thenReturn(mock(GlobalOptions.class));

    final GameState gs = mock(GameState.class);
    when(gs.getPieceForId(any())).thenAnswer(i -> i.getArgument(0) == null ? null : pieces.get((String) i.getArgument(0)));
    doAnswer(i -> {
      final GamePiece p = i.getArgument(0);
      if (p.getId() == null) {
        p.setId("new" + nextId++); // NON-NLS
      }
      pieces.put(p.getId(), p);
      return null;
    }).when(gs).addPiece(any(GamePiece.class));
    doAnswer(i -> {
      pieces.remove(((GamePiece) i.getArgument(0)).getId());
      return null;
    }).when(gs).removePiece(any(GamePiece.class));
    doAnswer(i -> {
      pieces.remove((String) i.getArgument(0));
      return null;
    }).when(gs).removePiece(anyString());

    final GameModule gm = mock(GameModule.class);
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

  @Test
  public void undoMergeOntoUnstackedPieceKeepsFixedPieceOnMap() {
    // A piece sitting on the map without a Stack (e.g. after a load or an earlier undo)
    final Point fixedPos = new Point(200, 200);
    final BasicPiece fixed = newPiece("fixed", fixedPos); // NON-NLS
    map.addPiece(fixed);

    // A piece in its own Stack in a neighbouring hex
    final Point movingPos = new Point(100, 200);
    final BasicPiece moving = newPiece("moving", movingPos); // NON-NLS
    final Stack origin = new Stack();
    origin.setId("origin"); // NON-NLS
    pieces.put(origin.getId(), origin);
    origin.setPosition(movingPos);
    map.addPiece(origin);
    origin.add(moving);

    // Drop the moving piece onto the fixed piece
    final Command move = map.getStackMetrics().merge(fixed, moving);
    final Stack merged = fixed.getParent();
    assertSame(merged, moving.getParent());
    assertEquals(Arrays.asList(fixed, moving), merged.asList());

    // Undo the move
    move.getUndoCommand().execute();

    // The moving piece goes back where it came from
    assertSame(map, moving.getMap());
    assertEquals(movingPos, moving.getPosition());

    // The fixed piece must still be on the map and drawn by it
    assertSame(map, fixed.getMap(), "fixed piece lost its map after undo");
    final GamePiece drawn = fixed.getParent() == null ? fixed : fixed.getParent();
    assertTrue(Arrays.asList(map.getPieces()).contains(drawn), "fixed piece is no longer drawn on the map");
    assertEquals(fixedPos, fixed.getPosition());
    assertTrue(fixed.getParent() == null || fixed.getParent().indexOf(fixed) >= 0,
      "fixed piece's parent Stack does not contain it");
  }
}
