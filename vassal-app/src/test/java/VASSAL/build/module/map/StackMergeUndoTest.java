package VASSAL.build.module.map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MovePiece;
import VASSAL.counters.BasicPiece;
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

  @Test
  public void undoMoveOntoStackReturnsPieceToItsEmptyStack() {
    final Point originPos = new Point(100, 200);
    final BasicPiece moving = newPiece("moving", originPos); // NON-NLS
    final Stack origin = newStack("origin", originPos, moving); // NON-NLS

    final Point destPos = new Point(200, 200);
    final BasicPiece other = newPiece("other", destPos); // NON-NLS
    final Stack dest = newStack("dest", destPos, other); // NON-NLS

    // Move the only piece out of its hex onto another Stack, then undo
    final Command move = map.getStackMetrics().merge(dest, moving);
    assertSame(dest, moving.getParent());
    assertTrue(origin.isEmpty());

    move.getUndoCommand().execute();

    assertSame(origin, moving.getParent(), "piece was not returned to its Stack");
    assertEquals(Arrays.asList(moving), origin.asList());
    assertEquals(originPos, moving.getPosition());
    assertEquals(Arrays.asList(other), dest.asList());
  }

  @Test
  public void undoMoveToEmptyHexLeavesPieceInAStack() {
    final Point originPos = new Point(100, 200);
    final BasicPiece moving = newPiece("moving", originPos); // NON-NLS
    newStack("origin", originPos, moving); // NON-NLS

    final Command move = map.placeOrMerge(moving, new Point(200, 200));
    move.getUndoCommand().execute();

    assertNotNull(moving.getParent(), "piece was left unstacked");
    assertEquals(originPos, moving.getPosition());
    assertTrue(Arrays.asList(map.getPieces()).contains(moving.getParent()));
  }

  @Test
  public void moveIntoEmptyHexStillEndsInItsNewStack() {
    // As executed on another client: MovePiece, then AddPiece of the Stack created by the mover
    final Point originPos = new Point(100, 200);
    final BasicPiece moving = newPiece("moving", originPos); // NON-NLS
    newStack("origin", originPos, moving); // NON-NLS

    // An empty Stack left behind at the destination by an earlier move
    final Point destPos = new Point(200, 200);
    final Stack leftover = newStack("leftover", destPos); // NON-NLS

    final Stack created = new Stack();
    created.setId("created"); // NON-NLS
    final String createdState = new SequenceEncoder(';')
      .append(map.getIdentifier()).append(destPos.x).append(destPos.y)
      .append(moving.getId()).append(Stack.HAS_LAYER_MARKER + 1).getValue();

    new MovePiece(moving.getId(), map.getIdentifier(), destPos, null,
      map.getIdentifier(), originPos, null, null)
      .append(new AddPiece(created, createdState))
      .execute();

    assertSame(created, moving.getParent());
    assertEquals(destPos, moving.getPosition());
    assertTrue(leftover.isEmpty());
  }

  @Test
  public void undoMoveDoesNotReturnPieceToEmptyStackOnAnotherLayer() {
    final Point originPos = new Point(100, 200);

    // An empty Stack bound to a different layer, with an id that would be preferred
    final Stack otherLayer = new Stack();
    otherLayer.setId("0"); // NON-NLS
    pieces.put(otherLayer.getId(), otherLayer);
    otherLayer.setState(new SequenceEncoder(';')
      .append(map.getIdentifier()).append(originPos.x).append(originPos.y)
      .append(Stack.HAS_LAYER_MARKER + 0).getValue());

    final BasicPiece moving = newPiece("moving", originPos); // NON-NLS
    final Stack origin = newStack("origin", originPos, moving); // NON-NLS
    final Stack dest = newStack("dest", new Point(200, 200), newPiece("other", new Point(200, 200))); // NON-NLS

    map.getStackMetrics().merge(dest, moving).getUndoCommand().execute();

    assertSame(origin, moving.getParent());
    assertTrue(otherLayer.isEmpty());
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
