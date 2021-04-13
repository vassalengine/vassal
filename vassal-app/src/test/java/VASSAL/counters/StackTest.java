package VASSAL.counters;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

import VASSAL.build.module.Map;
import java.awt.Point;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.Test;

public class StackTest {

  @Test
  public void noArgConstructorShouldCreateEmptyStack() {
    // run
    Stack s = new Stack();

    // assert
    assertEquals(0, s.pieceCount);
    assertNull(s.getMap());
    assertEquals(new Point(0, 0), s.getPosition());
  }

  @Test
  public void constructorShouldCreateEmptyStackWhenPassedNull() {
    // run
    Stack s = new Stack(null);

    // assert
    assertEquals(0, s.pieceCount);
    assertNull(s.getMap());
    assertEquals(new Point(0, 0), s.getPosition());
  }

  @Test
  public void constructorShouldSetMapAndPosition() {
    // prepare
    final GamePiece gamePiece = mock(GamePiece.class);
    final Map map = mock(Map.class);
    final Point point = new Point(10, 100);
    when(gamePiece.getMap()).thenReturn(map);
    when(gamePiece.getPosition()).thenReturn(point);

    // run
    Stack s = new Stack(gamePiece);

    // assert
    assertEquals(1, s.getPieceCount());
    assertEquals(map, s.getMap());
    assertEquals(point, s.getPosition());
    assertEquals(gamePiece, s.getPieceAt(0));
  }

  @Test
  public void getPieceCountShouldWork() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final Map map = mock(Map.class);
    final Point point = new Point(10, 100);
    when(gamePiece1.getMap()).thenReturn(map);
    when(gamePiece1.getPosition()).thenReturn(point);

    // run
    Stack s = new Stack(gamePiece1);
    s.add(gamePiece2);

    // assert
    assertEquals(2, s.getPieceCount());
  }

  @Test
  public void addNullPieceShouldNotAdd() {
    // prepare
    final GamePiece gamePiece = mock(GamePiece.class);
    final Map map = mock(Map.class);
    final Point point = new Point(10, 100);
    when(gamePiece.getMap()).thenReturn(map);
    when(gamePiece.getPosition()).thenReturn(point);

    // run
    Stack s = new Stack(gamePiece);
    s.add(null);

    // assert
    assertEquals(1, s.getPieceCount());
  }

  @Test
  public void addNewPieceShouldInsertAtLastPosition() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final Map map = mock(Map.class);
    final Point point = new Point(10, 100);
    when(gamePiece1.getMap()).thenReturn(map);
    when(gamePiece1.getPosition()).thenReturn(point);

    // run
    Stack s = new Stack(gamePiece1);
    s.add(gamePiece2);

    // assert
    assertEquals(gamePiece2, s.getPieceAt(1));
  }

  @Test
  public void getPieceShouldGetProperPiece() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final Map map = mock(Map.class);
    final Point point = new Point(10, 100);
    when(gamePiece1.getMap()).thenReturn(map);
    when(gamePiece1.getPosition()).thenReturn(point);

    // run
    Stack s = new Stack(gamePiece1);
    s.add(gamePiece2);

    // assert
    assertEquals(gamePiece1, s.getPieceAt(0));
    assertEquals(gamePiece2, s.getPieceAt(1));
  }

  @Test
  public void addPieceToEmptyStackShouldNotSetMapAndPosition() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);

    // assert
    assertEquals(2, s.getPieceCount());
    assertNull(s.getMap());
    assertEquals(new Point(0, 0), s.getPosition());
  }

  @Test
  public void insertShouldInsertAtCorrectPosition() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.insert(gamePiece1, 0);
    s.insert(gamePiece2, 1);
    s.insert(gamePiece3, 0);

    // assert
    assertEquals(3, s.getPieceCount());
    assertEquals(gamePiece3, s.getPieceAt(0));
    assertEquals(gamePiece1, s.getPieceAt(1));
    assertEquals(gamePiece2, s.getPieceAt(2));
  }

  @Test
  public void insertShouldRepositionWhenPieceInStack() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.insert(gamePiece1, 1);

    // assert
    assertEquals(2, s.getPieceCount());
    assertEquals(gamePiece2, s.getPieceAt(0));
    assertEquals(gamePiece1, s.getPieceAt(1));
  }

  @Test
  public void insertShouldCorrectWrongIndex() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.insert(gamePiece2, -1);
    s.insert(gamePiece3, 5);

    // assert
    assertEquals(3, s.getPieceCount());
    assertEquals(gamePiece2, s.getPieceAt(0));
    assertEquals(gamePiece1, s.getPieceAt(1));
    assertEquals(gamePiece3, s.getPieceAt(2));
  }

  @Test
  public void indexOfShouldWork() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);

    // assert
    assertEquals(2, s.getPieceCount());
    assertEquals(0, s.indexOf(gamePiece1));
    assertEquals(1, s.indexOf(gamePiece2));
  }

  @Test
  public void removeAllShouldRemoveAll() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.removeAll();

    // assert
    assertEquals(0, s.getPieceCount());
  }

  @Test
  public void setExpandedAndIsExpandedShouldWork() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.setExpanded(true);

    // assert
    assertTrue(s.isExpanded());
  }

  @Test
  public void removeAllShouldResetExpanded() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.setExpanded(true);
    s.removeAll();

    // assert
    assertFalse(s.isExpanded());
  }

  @Test
  public void removeShouldRemove() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.remove(gamePiece1);

    // assert
    assertEquals(1, s.getPieceCount());
    assertEquals(gamePiece2, s.getPieceAt(0));
  }

  @Test
  public void removePieceAtShouldWork() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.removePieceAt(0);

    // assert
    assertEquals(1, s.getPieceCount());
    assertEquals(gamePiece2, s.getPieceAt(0));
  }

  @Test
  public void removeShouldResetPiecesParent() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.remove(gamePiece1);

    // assert
    assertEquals(1, s.getPieceCount());
    verify(gamePiece1).setParent(null);
  }

  @Test
  public void removeShouldRepaintMap() {
    // prepare
    final GamePiece gamePiece = mock(GamePiece.class);
    final Map map = mock(Map.class);
    final Point point = new Point(10, 100);
    when(gamePiece.getMap()).thenReturn(map);
    when(gamePiece.getPosition()).thenReturn(point);

    // run
    Stack s = new Stack(gamePiece);
    s.remove(gamePiece);

    // assert
    verify(map).repaint();
  }

  @Test
  public void publicStaticTypeShouldBeCorrect() {
    assertEquals("stack", Stack.TYPE);
  }

  @Test
  public void getTypeShouldReturnProperType() {
    // run
    Stack s = new Stack();

    // assert
    assertEquals("stack", s.getType());
  }

  @Test
  public void setMapGetMapShouldWork() {
    // prepare
    final GamePiece gamePiece = mock(GamePiece.class);
    final Map map1 = mock(Map.class);
    final Map map2 = mock(Map.class);
    final Point point = new Point(10, 100);
    when(gamePiece.getMap()).thenReturn(map1);
    when(gamePiece.getPosition()).thenReturn(point);

    // run
    Stack s = new Stack(gamePiece);
    s.setMap(map2);

    // assert
    assertEquals(map2, s.getMap());
  }

  @Test
  public void setPosGetPosShouldWork() {
    // prepare
    final GamePiece gamePiece = mock(GamePiece.class);
    final Map map = mock(Map.class);
    final Point point1 = new Point(10, 100);
    final Point point2 = new Point(11, 101);
    when(gamePiece.getMap()).thenReturn(map);
    when(gamePiece.getPosition()).thenReturn(point1);

    // run
    Stack s = new Stack(gamePiece);
    s.setPosition(point2);

    // assert
    assertEquals(point2, s.getPosition());
  }

  @Test
  public void setIdGetIdShouldWork() {
    // prepare
    final String id = "someId";

    // run
    Stack s = new Stack();
    s.setId(id);

    // assert
    assertEquals(id, s.getId());
  }

  @Test
  public void setPropertyShouldDoNothingGetPropertyShouldReturnNull() {
    // prepare
    final Object key = new Object();
    final Object value = new Object();

    // run
    Stack s = new Stack();
    s.setProperty(key, value);

    // assert
    assertNull(s.getProperty(key));

  }

  @Test
  public void getPiecesIteratorShouldIterateInOrder() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);
    List<GamePiece> gamePieces = Arrays.asList(gamePiece1, gamePiece2, gamePiece3);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.add(gamePiece3);
    final Iterator<GamePiece> iterator = s.getPiecesIterator();

    // assert
    int i = 0;
    while (iterator.hasNext()) {
      final GamePiece gamePiece = iterator.next();
      assertEquals(gamePieces.get(i++), gamePiece);
    }
  }

  @Test
  public void getPiecesReverseIteratorShouldIterateInReverseOrder() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);
    List<GamePiece> gamePieces = Arrays.asList(gamePiece3, gamePiece2, gamePiece1);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.add(gamePiece3);
    final Iterator<GamePiece> iterator = s.getPiecesReverseIterator();

    // assert
    int i = 0;
    while (iterator.hasNext()) {
      final GamePiece gamePiece = iterator.next();
      assertEquals(gamePieces.get(i++), gamePiece);
    }

  }

  @Test
  public void asListShouldReturnWithCorrectSize() {
    // prepare
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece1);
    s.add(gamePiece2);
    s.add(gamePiece3);
    final List<GamePiece> pieces = s.asList();

    // assert
    assertEquals(3, pieces.size());
  }

  @Test
  public void asListShouldGetAllPiecesInOrder() {
    // prepare
    final GamePiece gamePiece0 = mock(GamePiece.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Stack s = new Stack();
    s.add(gamePiece0);
    s.add(gamePiece1);
    s.add(gamePiece2);
    final List<GamePiece> pieces = s.asList();

    // assert
    assertEquals(gamePiece0, pieces.get(0));
    assertEquals(gamePiece1, pieces.get(1));
    assertEquals(gamePiece2, pieces.get(2));
  }

  @Test
  public void asListShouldReturnDefensiveList() {
    // prepare
    final GamePiece gamePiece0 = mock(GamePiece.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    Stack s = new Stack();
    s.add(gamePiece0);
    s.add(gamePiece1);

    // run
    final List<GamePiece> pieces = s.asList();
    s.removePieceAt(0);

    // assert
    assertEquals(2, pieces.size());
    assertEquals(gamePiece0, pieces.get(0));
    assertEquals(gamePiece1, pieces.get(1));
  }
}
