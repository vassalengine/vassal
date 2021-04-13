package VASSAL.counters;

import java.awt.Point;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class DeckTest {

  @Test
  public void defaultConstructorShouldCreateEmptyDeck() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);

    // run
    Deck d = new Deck(gameModule);

    // assert
    assertEquals(0, d.pieceCount);
    assertNull(d.getMap());
    assertEquals(new Point(0, 0), d.getPosition());
  }

  @Test
  public void getPieceCountShouldWork() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);

    // assert
    assertEquals(2, d.getPieceCount());
  }

  @Test
  public void addNullPieceShouldNotAdd() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(null);

    // assert
    assertEquals(0, d.getPieceCount());
  }

  @Test
  public void addNewPieceShouldInsertAtLastPosition() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);

    // assert
    assertEquals(gamePiece2, d.getPieceAt(1));
  }

  @Test
  public void getPieceShouldGetProperPiece() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);

    // assert
    assertEquals(gamePiece1, d.getPieceAt(0));
    assertEquals(gamePiece2, d.getPieceAt(1));
  }

  @Test
  public void addPieceToEmptyDeckShouldNotSetMapAndPosition() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);

    // assert
    assertEquals(2, d.getPieceCount());
    assertNull(d.getMap());
    assertEquals(new Point(0, 0), d.getPosition());
  }

  @Test
  public void insertShouldInsertAtCorrectPosition() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.insert(gamePiece1, 0);
    d.insert(gamePiece2, 1);
    d.insert(gamePiece3, 0);

    // assert
    assertEquals(3, d.getPieceCount());
    assertEquals(gamePiece3, d.getPieceAt(0));
    assertEquals(gamePiece1, d.getPieceAt(1));
    assertEquals(gamePiece2, d.getPieceAt(2));
  }

  @Test
  public void insertShouldRepositionWhenPieceInDeck() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.insert(gamePiece1, 1);

    // assert
    assertEquals(2, d.getPieceCount());
    assertEquals(gamePiece2, d.getPieceAt(0));
    assertEquals(gamePiece1, d.getPieceAt(1));
  }

  @Test
  public void insertShouldCorrectWrongIndex() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.insert(gamePiece2, -1);
    d.insert(gamePiece3, 5);

    // assert
    assertEquals(3, d.getPieceCount());
    assertEquals(gamePiece2, d.getPieceAt(0));
    assertEquals(gamePiece1, d.getPieceAt(1));
    assertEquals(gamePiece3, d.getPieceAt(2));
  }

  @Test
  public void indexOfShouldWork() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);

    // assert
    assertEquals(2, d.getPieceCount());
    assertEquals(0, d.indexOf(gamePiece1));
    assertEquals(1, d.indexOf(gamePiece2));
  }

  @Test
  public void removeAllShouldRemoveAll() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.removeAll();

    // assert
    assertEquals(0, d.getPieceCount());
  }

  @Test
  public void isExpandedShouldAlwaysReturnFalse() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.setExpanded(true);

    // assert
    assertFalse(d.isExpanded());
  }

  @Test
  public void removeAllShouldResetExpanded() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.setExpanded(true);
    d.removeAll();

    // assert
    assertFalse(d.isExpanded());
  }

  @Test
  public void removeShouldRemove() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.remove(gamePiece1);

    // assert
    assertEquals(1, d.getPieceCount());
    assertEquals(gamePiece2, d.getPieceAt(0));
  }

  @Test
  public void removePieceAtShouldWork() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.removePieceAt(0);

    // assert
    assertEquals(1, d.getPieceCount());
    assertEquals(gamePiece2, d.getPieceAt(0));
  }

  @Test
  public void removeShouldResetPiecesParent() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);

    // run
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.remove(gamePiece1);

    // assert
    assertEquals(1, d.getPieceCount());
    verify(gamePiece1).setParent(null);
  }

  @Test
  public void removeShouldRepaintMap() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece = mock(GamePiece.class);
    final Map map = mock(Map.class);

    Deck d = new Deck(gameModule);
    d.add(gamePiece);
    d.setMap(map);

    // run
    d.remove(gamePiece);

    // assert
    verify(map).repaint();
  }

  @Test
  public void setMapGetMapShouldWork() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final Map map = mock(Map.class);

    // run
    Deck d = new Deck(gameModule);
    d.setMap(map);

    // assert
    assertEquals(map, d.getMap());
  }

  @Test
  public void setPosGetPosShouldWork() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final Point point = new Point(10, 100);
    final Deck d = new Deck(gameModule);

    // run
    d.setPosition(point);

    // assert
    assertEquals(point, d.getPosition());
  }

  @Test
  public void setIdGetIdShouldWork() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final String id = "someId";
    final Deck d = new Deck(gameModule);

    // run
    d.setId(id);

    // assert
    assertEquals(id, d.getId());
  }

  @Test
  public void setPropertyShouldDoNothingGetPropertyShouldReturnNull() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final Object key = new Object();
    final Object value = new Object();

    // run
    Deck d = new Deck(gameModule);
    d.setProperty(key, value);

    // assert
    assertNull(d.getProperty(key));
  }

  @Test
  public void getPiecesIteratorShouldIterateInOrder() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);
    List<GamePiece> gamePieces = Arrays.asList(gamePiece1, gamePiece2, gamePiece3);
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.add(gamePiece3);

    // run
    final Iterator<GamePiece> iterator = d.getPiecesIterator();

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
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);
    List<GamePiece> gamePieces = Arrays.asList(gamePiece3, gamePiece2, gamePiece1);
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.add(gamePiece3);

    // run
    final Iterator<GamePiece> iterator = d.getPiecesReverseIterator();

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
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    final GamePiece gamePiece3 = mock(GamePiece.class);
    Deck d = new Deck(gameModule);
    d.add(gamePiece1);
    d.add(gamePiece2);
    d.add(gamePiece3);

    // run
    final List<GamePiece> pieces = d.asList();

    // assert
    assertEquals(3, pieces.size());
  }

  @Test
  public void asListShouldGetAllPiecesInOrder() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece0 = mock(GamePiece.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    final GamePiece gamePiece2 = mock(GamePiece.class);
    Deck d = new Deck(gameModule);
    d.add(gamePiece0);
    d.add(gamePiece1);
    d.add(gamePiece2);

    // run
    final List<GamePiece> pieces = d.asList();

    // assert
    assertEquals(gamePiece0, pieces.get(0));
    assertEquals(gamePiece1, pieces.get(1));
    assertEquals(gamePiece2, pieces.get(2));
  }

  @Test
  public void asListShouldReturnDefensiveList() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);
    final GamePiece gamePiece0 = mock(GamePiece.class);
    final GamePiece gamePiece1 = mock(GamePiece.class);
    Deck d = new Deck(gameModule);
    d.add(gamePiece0);
    d.add(gamePiece1);

    // run
    final List<GamePiece> pieces = d.asList();
    d.removePieceAt(0);

    // assert
    assertEquals(2, pieces.size());
    assertEquals(gamePiece0, pieces.get(0));
    assertEquals(gamePiece1, pieces.get(1));
  }
}
