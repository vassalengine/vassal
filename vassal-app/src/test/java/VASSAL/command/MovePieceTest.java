package VASSAL.command;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.Point;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class MovePieceTest {

  // Select a piece, replay a move of it from oldMapId to newMapId,
  // and report whether the piece is still selected afterwards
  private boolean selectedAfterMove(String oldMapId, String newMapId) {
    final GamePiece piece = mock(GamePiece.class);
    final GameState gameState = mock(GameState.class);
    when(gameState.getPieceForId("p")).thenReturn(piece);
    final GameModule gm = mock(GameModule.class);
    when(gm.getGameState()).thenReturn(gameState);

    final KeyBuffer kb = KeyBuffer.getBuffer();
    kb.add(piece);
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      new MovePiece("p", newMapId, new Point(0, 0), null,
                    oldMapId, new Point(0, 0), null, "player").execute();
      return kb.contains(piece);
    }
    finally {
      kb.clear();
    }
  }

  @Test
  public void pieceMovedToAnotherMapShouldBeDeselected() {
    assertFalse(selectedAfterMove("Main Map", "Other Map"));
  }

  @Test
  public void pieceMovedWithinSameMapShouldStaySelected() {
    assertTrue(selectedAfterMove("Main Map", "Main Map"));
  }
}
