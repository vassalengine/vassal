package VASSAL.command;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.Delete;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class SetPersistentPropertyCommandTest {

  static final String id = "1234";
  static final String id2 = "12345";
  static final String key = "key";
  static final String oldValue = "123";
  static final String newValue = "456";

  @Test
  public void constructorTest() {
    final SetPersistentPropertyCommand p = new SetPersistentPropertyCommand(id, key, oldValue, newValue);
    assertThat(p.getId(), is(equalTo(id)));
    assertThat(p.getKey(), is(equalTo(key)));
    assertThat(p.getOldValue(), is(equalTo(oldValue)));
    assertThat(p.getNewValue(), is(equalTo(newValue)));
  }

  /*
   * Test generation and execution of the command
   * Bug 13454 - Counters consisting of a BasicPiece only would generate ClassCastExceptions when
   * the target of a SetPersistentProperty Command.
   */
  @Test
  public void execution_test () {

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Set up Dummy Pieces
      final BasicPiece bp = new BasicPiece();
      bp.setId(id);

      final BasicPiece bp2 = new BasicPiece();
      bp2.setId(id2);
      Decorator d = new Delete();
      d.setInner(bp2);

      // Set up Mocks to return our dummy pieces
      GameModule gm = mock(GameModule.class);
      GameState gs = mock(GameState.class);
      when(gs.getPieceForId(id)).thenReturn(bp);
      when(gs.getPieceForId(id2)).thenReturn(d);
      when(gm.getGameState()).thenReturn(gs);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      // TEST 1 - BasicPiece

      // Set the property to a known value and validate it
      bp.setPersistentProperty(key, oldValue);
      assertThat(bp.getPersistentProperty(key), is(equalTo(oldValue)));

      // Set the property to a new value and save the command it generates
      final SetPersistentPropertyCommand command = (SetPersistentPropertyCommand) bp.setPersistentProperty(key, newValue);
      assertThat(bp.getPersistentProperty(key), is(equalTo(newValue)));
      assertThat(command.getId(), is(equalTo(id)));
      assertThat(command.getKey(), is(equalTo(key)));
      assertThat(command.getOldValue(), is(equalTo(oldValue)));
      assertThat(command.getNewValue(), is(equalTo(newValue)));

      // Set the property back to the old value
      bp.setPersistentProperty(key, oldValue);
      assertThat(bp.getPersistentProperty(key), is(equalTo(oldValue)));

      // Execute the command and check it changes the property
      command.execute();
      assertThat(bp.getPersistentProperty(key), is(equalTo(newValue)));

      // TEST 2 - Decorator

      // Set the property to a known value and validate it
      bp2.setPersistentProperty(key, oldValue);
      assertThat(bp2.getPersistentProperty(key), is(equalTo(oldValue)));

      // Set the property to a new value and save the command it generates
      final SetPersistentPropertyCommand command2 = (SetPersistentPropertyCommand) bp2.setPersistentProperty(key, newValue);
      assertThat(bp2.getPersistentProperty(key), is(equalTo(newValue)));
      assertThat(command2.getId(), is(equalTo(id2)));
      assertThat(command2.getKey(), is(equalTo(key)));
      assertThat(command2.getOldValue(), is(equalTo(oldValue)));
      assertThat(command2.getNewValue(), is(equalTo(newValue)));

      // Set the property back to the old value
      bp2.setPersistentProperty(key, oldValue);
      assertThat(bp2.getPersistentProperty(key), is(equalTo(oldValue)));

      // Execute the command and check it changes the property
      command2.execute();
      assertThat(bp2.getPersistentProperty(key), is(equalTo(newValue)));

    }
  }
}