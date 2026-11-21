package VASSAL.build.module;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.preferences.Prefs;
import VASSAL.tools.filechooser.FileChooser;
import java.io.File;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class BasicLoggerTest {

  @Test
  public void undoAfterBeginningNewLogfile(@TempDir File dir) {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final FileChooser fc = mock(FileChooser.class);
      when(fc.showSaveDialog()).thenReturn(FileChooser.APPROVE_OPTION);
      when(fc.getSelectedFile()).thenReturn(new File(dir, "test.vlog"));
      when(gm.getFileChooser()).thenReturn(fc);

      final GameState gs = mock(GameState.class);
      when(gs.getRestoreCommand()).thenReturn(new NullCommand());
      when(gm.getGameState()).thenReturn(gs);

      final Prefs prefs = mock(Prefs.class);
      when(prefs.getValue(SaveMetaData.PROMPT_LOG_COMMENT)).thenReturn(false);
      when(gm.getPrefs()).thenReturn(prefs);

      final BasicLogger logger = new BasicLogger();

      // Undoable commands from stepping through a replayed log
      logger.log(new ChangePiece("a", "1", "2"));
      logger.log(new ChangePiece("b", "1", "2"));

      logger.beginOutput();

      // Logged after the new log begins, with nothing to undo
      logger.log(new ChangePiece("c", null, "1"));

      assertDoesNotThrow(logger::undo);
    }
  }

  /**
   * Every change to a game -- made locally, received from another player, or
   * replayed from a logfile -- is logged here, which is how GameState knows the
   * game has been modified without serialising the whole of it to compare.
   */
  @Test
  public void loggingACommandMarksTheGameModified() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final GameState gs = mock(GameState.class);
      when(gm.getGameState()).thenReturn(gs);

      final BasicLogger logger = new BasicLogger();
      logger.log(new ChangePiece("a", "1", "2"));
      verify(gs).setModified(true);
    }
  }

  /** A command that does nothing is not logged, and must not dirty the game. */
  @Test
  public void loggingANullCommandLeavesTheGameUnmodified() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final GameState gs = mock(GameState.class);
      when(gm.getGameState()).thenReturn(gs);

      final BasicLogger logger = new BasicLogger();
      logger.log(new NullCommand());
      logger.log(null);
      verify(gs, never()).setModified(true);
    }
  }

  /**
   * An undo is a change to the game like any other, but undo() adds it to the
   * log directly rather than through log(). Undoing a move after a save used to
   * leave the game looking clean, so closing it threw the undo away unasked.
   */
  @Test
  public void undoingAMoveMarksTheGameModified() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final GameState gs = mock(GameState.class);
      when(gm.getGameState()).thenReturn(gs);
      when(gm.getServer()).thenReturn(mock(ServerConnection.class));

      final BasicLogger logger = new BasicLogger();
      when(gm.getBasicLogger()).thenReturn(logger);

      logger.log(new ChangePiece("a", "1", "2"));

      // The game is saved here: everything logged so far is on disk, and the
      // flag goes back to clean.
      Mockito.clearInvocations(gs);

      logger.undo();
      verify(gs).setModified(true);
    }
  }

  /**
   * A restore command is the game, not a change to it: it is how a joining
   * client is sent the host's game. Its own trailing SetupCommand(true) has just
   * run setup(), which decides the flag, so logging it must not overrule that
   * and ask a player who has touched nothing whether to save.
   */
  @Test
  public void loggingARestoreCommandLeavesTheGameUnmodified() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final GameState gs = mock(GameState.class);
      when(gm.getGameState()).thenReturn(gs);

      // Shaped as GameState.getRestoreCommand() builds it.
      final Command restore = new GameState.SetupCommand(false);
      restore.append(new ChangePiece("a", "1", "2"));
      restore.append(new GameState.SetupCommand(true));

      final BasicLogger logger = new BasicLogger();
      logger.log(restore);
      verify(gs, never()).setModified(true);
    }
  }

  /**
   * Chat is in the logfile but not in getRestoreCommand(), so it changes nothing
   * a save would record. Startup Global Key Commands which report to chat run
   * after setup() has cleared the flag, so without this a freshly loaded game in
   * such a module would offer to save itself.
   */
  @Test
  public void loggingChatAloneLeavesTheGameUnmodified() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final GameState gs = mock(GameState.class);
      when(gm.getGameState()).thenReturn(gs);

      final BasicLogger logger = new BasicLogger();
      logger.log(new Chatter.DisplayText(null, "a startup message"));
      verify(gs, never()).setModified(true);
    }
  }

  /** ... but chat alongside a real change must still mark the game modified. */
  @Test
  public void loggingChatWithAChangeMarksTheGameModified() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final GameState gs = mock(GameState.class);
      when(gm.getGameState()).thenReturn(gs);

      final Command c = new Chatter.DisplayText(null, "moving");
      c.append(new ChangePiece("a", "1", "2"));

      final BasicLogger logger = new BasicLogger();
      logger.log(c);
      verify(gs).setModified(true);
    }
  }
}
