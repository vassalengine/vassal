package VASSAL.build.module;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.command.ChangePiece;
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
}
