package VASSAL.build.module;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.chat.node.NodeClient;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.preferences.Prefs;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.RecursionLimitException;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

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
   * Step through a complete log file.
   */
  @Test
  public void stepThroughLogExpectMatchingOutput() throws RecursionLimitException, IOException {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      GameState gameState = new GameState();
      staticGm.when(gm::getGameState).thenReturn(gameState);

      Chatter chatter = new Chatter();
      staticGm.when(gm::getChatter).thenReturn(chatter);

      // Capture the argument for the sendAndLog calls.
      ArgumentCaptor<Command> sendAndLogCaptor = ArgumentCaptor.forClass(Command.class);

      BasicLogger logger = new BasicLogger();

      char letter = 'A';
      for (int i=0; i<4; i++) {
        final Command c = new Chatter.DisplayText(gm.getChatter(), Character.toString(letter));
        letter++;
        logger.logInput.add(c);
      }

      int index = 0;
      while (logger.isReplaying()) {
        logger.step();
        index++;
      }
      // Capture the sendAndLog output.
      verify(gm, times(index)).sendAndLog(sendAndLogCaptor.capture());
      final List<Command> commands = sendAndLogCaptor.getAllValues();
      // Compare logInput to the commands output.
      for (int i=0; i<index; i++) {
        assertEquals(logger.logInput.get(i), commands.get(i));
      }
    }
  }

  /**
   * Step through log file, then undo all checking for a proper undo sequence.
   */
  @Test
  public void outputLogExpectUndoInReverseOrder() throws RecursionLimitException, IOException {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      Chatter chatter = new Chatter();
      staticGm.when(gm::getChatter).thenReturn(chatter);
      BasicLogger logger = new BasicLogger();
      staticGm.when(gm::getBasicLogger).thenReturn(logger);

      ServerConnection server= new NodeClient("moduleName", "playerId", logger,  "host", 0, null);
      staticGm.when(gm::getServer).thenReturn(server);

      char letter = 'A';
      for (int i=0; i<4; i++) {
        final Command c = new Chatter.DisplayText(gm.getChatter(), Character.toString(letter));
        letter++;
        logger.log(c);
      }

      final int cmdCount = logger.logOutput.size();
      for (int i=0; i<cmdCount; i++) {
        logger.undo();
      }

      // one extra to confirm it is ignored.
      logger.undo();

      assertEquals(8, logger.logOutput.size());

      // Regex pattern to match the last char within the square brackets.
      Pattern p = Pattern.compile("\\[.*(.)]");
      // Verify that undo commands are in reverse order.
      // First half of list contains the commands, second half the undo commands.
      // For example: comparing [A] to [* UNDO: A] is considered a match.
      final int size = logger.logOutput.size();
      for (int i=0; i<size/2; i++) {
        Matcher m1 = p.matcher(logger.logOutput.get(i).toString());
        Matcher m2 = p.matcher(logger.logOutput.get(size-i-1).toString());
        assertTrue(m1.find());
        assertTrue(m2.find());
        assertEquals(m1.group(1), m2.group(1));
      }
    }
  }

  @Test
  public void alternatingUndoLogWithCommandInjectionDuringUndo() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      BasicLogger logger = new BasicLogger();
      staticGm.when(gm::getBasicLogger).thenReturn(logger);
      GameState gameState = new GameState();
      staticGm.when(gm::getGameState).thenReturn(gameState);
      ServerConnection server = new NodeClient("moduleName", "playerId", logger, "host", 0, null);
      staticGm.when(gm::getServer).thenReturn(server);
      Chatter chatter = new Chatter();
      staticGm.when(gm::getChatter).thenReturn(chatter);

      // Mock sendAndLog removing the send while retaining the logging portion.
      staticGm.when(()->gm.sendAndLog(any(Command.class))).thenAnswer(new Answer<Object>() {
        @Override
        public Object answer(InvocationOnMock input) throws Throwable {
          logger.log(input.getArgument(0));
          return null;
        }
      });

      for (char letter = 'A'; letter <= 'C'; ++letter) {
        Command c = new Chatter.DisplayText(gm.getChatter(), Character.toString(letter));
        logger.logInput.add(c);
        Command undo = new BasicLogger.UndoCommand(true)
                .append(c.getUndoCommand())
                .append((new BasicLogger.UndoCommand(false)));
        logger.logInput.add(undo);
      }

      while (logger.isReplaying()) {
        logger.step();
      }
      logger.undo();  // Undo Undo C
      logger.undo();  // Undo C

      logger.log(new Chatter.DisplayText(gm.getChatter(),"User message"));
      logger.undo();  // Undo User message
      logger.undo();  // Undo Undo B
      logger.undo();  // Undo B

      Command b = new Chatter.DisplayText(gm.getChatter(), "B");
      Command undoB = new BasicLogger.UndoCommand(true)
              .append(b.getUndoCommand())
              .append((new BasicLogger.UndoCommand(false)));
      assertEquals(undoB.toString(), logger.logOutput.get(logger.logOutput.size()-1).toString());

      logger.undo();
      logger.undo();

      // Confirm return to the start of the log file.
      assertTrue(logger.isReplaying());
      assertFalse(logger.undoAction.isEnabled());
    }
  }

  /**
   * Step through and then fully undo a log file containing Undo commands.
   * The outputLog should contain double the log file commands with the
   * same number of step and undo commands.
   * @throws RecursionLimitException
   * @throws IOException
   */
  @Test
  public void processUndoDuringLogStepping() throws RecursionLimitException, IOException {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      BasicLogger logger = new BasicLogger();
      staticGm.when(gm::getBasicLogger).thenReturn(logger);

      GameState gameState = new GameState();
      staticGm.when(gm::getGameState).thenReturn(gameState);
      ServerConnection server = new NodeClient("moduleName", "playerId", logger, "host", 0, null);
      staticGm.when(gm::getServer).thenReturn(server);
      Chatter chatter = new Chatter();
      staticGm.when(gm::getChatter).thenReturn(chatter);

      // Mock sendAndLog() bypassing the send while retaining the logging portion.
      // This adds the command to logOutput.
      staticGm.when(()->gm.sendAndLog(any(Command.class))).thenAnswer(new Answer<Object>() {
        @Override
        public Object answer(InvocationOnMock input) throws Throwable {
          logger.log(input.getArgument(0));
          return null;
        }
      });

      // Simulate a simple log file with some display text and an undo.
      logger.logInput.add(new Chatter.DisplayText(gm.getChatter(),"First message"));
      logger.logInput.add(
              new BasicLogger.UndoCommand(true)
                      .append(new Chatter.DisplayText(gm.getChatter(),"logged undo"))
                      .append((new BasicLogger.UndoCommand(false))));
      logger.step();
      logger.step();
      logger.undo();
      logger.undo();

      assertEquals(logger.logInput.size() * 2, logger.logOutput.size());
     }
  }

  @Test
  public void stepUndoRedoExpectCorrectButtonStates() throws RecursionLimitException, IOException {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      BasicLogger logger = new BasicLogger();
      staticGm.when(gm::getBasicLogger).thenReturn(logger);

      GameState gameState = new GameState();
      staticGm.when(gm::getGameState).thenReturn(gameState);
      ServerConnection server = new NodeClient("moduleName", "playerId", logger, "host", 0, null);
      staticGm.when(gm::getServer).thenReturn(server);

      Chatter chatter = new Chatter();
      staticGm.when(gm::getChatter).thenReturn(chatter);

      // Mock sendAndLog() bypassing the send while retaining the logging portion.
      // This adds the command to logOutput.
      staticGm.when(()->gm.sendAndLog(any(Command.class))).thenAnswer(new Answer<Object>() {
        @Override
        public Object answer(InvocationOnMock input) throws Throwable {
          logger.log(input.getArgument(0));
          return null;
        }
      });

      // Simulate a simple log file with some display text and an undo.
      logger.logInput.add(new Chatter.DisplayText(gm.getChatter(),"OutputLog message"));
      logger.logInput.add(
              new BasicLogger.UndoCommand(true)
                      .append(new Chatter.DisplayText(gm.getChatter(),"logged undo"))
                      .append((new BasicLogger.UndoCommand(false))));

      // Inject a user action, prior to log playback, to offset input and output logs.
      logger.logOutput.add(new Chatter.DisplayText(gm.getChatter(),"User Action"));

      logger.step();
      logger.step();
      assertFalse(logger.isReplaying());  // at end of log file
      logger.undo();
      assertTrue(logger.isReplaying());
      logger.undo();
      logger.undo();  // Undo OutputLog message

      logger.step();  // Replay log file
      logger.step();
      assertFalse(logger.isReplaying());  // at end of log file
      logger.undo();
      logger.undo();
      assertFalse(logger.undoAction.isEnabled());
    }
  }
}
