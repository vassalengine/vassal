package VASSAL.build.module;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.Logger;
import VASSAL.tools.RecursionLimitException;
import org.codehaus.plexus.util.StringUtils;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.awt.*;
import java.lang.reflect.Field;
import java.util.ArrayDeque;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class DoActionButtonTest {

  /**
   * Test the enqueuing of commands onto the GameModule.pausedCommands deque
   * during multi-command construction. The DoActionButton uses the deque to
   * generate a sequence of commands which converts into a single composite
   * command.
   *
   * @throws RecursionLimitException source DoActionButton.doActions
   * @throws IllegalAccessException source Field.set
   * @throws NoSuchFieldException source Class.getDeclaredField
   */
  @Test
  public void deque_nullCommandDepth_expectOne() throws RecursionLimitException, IllegalAccessException, NoSuchFieldException {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);

      // The GameModule is mocked, but since some real member functions
      // are invoked, GameModule requires some partial initialization.
      // Get access to private members and initialize them.
      Field field = gm.getClass().getDeclaredField("loggingLock");
      field.setAccessible(true);
      field.set(gm, new Object());
      field = gm.getClass().getDeclaredField("pausedCommands");
      field.setAccessible(true);
      field.set(gm, new ArrayDeque<>());

      // Mock the chatter, server and logger.
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      final Chatter chatter = mock(Chatter.class);
      final ServerConnection server = mock(ServerConnection.class);
      final Logger logger = mock(Logger.class);
      when(gm.getChatter()).thenReturn(chatter);
      when(gm.getServer()).thenReturn(server);
      when(gm.getLogger()).thenReturn(logger);

      // Indicate which of the "real" functions to call.
      doCallRealMethod().when(gm).sendAndLog(any(Command.class));
      when(gm.pauseLogging()).thenCallRealMethod();
      when(gm.resumeLogging()).thenCallRealMethod();

      // Setup a simple DoActionButton to generate a report.
      DoActionButton button = new DoActionButton();
      button.setAncestor(gm);
      button.setAttribute(DoActionButton.DO_REPORT, true);
      button.setAttribute(DoActionButton.REPORT_FORMAT, "Report format text");

      button.doActions();

      // Capture the output sent to others.
      ArgumentCaptor<Command> commandCapture = ArgumentCaptor.forClass(Command.class);
      verify(server).sendToOthers(commandCapture.capture());
      Command command = commandCapture.getValue();

      // The toString() method does a recursive descent concatenating the command
      // names with a plus sign separator. Count the separators to get the total
      // command count less one.
      // Expecting a single null command and a display command.
      assertEquals(1, StringUtils.countMatches(command.toString(), "+"));
    }
  }
}
