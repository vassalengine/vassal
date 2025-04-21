package VASSAL.build.module.turn;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import javax.swing.JToolBar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class CounterTurnTrackerTest {

  // A helper class to access protected functions.
  static class TurnTrackerAccessor extends TurnTracker {
    public void next() {
      super.next();
    }

    public void prev() {
      super.prev();
    }

    public String getTurnString() {
      return super.getTurnString().trim();
    }
  }

  // Mocked GameModule members required by the TurnTracker constructor.
  final GameState gameState = new GameState();
  final JToolBar toolbar = new JToolBar();
  final Prefs prefs = new Prefs(new PrefsEditor(), "");

  private void testSetup() {
    when(GameModule.getGameModule().getPrefs()).thenReturn(prefs);
    when(GameModule.getGameModule().getToolBar()).thenReturn(toolbar);
    when(GameModule.getGameModule().getGameState()).thenReturn(gameState);
  }

  @Test
  public void nestedCounterTurnTracker() {
    // Test the wrapping of two CounterTurnTrackers where one
    // is a sibling of the other.
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      testSetup();

      TurnTrackerAccessor tracker = new TurnTrackerAccessor();
      tracker.addTo(gm);

      // Top level counter wraps with sequence 10, 30, 50.
      CounterTurnLevel level1 = new CounterTurnLevel();
      level1.setAttribute(CounterTurnLevel.START, 10);
      level1.setAttribute(CounterTurnLevel.INCR, 20);
      level1.addTo(tracker);

      // Sibling counter wraps with sequence 1,2,3.
      CounterTurnLevel level2 = new CounterTurnLevel();
      level2.setAttribute(CounterTurnLevel.START, 1);
      level2.setAttribute(CounterTurnLevel.INCR, 1);
      level2.addTo(level1);

      // Set start and end values, with wrapping enabled.
      tracker.setState("0|10;0;true;50;1\\;0\\;true\\;3");
      tracker.setAttribute(TurnTracker.TURN_FORMAT, "$level1$-$level2$");

      // Check initial conditions and advance to next.
      for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
          String expected = String.format("%d-%d", (i*20)+10, j+1);
          assertEquals(expected, tracker.getTurnString());
          tracker.next();
        }
      }

      // Should be wrapped back to starting conditions.
      assertEquals("10-1", tracker.getTurnString());
      tracker.prev();

      // Check previous command.
      for (int i = 2; i >= 0; --i) {
        for (int j = 2; j >= 0; --j) {
          String expected = String.format("%d-%d", (i*20)+10, j+1);
          assertEquals(expected, tracker.getTurnString());
          tracker.prev();
        }
      }

      // Go back to initial conditions.
      tracker.next();
      assertEquals("10-1", tracker.getTurnString());
    }
  }
}
