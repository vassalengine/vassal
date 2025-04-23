package VASSAL.build.module.turn;

import VASSAL.build.GameModule;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.Iterator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class ListTurnTrackerTest {

  private final List<String> turnList = List.of(
          "alpha",
          "beta",
          "gamma"
  );

  // A subset of days.
  private final List<String> dayList = List.of(
          "Monday",
          "Tuesday",
          "Wednesday"
  );

  private final List<String> numbersList = List.of(
          "one",
          "two",
          "three"
  );

  // A helper class to access protected functions.
  static class TurnTrackerAccessor extends TurnTracker {

    public TurnTrackerAccessor() {
      super();
      turnWindow = tw;
    }

    public void next() {
      super.next();
    }

    public void prev() {
      super.prev();
    }

    public void reset() { super.reset(); }

    public String getTurnString() {
      return super.getTurnString().trim();
    }
  }

  // Mocked members of GameModule and TurnTracker
  static final Prefs prefs = new Prefs(new PrefsEditor(), "");
  static final TurnTracker.TurnWindow tw = mock(TurnTracker.TurnWindow.class);

  @Test
  public void turnTrackerNext() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);
      tracker.reset();

      // Check initial conditions, check advance to next, and one wrap around.
      for (int i = 0; i <= turnList.size(); ++i) {
        assertEquals(turnList.get(i % turnList.size()), level.getTurnString());
        tracker.next();
      }
    }
  }

  @Test
  public void turnTrackerPrev() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);
      tracker.reset();

      // Check initial conditions, wrap backwards and check backwards movement through the list.
      for (int i = turnList.size(); i >= 0; --i) {
        assertEquals(turnList.get(i % turnList.size()), level.getTurnString());
        tracker.prev();
      }
    }
  }

  // Move forward on a turn tracker with two lists that are siblings.
  @Test
  public void siblingListTurnTrackerNext() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      final String dayNames = String.join(",", dayList);
      final ListTurnLevel other = new ListTurnLevel();
      other.setAttribute("list", dayNames);
      other.addTo(tracker);

      tracker.reset(); // alpha
      tracker.next(); // beta
      tracker.next(); // gamma
      assertEquals("gamma", tracker.getTurnString());
      tracker.next(); // Monday
      tracker.next(); // Tuesday
      Iterator<TurnLevel> levels = tracker.getTurnLevels();
      assertEquals("alpha", levels.next().getTurnString());
      assertEquals("Tuesday", levels.next().getTurnString());
      assertEquals("Tuesday", tracker.getTurnString());
      tracker.next(); // Wednesday
      tracker.next(); // wrap
      levels = tracker.getTurnLevels();
      assertEquals("alpha", levels.next().getTurnString());
      assertEquals("Monday", levels.next().getTurnString());
      assertEquals("alpha", tracker.getTurnString());
    }
  }

  // Move forward on a turn tracker with two lists that are siblings.
  // First and last elements are not active.
  @Test
  public void siblingListInactiveElements() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      final String dayNames = String.join(",", dayList);
      final ListTurnLevel other = new ListTurnLevel();
      other.setAttribute("list", dayNames);
      other.addTo(tracker);

      // Active elements are:
      // beta -> gamma -> Monday -> Tuesday
      tracker.setState("0|0;0;0;false,true,true|0;1;0;true,true,false");

      tracker.next(); // beta
      tracker.next(); // gamma
      assertEquals("gamma", tracker.getTurnString());
      tracker.next(); // Monday
      tracker.next(); // Tuesday
      Iterator<TurnLevel> levels = tracker.getTurnLevels();
      assertEquals("beta", levels.next().getTurnString());
      assertEquals("Tuesday", levels.next().getTurnString());
      assertEquals("Tuesday", tracker.getTurnString());
      tracker.next(); // wrap
      levels = tracker.getTurnLevels();
      assertEquals("beta", levels.next().getTurnString());
      assertEquals("Monday", levels.next().getTurnString());
      assertEquals("beta", tracker.getTurnString());
      tracker.prev(); // wrap backwards
      levels = tracker.getTurnLevels();
      assertEquals("gamma", levels.next().getTurnString());
      assertEquals("Tuesday", levels.next().getTurnString());
      assertEquals("Tuesday", tracker.getTurnString());
      tracker.prev();
      assertEquals("Monday", tracker.getTurnString());
      tracker.prev();
      assertEquals("gamma", tracker.getTurnString());
    }
  }

  // Move forward on a turn track with two lists that are siblings.
  // Part 1: All elements in the second list are inactive.
  // Part 2: All elements of both lists are inactive.
  @Test
  public void siblingListInactiveList() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      final ListTurnLevel other = new ListTurnLevel();
      other.setAttribute("list", "1,2,3");
      other.addTo(tracker);

      tracker.setState("0|0;0;0;true,true,true|0;1;0;false,false,false");

      // Forward direction
      for (int i = 0; i < turnList.size() * 2; ++i) {
        assertEquals(turnList.get(i % turnList.size()), tracker.getTurnString());
        tracker.next();
      }

      // The previous advance skips 1,2,3 and wraps to alpha.
      assertEquals(turnList.get(0), tracker.getTurnString());

      // Reverse direction
      for (int i = turnList.size()-1; i >= 0; --i) {
        tracker.prev();
        assertEquals(turnList.get(i), tracker.getTurnString());
      }

      // Part 2
      // Beta is current, all inactive.
      tracker.setState("0|1;0;0;false,false,false|0;1;0;false,false,false");

      // Forward
      for (int i = 0; i < turnList.size() ; ++i) {
        assertEquals(turnList.get(1), tracker.getTurnString());
        tracker.next();
      }

      // Reverse
      for (int i = 0; i < turnList.size() ; ++i) {
        assertEquals(turnList.get(1), tracker.getTurnString());
        tracker.prev();
      }
    }
  }

  // Test for inactive element in the first position of a level.
  @Test
  public void turnTrackerNextFirstInactive() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      // The first element is not active.
      tracker.setState("0|0;0;0;false,true,true");

      // setState can select inactive levels.
      assertEquals("alpha", level.getTurnString());
      tracker.next(); // beta
      assertEquals("beta", level.getTurnString());
      tracker.prev(); // wrap around backwards skipping alpha
      assertEquals("gamma", level.getTurnString());
      tracker.next(); // beta
      assertEquals("beta", level.getTurnString());
    }
  }

  // Test for inactive element at the end of a level.
  @Test
  public void turnTrackerPrevLastInactive() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);
      tracker.setState("0|0;0;0;true,true,false");

      assertEquals("alpha", level.getTurnString());
      tracker.prev(); // wrap around backwards to beta
      assertEquals("beta", level.getTurnString());
      tracker.next(); // skips gamma
      assertEquals("alpha", level.getTurnString());

    }
  }

  @Test
  public void turnTrackerNextNestedLists() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      final String numberNames = String.join(",", numbersList);
      final ListTurnLevel level2 = new ListTurnLevel();
      level2.setAttribute("list", numberNames);
      level.addLevel(level2);

      tracker.reset();

      for (int i = 0; i < turnList.size(); ++i) {
        for (int j = 0; j < numbersList.size(); ++j) {
          assertEquals(turnList.get(i), level.getTurnString());
          assertEquals(numbersList.get(j), level2.getTurnString());
          tracker.next();
        }
      }
      assertEquals("alpha", level.getTurnString());
      assertEquals("one", level2.getTurnString());
    }
  }

  @Test
  public void nestedListInactiveElement() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      final String numberNames = String.join(",", numbersList);
      final ListTurnLevel level2 = new ListTurnLevel();
      level2.setAttribute("list", numberNames);
      level.addLevel(level2);

      // First element of the child list is inactive.
      tracker.setState("0|0;0;0;true,true,true;0\\;0\\;0\\;false,true,true");

      assertEquals("alpha", level.getTurnString());
      assertEquals("one", level2.getTurnString());
      tracker.next();

      for (int i = 0; i < turnList.size(); ++i) {
        for (int j = 1; j < numbersList.size(); ++j) {
          assertEquals(turnList.get(i), level.getTurnString());
          assertEquals(numbersList.get(j), level2.getTurnString());
          tracker.next();
        }
      }
      assertEquals("alpha", level.getTurnString());
      assertEquals("two", level2.getTurnString());
    }
  }

  @Test
  public void twoNestedListsOneInactive() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      final ListTurnLevel inactiveList = new ListTurnLevel();
      inactiveList.setAttribute("list", "odd_ball");
      level.addLevel(inactiveList);

      final String numberNames = String.join(",", numbersList);
      final ListTurnLevel level2 = new ListTurnLevel();
      level2.setAttribute("list", numberNames);
      level.addLevel(level2);

      // The top level list starts at beta and first is beta.
      // The first and only element of the first child list is inactive.
      tracker.setState("0|1;0;1;true,true,true;0\\;0\\;0\\;false;0\\;1\\;0\\;true,true,true");

      assertEquals("beta", level.getTurnString());
      assertEquals("odd_ball", inactiveList.getTurnString());
      assertEquals("one", level2.getTurnString());
      assertEquals("beta odd_ball", tracker.getTurnString());

      // Forward
      for (int i = 0; i < turnList.size(); ++i) {
        for (String s : numbersList) {
          tracker.next();
          final String expected = turnList.get((i + 1) % turnList.size()) + " " + s;
          assertEquals(expected, tracker.getTurnString());
        }
      }

      // Reverse
      for (int i = turnList.size(); i > 0; --i) {
        for (int j = numbersList.size() - 1; j >= 0; --j) {
          final String expected = turnList.get(i % turnList.size()) + " " + numbersList.get(j);
          assertEquals(expected, tracker.getTurnString());
          tracker.prev();
        }
      }
    }
  }

  // Multiple sub-lists with all inactive.
  @Test
  public void allNestedListsInactive() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final String turnNames = String.join(",", turnList);
      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", turnNames);
      level.addTo(tracker);

      final ListTurnLevel inactiveList = new ListTurnLevel();
      inactiveList.setAttribute("list", "odd_ball");
      level.addLevel(inactiveList);

      final String numberNames = String.join(",", numbersList);
      final ListTurnLevel level2 = new ListTurnLevel();
      level2.setAttribute("list", numberNames);
      level.addLevel(level2);

      // The top level list starts at gamma and first is beta.
      // The first and only element of the first child list is inactive.
      tracker.setState("0|2;0;1;true,true,true;0\\;0\\;0\\;false;0\\;1\\;0\\;false,false,false");

      assertEquals("gamma", level.getTurnString());
      assertEquals("odd_ball", inactiveList.getTurnString());
      assertEquals("one", level2.getTurnString());
      assertEquals("gamma odd_ball", tracker.getTurnString());

      // Forward
      for (int i = 0; i < turnList.size(); ++i) {
        tracker.next();
        final String expected = turnList.get(i) + " odd_ball";
        assertEquals(expected, tracker.getTurnString());
      }

      // Reverse
      for (int i = turnList.size()-1; i >= 0; --i) {
        final String expected = turnList.get(i % turnList.size()) + " odd_ball";
        assertEquals(expected, tracker.getTurnString());
        tracker.prev();
      }
    }
  }

  // A four levels deep turn list with all levels set to the last value.
  // Test that the next advance resets all levels.
  @Test
  public void turnTrackerNextWrapAll() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(prefs);

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      final ListTurnLevel level = new ListTurnLevel();
      level.setAttribute("list", "alpha,bravo");
      level.addTo(tracker);

      final ListTurnLevel level2 = new ListTurnLevel();
      level2.setAttribute("list", "charlie,delta");
      level.addLevel(level2);

      final ListTurnLevel level3 = new ListTurnLevel();
      level3.setAttribute("list", "echo,foxtrot");
      level2.addLevel(level3);

      final ListTurnLevel level4 = new ListTurnLevel();
      level4.setAttribute("list", "golf,hotel");
      level3.addLevel(level4);

      tracker.setState("0|1;0;0;true,true;1\\;0\\;0\\;true,true\\;1\\\\;0\\\\;0\\\\;true,true\\\\;1\\\\\\;0\\\\\\;0\\\\\\;true,true");

      assertEquals("bravo", level.getTurnString());
      assertEquals("delta", level2.getTurnString());
      assertEquals("foxtrot", level3.getTurnString());
      assertEquals("hotel", level4.getTurnString());

      tracker.next();

      assertEquals("alpha", level.getTurnString());
      assertEquals("charlie", level2.getTurnString());
      assertEquals("echo", level3.getTurnString());
      assertEquals("golf", level4.getTurnString());
    }
  }
}
