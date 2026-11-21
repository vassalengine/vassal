/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module.turn;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class TurnTrackerSetTest {
  static class TurnTrackerAccessor extends TurnTracker {
    public TurnTrackerAccessor() {
      super();
      turnWindow = mock(TurnWindow.class);
    }

    @Override
    public void set() {
      super.set();
    }
  }

  /**
   * A Turn Tracker with no levels yet has nothing to set. Opening the Set
   * dialog used to ask the missing current level for its controls.
   */
  @Test
  public void setWithNoLevelsDoesNothing() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getPrefs()).thenReturn(new Prefs(new PrefsEditor(), ""));

      final TurnTrackerAccessor tracker = new TurnTrackerAccessor();

      assertDoesNotThrow(tracker::set);
    }
  }
}
