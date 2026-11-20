/*
 *
 * Copyright (c) 2026 by the VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.build.module;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import VASSAL.build.GameModule;
import VASSAL.tools.filechooser.FileChooser;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The fast-forward flag suppresses sounds and the new-logfile prompt while a log is being replayed
 * at speed, so it must not outlive the replay. Anything which escapes {@code loadFastForward}
 * would otherwise leave it set for the rest of the session.
 */
public class GameStateFastForwardTest {

  /** @return GameState.loadFastForward, which is private */
  private static Method loadFastForward() throws Exception {
    final Method m = GameState.class.getDeclaredMethod("loadFastForward", boolean.class);
    m.setAccessible(true);
    return m;
  }

  @Test
  public void testFastForwardingIsClearedWhenTheLoadThrows() throws Exception {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final GameState gs = new GameState();
      final AtomicBoolean setDuringLoad = new AtomicBoolean();

      // Fail the way a log load really can: GameState.loadGame rethrows an IllegalStateException
      // it does not recognise, and a replayed command rethrows any Error it hits -- a
      // NoSuchMethodError from a module's stale custom code, say.
      final FileChooser fc = mock(FileChooser.class);
      when(fc.showOpenDialog()).thenAnswer(inv -> {
        setDuringLoad.set(gs.isFastForwarding());
        throw new IllegalStateException("loading blew up");
      });
      when(gm.getFileChooser()).thenReturn(fc);

      final InvocationTargetException e = assertThrows(
        InvocationTargetException.class, () -> loadFastForward().invoke(gs, false)
      );
      assertTrue(e.getCause() instanceof IllegalStateException, "the failure still propagates");

      assertTrue(setDuringLoad.get(), "the flag should be set while the log is being loaded");
      assertFalse(gs.isFastForwarding(), "and cleared once the load has failed");
    }
  }

  @Test
  public void testFastForwardingIsClearedWhenTheUserCancels() throws Exception {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final GameState gs = new GameState();

      final FileChooser fc = mock(FileChooser.class);
      when(fc.showOpenDialog()).thenReturn(FileChooser.CANCEL_OPTION);
      when(gm.getFileChooser()).thenReturn(fc);

      loadFastForward().invoke(gs, false);

      assertFalse(gs.isFastForwarding());
    }
  }
}
