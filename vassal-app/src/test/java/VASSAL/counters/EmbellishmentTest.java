/*
 * Copyright 2020 Vassal Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.counters;

import java.lang.reflect.InvocationTargetException;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.tools.NamedKeyStroke;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

public class EmbellishmentTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Embellishment trait = new Embellishment();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // TODO Need some more complex Embellishment tests here
    trait = new Embellishment();


  }

  private static Embellishment createLayer(boolean followProperty) {
    final Embellishment layer = new Embellishment();
    layer.setInner(new BasicPiece(BasicPiece.ID + ";;;"));
    layer.activateKeyStroke = NamedKeyStroke.of("activateLayer"); // NON-NLS
    layer.increaseKeyStroke = NamedKeyStroke.of("increaseLayer"); // NON-NLS
    layer.decreaseKeyStroke = NamedKeyStroke.of("decreaseLayer"); // NON-NLS
    layer.nValues = 2;
    layer.alwaysActive = false;
    layer.followProperty = followProperty;
    layer.value = -1; // level 1, inactive
    return layer;
  }

  /** BasicPiece that counts full-state snapshots taken of the piece */
  private static class CountingPiece extends BasicPiece {
    int stateCalls = 0;

    CountingPiece() {
      super(BasicPiece.ID + ";;;");
    }

    @Override
    public String getState() {
      stateCalls++;
      return super.getState();
    }
  }

  @Test
  public void testUnhandledKeyTakesNoSnapshot() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final Embellishment layer = createLayer(false);
      final CountingPiece inner = new CountingPiece();
      layer.setInner(inner);

      layer.keyEvent(NamedKeyStroke.of("someOtherKey").getKeyStroke()); // NON-NLS

      assertEquals(0, inner.stateCalls);
    }
  }

  // getState() reads the piece's GamePiece id through the GameModule, so each test needs one
  @Test
  public void testUnhandledKeyReturnsNoCommand() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final Embellishment layer = createLayer(false);
      final String before = layer.getState();

      final Command c = layer.keyEvent(NamedKeyStroke.of("someOtherKey").getKeyStroke()); // NON-NLS

      assertNull(c);
      assertEquals(before, layer.getState());
    }
  }

  @Test
  public void testActivateKeyChangesLayer() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final Embellishment layer = createLayer(false);

      final Command c = layer.keyEvent(NamedKeyStroke.of("activateLayer").getKeyStroke()); // NON-NLS

      assertNotNull(c);
      assertEquals(1, layer.value);
    }
  }

  @Test
  public void testIncreaseKeyIgnoredWhenFollowingProperty() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final Embellishment layer = createLayer(true);
      final String before = layer.getState();

      final Command c = layer.keyEvent(NamedKeyStroke.of("increaseLayer").getKeyStroke()); // NON-NLS

      assertNull(c);
      assertEquals(before, layer.getState());
    }
  }

  @Test
  public void testAlwaysActiveLayerIgnoresActivateKey() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      // An always-active Layer can't be toggled, so its activate key must cost nothing
      final Embellishment layer = createLayer(false);
      layer.alwaysActive = true;
      final CountingPiece inner = new CountingPiece();
      layer.setInner(inner);

      final Command c = layer.keyEvent(NamedKeyStroke.of("activateLayer").getKeyStroke()); // NON-NLS

      assertNull(c);
      assertEquals(0, inner.stateCalls);
    }
  }

  @Test
  public void testHandledKeyTakesTwoSnapshots() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      // A real change needs exactly two snapshots: before, and after to build the Command
      final Embellishment layer = createLayer(false);
      final CountingPiece inner = new CountingPiece();
      layer.setInner(inner);

      final Command c = layer.keyEvent(NamedKeyStroke.of("activateLayer").getKeyStroke()); // NON-NLS

      assertNotNull(c);
      assertEquals(2, inner.stateCalls);
    }
  }
}
