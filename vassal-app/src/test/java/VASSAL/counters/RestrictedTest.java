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

import static org.mockito.Mockito.mock;

import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;

import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class RestrictedTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Restricted trait = createTrait();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = createTrait();

    trait.mySetType(Restricted.ID + "side1,side2;true;true");
    trait.mySetState("A-player");
    trait.setDescription("plover");
    serializeTest("Complex trait", trait); // NON-NLS

  }

  private Restricted createTrait() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Mock some GpID Support
      final GpIdSupport gpid = mock(GpIdSupport.class);

      // Mock GameModule to return various resources
      final GameModule gm = mock(GameModule.class);
      //doNothing().when(gm).addSideChangeListenerToPlayerRoster(any());

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      return new Restricted();
    }
  }
}
