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
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.tools.NamedKeyStroke;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class ReplaceTest extends DecoratorTest {


  @Test
  public void serializeTests() throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException {

    // Default piece
    Replace trait = createTrait();
    serializeTest("Default trait", trait); // NON-NLS

    //
    trait = createTrait();
    trait.command = new KeyCommand("testCommand", NamedKeyStroke.of("plugh"), trait); // NON-NLS
    trait.key = NamedKeyStroke.of("xyzzy"); // NON-NLS
    trait.markerSpec = "spec";
    trait.markerText = "text";
    trait.xOffset = 2;
    trait.yOffset = 3;
    trait.matchRotation = true;
    trait.afterBurnerKey = NamedKeyStroke.of("plover");
    trait.description = "desc";
    trait.placement = Replace.STACK_TOP;
    trait.above = true;
    trait.description = "plover2";
    serializeTest("NamedKeyStroke", trait); // NON-NLS


  }

  // Don't even try and run the editorTest on Replace!
  public void editorTest(String test, Decorator referenceTrait) {

  }


  private Replace createTrait() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Mock some GpID Support
      final GpIdSupport gpid = mock(GpIdSupport.class);

      // Mock GameModule to return various resources
      final GameModule gm = mock(GameModule.class);
      when(gm.getGpIdSupport()).thenReturn(gpid);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      return new Replace();
    }
  }
}
