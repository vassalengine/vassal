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

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.configure.PieceAccessConfigurer;
import VASSAL.tools.NamedKeyStroke;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class ObscurableTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Obscurable trait = creatObscurable();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Peek Style
    trait = creatObscurable();
    trait.keyCommand = NamedKeyStroke.of("xyzzy");
    trait.imageName = "Undo16.gif";
    trait.hideCommand = "plugh";
    trait.displayStyle = 'P';
    trait.peekKey = NamedKeyStroke.of("plover");
    trait.maskName = "masked";
    trait.access = PieceAccessConfigurer.decode("side:");
    trait.peekCommand = "peeking";
    trait.description = "plover";
    serializeTest("Peek Style", trait); // NON-NLS

    // Image Style
    trait = creatObscurable();
    trait.keyCommand = NamedKeyStroke.of("xyzzy");
    trait.imageName = "Undo16.gif";
    trait.hideCommand = "plugh";
    trait.displayStyle = 'I';
    trait.obscuredToOthersImage = "moved.gif";
    trait.maskName = "masked";
    trait.access = PieceAccessConfigurer.decode("side:");
    trait.peekCommand = "peeking";
    trait.description = "plover";
    serializeTest("Image Style", trait); // NON-NLS

    // Inset Style
    trait = creatObscurable();
    trait.keyCommand = NamedKeyStroke.of("xyzzy");
    trait.imageName = "Undo16.gif";
    trait.hideCommand = "plugh";
    trait.displayStyle = 'I';
    trait.maskName = "masked";
    trait.access = PieceAccessConfigurer.decode("side:");
    trait.description = "plover";
    serializeTest("Inset Style", trait); // NON-NLS
  }

  public Obscurable creatObscurable() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

       // Mock GameModule to create a BasicPiece when requested
      final GameModule gm = mock(GameModule.class);
      when(gm.createPiece(anyString())).thenAnswer(i -> new BasicPiece((String) i.getArguments()[0]));

      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      return new Obscurable();
    }
  }
}
