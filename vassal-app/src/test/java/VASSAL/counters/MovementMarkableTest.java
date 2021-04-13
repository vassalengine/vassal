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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.tools.DataArchive;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import java.awt.image.BufferedImage;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class MovementMarkableTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    MovementMarkable trait = createTrait(null);

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = createTrait("markmoved;Undo16.gif;1;2;xyzzy;" + NamedHotKeyConfigurer.encode(NamedKeyStroke.of("plugh"))); // NON-NLS
    trait.setMoved(true);
    trait.setDescription("plover");
    serializeTest("Complex trait", trait); // NON-NLS
  }

  private MovementMarkable createTrait(String type) {

    // Create a dummy ImageOp before mocking Op
    ImageOp im = Op.load(new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR));

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      try (MockedStatic<Op> staticOp = Mockito.mockStatic(Op.class)) {

        // Now return the dummy image when asked instead of looking in the archive
        staticOp.when(() -> Op.load(any(String.class))).thenReturn(im);

        // Mock DataArchive to return a list of image names
        final DataArchive da = mock(DataArchive.class);
        when(da.getImageNames()).thenReturn(new String[0]);

        // Mock GameModule to return a DataArchive
        final GameModule gm = mock(GameModule.class);
        when(gm.createPiece(anyString())).thenReturn(new BasicPiece());

        staticGm.when(GameModule::getGameModule).thenReturn(gm);
        return type == null ? new MovementMarkable() : new MovementMarkable(type, null);
      }
    }
  }
}
