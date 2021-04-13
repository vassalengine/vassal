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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doAnswer;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.tools.DataArchive;
import java.awt.Point;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class BasicPieceTest {

  @Test
  public void serializeTest() {

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      try (MockedStatic<Map> staticMap = Mockito.mockStatic(Map.class)) {

        final BasicPiece t1 = new BasicPiece();
        final String mapId = "map1"; // NON-NLS

        // Mock a map to handle having a piece added to it, and return an id
        final Map map = mock(Map.class);
        when(map.getIdentifier()).thenReturn(mapId);
        doAnswer(invocation -> {
          GamePiece p = (GamePiece) invocation.getArguments()[0];
          p.setMap(map);
          return null;
        }).when(map).addPiece(t1);
        staticMap.when(() -> Map.getMapById(mapId)).thenReturn(map);

        // Mock DataArchive to return a list of image names
        final DataArchive da = mock(DataArchive.class);
        when(da.getImageNames()).thenReturn(new String[0]);

        // Mock GameModule to return a DataArchive
        final GameModule gm = mock(GameModule.class);
        when(gm.getDataArchive()).thenReturn(da);

        staticGm.when(GameModule::getGameModule).thenReturn(gm);

        // Create a BasicPiece, set and check an initial type
        final String initialType = BasicPiece.ID + "A;B;;testUnit"; // NON-NLS
        final BasicPiece bp = new BasicPiece(initialType);
        assertThat(bp.getType(), is(equalTo(initialType)));

        final int initialX = 42;
        final int initialY = 58;
        final String gpId = "123";

        final String pp1Key = "key1"; // NON-NLS
        final String pp2Key = "key2"; // NON-NLS
        final String pp1Val = "val1"; // NON-NLS
        final String pp2Val = "val2"; // NON-NLS

        // Set initial state directly
        bp.setGpId(gpId);
        bp.setPosition(new Point(initialX, initialY));

        bp.setMap(map);

        bp.setPersistentProperty(pp1Key, pp1Val);
        bp.setPersistentProperty(pp2Key, pp2Val);

        final String initialState = bp.getState();

        // Ensure the same state and type are reported back from the Editor
        PieceEditor editor = bp.getEditor();
        assertThat(editor.getState(), is(equalTo(initialState)));
        assertThat(editor.getType(), is(equalTo(initialType)));

        // Create a new piece, apply the supplied Type and State, Check they match the original

        t1.mySetType(initialType);
        t1.setState(initialState);
        assertThat(bp.testEquals(t1), is(true));
      }
    }
  }
}