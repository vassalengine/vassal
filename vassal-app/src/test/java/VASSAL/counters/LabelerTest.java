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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.Font;
import java.lang.reflect.InvocationTargetException;

import VASSAL.build.GameModule;
import VASSAL.build.module.font.FontOrganizer;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class LabelerTest extends DecoratorTest {

  @Test
  public void testPieceNameInLabelBug3463() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Font Organizer
      final FontOrganizer fo = mock(FontOrganizer.class);
      when(fo.createFont(any(String.class), any(Integer.class), any(Integer.class))).thenReturn(new Font(Font.DIALOG, Font.PLAIN, 10));

      // Mock GameModule to return various resources
      final GameModule gm = mock(GameModule.class);
      when(gm.getFontOrganizer()).thenReturn(fo);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      // Putting piece name in label was causing infinite recursion
      // as labeler sets a new name which may be based on the label
      final String pieceName = "Test Piece"; // NON-NLS
      final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;" + pieceName + ";");
      final Labeler labeler = new Labeler(Labeler.ID, piece);

      labeler.setLabel("$" + BasicPiece.PIECE_NAME + "$");

      assertThat(pieceName, is(equalTo(labeler.getLabel())));
    }
  }

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    // Default trait
    final Labeler t1 = createLabeler(null);
    serializeTest("Basic Label", t1); // NON-NLS

    // Try with a real type and state
    final Labeler t2 = createLabeler("label;76,130;Change Label;13;51,204,255;255,0,51;c;4;l;4;t;r;$pieceName$ ($label$);DialogInput;3;90;TextLabel2;A Label;description");
    t2.setInner(createBasicPiece());
    t2.setLabel("Label Text"); // NON-NLS // Sets the label text which is the only state field
    serializeTest("Complex Label", t2); // NON-NLS
  }

  private Labeler createLabeler(String type) {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Font Organizer
      final FontOrganizer fo = mock(FontOrganizer.class);
      when(fo.createFont(any(String.class), any(Integer.class), any(Integer.class))).thenReturn(new Font(Font.DIALOG, Font.PLAIN, 10));

      // Mock GameModule to return various resources
      final GameModule gm = mock(GameModule.class);
      when(gm.getFontOrganizer()).thenReturn(fo);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      return type == null ? new Labeler() : new Labeler(type, null);
    }
  }
}
