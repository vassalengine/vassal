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

import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;

public class LabelerTest extends DecoratorTest {

  @Test
  public void testPieceNameInLabelBug3463() {
    // Putting piece name in label was causing infinite recursion
    // as labeler sets a new name which may be based on the label
    final String pieceName = "Test Piece"; // NON-NLS
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;" + pieceName + ";");
    final Labeler labeler = new Labeler(Labeler.ID, piece);

    labeler.setLabel("$" + BasicPiece.PIECE_NAME + "$");

    assertThat(pieceName, is(equalTo(labeler.getLabel())));
  }

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    // Default trait
    final Labeler t1 = new Labeler();
    serializeTest("Basic Label", t1); // NON-NLS

    // Try with a real type and state
    final Labeler t2 = new Labeler();
    t2.mySetType("label;76,130;Change Label;13;51,204,255;255,0,51;c;4;l;4;t;r;$pieceName$ ($label$);DialogInput;3;90;TextLabel2;A Label;description"); // NON-NLS
    t2.setInner(createBasicPiece());
    t2.setLabel("Label Text"); // NON-NLS // Sets the label text which is the only state field
    serializeTest("Complex Label", t2); // NON-NLS
  }

  @Test
  public void testStateOfLabelEndingWithBackslashBug15337() {
    // A label is free text, and on a UK keyboard the backslash key sits beside the left shift,
    // so a label ending with one is easily typed. The piece's state is each trait's state joined
    // with TABs; a token ending with a backslash used to swallow the delimiter after it, taking
    // every inner trait's state with it, and reading the state back threw
    // IllegalStateException: No state for trait=label;... See vassalengine/vassal#15337.
    final Labeler saved = new Labeler(Labeler.ID, createBasicPiece());
    saved.setLabel("test\\");

    final Labeler loaded = new Labeler(Labeler.ID, createBasicPiece());
    loaded.setState(saved.getState());

    assertThat(loaded.getState(), is(equalTo(saved.getState())));
  }

}
