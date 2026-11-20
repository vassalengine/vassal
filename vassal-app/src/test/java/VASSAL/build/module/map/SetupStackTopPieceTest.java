/*
 * Copyright (c) 2026 by the VASSAL Development Team
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
package VASSAL.build.module.map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import VASSAL.build.GameModule;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * An At-Start Stack's "top piece" is the slot its two Editor views draw.
 * Neither checks that the slot actually holds a piece --- only that there is
 * a slot --- so an empty first slot reached PieceCloner.clonePiece(null) and
 * threw in AddPiece's constructor, which is what the Reposition Stack button
 * does. See vassalengine/vassal#14405 and #14161.
 *
 * StackConfigurer is a JFrame and cannot be built in a headless test, so what
 * is tested here is the method it gets the slot from, and the one thing it
 * then does with it.
 */
public class SetupStackTopPieceTest {

  /** Marks a slot as empty, which is all an empty slot is. */
  private static final GamePiece NO_PIECE = null;

  private MockedStatic<GameModule> gameModule;

  @BeforeEach
  public void setUp() {
    // PieceSlot.setPiece() re-encodes the definition through the module, and
    // PieceCloner builds a BasicPiece clone through it.
    final GameModule mod = mock(GameModule.class);
    when(mod.encode(any(Command.class))).thenReturn("+/null/piece;;;;\\");
    when(mod.createPiece(anyString())).thenAnswer(inv -> new BasicPiece());

    gameModule = mockStatic(GameModule.class);
    gameModule.when(GameModule::getGameModule).thenReturn(mod);
  }

  @AfterEach
  public void tearDown() {
    gameModule.close();
  }

  /**
   * A slot given NO_PIECE is left as the Editor leaves one nobody has defined
   * a piece in: getPiece() returns null without decoding anything.
   */
  private static SetupStack stackOf(GamePiece... pieces) {
    final SetupStack stack = new SetupStack();
    for (final GamePiece piece : pieces) {
      final PieceSlot slot = new PieceSlot();
      if (piece != null) {
        slot.setPiece(piece);
      }
      stack.add(slot);
    }
    return stack;
  }

  @Test
  public void aStackWhoseOnlySlotIsEmptyHasNoTopPieceBug14405() {
    assertThat(stackOf(NO_PIECE).getTopPiece(), is(nullValue()));
  }

  @Test
  public void anEmptySlotIsPassedOverForOneWhichHasAPieceBug14405() {
    assertThat(stackOf(NO_PIECE, new BasicPiece()).getTopPiece().getPiece(),
      is(notNullValue()));
  }

  /**
   * The crash itself. Cloning the top piece is the first thing the Reposition
   * Stack dialog does, and against the unfixed engine this throws
   * "Cannot invoke VASSAL.counters.GamePiece.getState() because p is null"
   * at AddPiece.&lt;init&gt;, by way of PieceCloner.clonePiece --- the three
   * frames #14405 and #14161 report.
   */
  @Test
  public void theTopPieceOfAStackWithAnEmptyFirstSlotCanBeClonedBug14405() {
    final PieceSlot top = stackOf(NO_PIECE, new BasicPiece()).getTopPiece();

    assertThat(PieceCloner.getInstance().clonePiece(top.getPiece()),
      is(notNullValue()));
  }

  @Test
  public void aStackWhoseFirstSlotHasAPieceIsUnaffected() {
    assertThat(stackOf(new BasicPiece(), NO_PIECE).getTopPiece().getPiece(),
      is(notNullValue()));
  }

  @Test
  public void aStackWithNoSlotsAtAllHasNoTopPiece() {
    assertThat(stackOf().getTopPiece(), is(nullValue()));
  }
}
