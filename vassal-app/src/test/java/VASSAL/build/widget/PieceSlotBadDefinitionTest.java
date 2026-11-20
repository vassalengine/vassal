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
package VASSAL.build.widget;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.command.AddPiece;
import VASSAL.counters.GamePiece;
import VASSAL.tools.ErrorDialog;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * A piece definition which cannot be read should cost that piece, not the
 * module. PieceSlot.getPiece() already treats a definition which decodes to
 * nothing as bad data and carries on, but a definition which decodes and then
 * throws while being given its state escaped the whole buildFile walk, so the
 * module would not open at all.
 *
 * See vassalengine/vassal#14939, #14916, #14897, #14896, #14677 and #14491.
 */
public class PieceSlotBadDefinitionTest {

  private static final String DEFINITION = "+/null/piece;;;;/\tstate";

  private MockedStatic<GameModule> gameModule;
  private MockedStatic<ErrorDialog> errorDialog;
  private GameModule mod;
  private List<BadDataReport> reports;

  @BeforeEach
  public void setUp() {
    mod = mock(GameModule.class);
    gameModule = mockStatic(GameModule.class);
    gameModule.when(GameModule::getGameModule).thenReturn(mod);

    reports = new ArrayList<>();
    errorDialog = mockStatic(ErrorDialog.class);
    errorDialog.when(() -> ErrorDialog.dataWarning(any(BadDataReport.class)))
               .thenAnswer(inv -> reports.add(inv.getArgument(0)));
  }

  @AfterEach
  public void tearDown() {
    errorDialog.close();
    gameModule.close();
  }

  /** A slot built from the buildFile, which is where the definition lives. */
  private static PieceSlot slotWith(String definition) throws Exception {
    final Element e = DocumentBuilderFactory
      .newInstance()
      .newDocumentBuilder()
      .parse(new InputSource(new StringReader(
        "<" + PieceSlot.class.getName() + ">" + definition + "</" + PieceSlot.class.getName() + ">")))
      .getDocumentElement();

    final PieceSlot slot = new PieceSlot();
    slot.build(e);
    return slot;
  }

  /**
   * The reported case: the definition decodes, and setState() runs out of
   * state before it runs out of traits.
   */
  @Test
  public void aDefinitionWhoseStateIsTooShortIsReportedNotThrownBug14939() throws Exception {
    final GamePiece target = mock(GamePiece.class);
    doThrow(new IllegalStateException("No state for trait=prototype;Army"))
      .when(target).setState(anyString());
    when(mod.decode(DEFINITION)).thenReturn(new AddPiece(target, "short"));

    final PieceSlot slot = slotWith(DEFINITION);

    assertThat(slot.getPiece(), is(nullValue()));
    assertThat(reports.size(), is(1));
    assertThat(reports.get(0).getData(), containsString("piece"));
    // the cause is kept, so the error log still names what actually happened
    assertThat(reports.get(0).getCause(), instanceOf(IllegalStateException.class));
  }

  /** The slot does not retry on every call, as the sibling branch does not. */
  @Test
  public void anUnreadableDefinitionIsOnlyReportedOnce() throws Exception {
    final GamePiece target = mock(GamePiece.class);
    doThrow(new IllegalStateException("No state for trait=restrict;x"))
      .when(target).setState(anyString());
    when(mod.decode(DEFINITION)).thenReturn(new AddPiece(target, "short"));

    final PieceSlot slot = slotWith(DEFINITION);
    slot.getPiece();
    slot.getPiece();

    assertThat(reports.size(), is(1));
  }

  /** A definition which decodes to nothing is unchanged: already bad data. */
  @Test
  public void aDefinitionWhichDecodesToNothingIsStillReported() throws Exception {
    when(mod.decode(DEFINITION)).thenReturn(null);

    final PieceSlot slot = slotWith(DEFINITION);

    assertThat(slot.getPiece(), is(nullValue()));
    assertThat(reports.size(), is(1));
  }

  /** A definition which reads is unaffected, and reports nothing. */
  @Test
  public void aDefinitionWhichReadsIsUnaffected() throws Exception {
    final GamePiece target = mock(GamePiece.class);
    when(mod.decode(DEFINITION)).thenReturn(new AddPiece(target, "state"));

    final PieceSlot slot = slotWith(DEFINITION);

    assertThat(slot.getPiece(), is(sameInstance(target)));
    assertThat(reports, is(List.of()));
  }
}
