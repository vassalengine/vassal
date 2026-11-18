/*
 * Copyright 2026 Vassal Development Team
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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JList;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.PieceSlot;
import VASSAL.configure.ConfigureTree;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ResourcePathFinder;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests for the Multi-Piece Editor ("Edit all contained pieces").
 *
 * <p>Its one invariant is that the pieces it edits all carry the same list of traits: the
 * constructor drops any piece that does not match the first one, and from then on every
 * structural change has to be applied to all of them at the same position. A change that
 * lands at a different position in different pieces breaks the next edit, because an edit
 * applies one trait's type string to whatever trait each piece happens to hold there. See
 * vassalengine/vassal#15027.</p>
 */
public class MassPieceDefinerTest {

  /** BasicPiece / Marker / Embellishment / Marker. */
  private static GamePiece samplePiece() {
    final BasicPiece basic = new BasicPiece();
    final Marker nation = new Marker(Marker.ID + "Nation", basic);
    final Embellishment layer = new Embellishment();
    layer.setInner(nation);
    return new Marker(Marker.ID + "Side", layer);
  }

  /** A Configurable that just holds PieceSlots, standing in for a Piece Window. */
  public static class SlotHolder extends AbstractConfigurable {
    @Override public String[] getAttributeDescriptions() { return new String[0]; }
    @Override public Class<?>[] getAttributeTypes() { return new Class<?>[0]; }
    @Override public String[] getAttributeNames() { return new String[0]; }
    @Override public String getAttributeValueString(String key) { return null; }
    @Override public void setAttribute(String key, Object value) { }
    @Override public Class<?>[] getAllowableConfigureComponents() { return new Class<?>[0]; }
    @Override public HelpFile getHelpFile() { return null; }
    @Override public void addTo(Buildable parent) { }
    @Override public void removeFrom(Buildable parent) { }
  }

  private static List<String> traitsOf(GamePiece p) {
    final List<String> names = new ArrayList<>();
    while (p instanceof Decorator) {
      names.add(0, p.getClass().getSimpleName());
      p = ((Decorator) p).getInner();
    }
    names.add(0, p.getClass().getSimpleName());
    return names;
  }

  /**
   * Select a trait in a definer's In-Use list, which is what the user does by clicking on
   * it, or clear the selection for a negative index. The list is private and has no
   * accessor, so this has to be done by reflection. Note that JList.setSelectedIndex(-1)
   * does nothing at all, so clearing has to be asked for separately.
   */
  private static void clickTrait(PieceDefiner definer, int index) {
    try {
      final Field f = PieceDefiner.class.getDeclaredField("inUseList");
      f.setAccessible(true);
      final JList<?> list = (JList<?>) f.get(definer);
      if (index < 0) {
        list.clearSelection();
      }
      else {
        list.setSelectedIndex(index);
      }
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
  }

  /** The PieceDefiner behind one of the pieces being edited alongside the visible one. */
  private static PieceDefiner subDefiner(MassPieceDefiner mpd, int index) {
    try {
      final Object entry = mpd.definers.get(index);
      final Field f = entry.getClass().getDeclaredField("definer");
      f.setAccessible(true);
      return (PieceDefiner) f.get(entry);
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
  }

  /** Enough of a GameModule for PieceDefiner to build itself and to create pieces. */
  private static GameModule stubModule() {
    final GameModule gm = mock(GameModule.class);

    final Prefs prefs = mock(Prefs.class);
    when(prefs.getValue(anyString())).thenReturn(4);
    when(gm.getPrefs()).thenReturn(prefs);

    final ResourcePathFinder rpf = mock(ResourcePathFinder.class);
    when(rpf.findImagePath(anyString())).thenReturn("dummy.png");
    when(gm.getResourcePathFinder()).thenReturn(rpf);
    when(gm.getDataArchive()).thenReturn(mock(DataArchive.class));

    final BasicCommandEncoder encoder = new BasicCommandEncoder();
    when(gm.createPiece(anyString()))
      .thenAnswer(i -> encoder.createPiece(i.getArgument(0)));
    when(gm.createPiece(anyString(), any()))
      .thenAnswer(i -> encoder.createDecorator(i.getArgument(0), i.getArgument(1)));

    return gm;
  }

  /**
   * Run the body with GameModule.getGameModule() answering with a stub module. The stub is
   * finished before the static stubbing begins: building mocks inside thenReturn() leaves
   * Mockito's static stubbing half done.
   */
  private static void withStubModule(Runnable body) {
    final GameModule gm = stubModule();
    try (MockedStatic<GameModule> stat = Mockito.mockStatic(GameModule.class)) {
      stat.when(GameModule::getGameModule).thenReturn(gm);
      body.run();
    }
  }

  /** The editor plus the slots it writes back to when Ok is pressed. */
  private static class Fixture {
    private final MassPieceDefiner mpd;
    private final List<PieceSlot> slots = new ArrayList<>();

    Fixture(int pieceCount) {
      final SlotHolder holder = new SlotHolder();
      for (int i = 0; i < pieceCount; i++) {
        final PieceSlot slot = new PieceSlot(samplePiece());
        slots.add(slot);
        holder.add(slot);
      }
      mpd = new MassPieceDefiner(holder, mock(ConfigureTree.class));
    }

    void assertEveryPieceMatchesTheEditedOne() {
      mpd.save();
      final List<String> expected = traitsOf(mpd.getPiece());
      for (int i = 0; i < slots.size(); i++) {
        assertThat("piece " + i, traitsOf(slots.get(i).getPiece()), is(equalTo(expected)));
      }
    }
  }

  @Test
  public void aPastedTraitLandsAtTheSamePositionInEveryPieceBug15027() {
    withStubModule(() -> {
      final Fixture f = new Fixture(2);
      f.mpd.copy(1);          // copy the Marker just above the BasicPiece
      clickTrait(f.mpd, 3);   // click the outermost trait
      f.mpd.paste();
      f.assertEveryPieceMatchesTheEditedOne();
    });
  }

  @Test
  public void aPastedTraitLandsAtTheSamePositionWithNothingSelectedBug15027() {
    withStubModule(() -> {
      final Fixture f = new Fixture(2);
      f.mpd.copy(2);
      clickTrait(f.mpd, -1);  // no trait selected
      f.mpd.paste();
      f.assertEveryPieceMatchesTheEditedOne();
    });
  }

  @Test
  public void movingATraitToTheTopMovesItInEveryPiece() {
    withStubModule(() -> {
      final Fixture f = new Fixture(2);
      f.mpd.moveDecoratorTop(3);
      f.assertEveryPieceMatchesTheEditedOne();
    });
  }

  @Test
  public void movingATraitToTheBottomMovesItInEveryPiece() {
    withStubModule(() -> {
      final Fixture f = new Fixture(2);
      f.mpd.moveDecoratorBottom(1);
      f.assertEveryPieceMatchesTheEditedOne();
    });
  }

  @Test
  public void addingATraitAddsItOnceToEveryPiece() {
    withStubModule(() -> {
      final Fixture f = new Fixture(2);
      final int before = traitsOf(f.mpd.getPiece()).size();

      clickTrait(f.mpd, 2);
      f.mpd.addTrait(new Marker());

      assertThat(traitsOf(f.mpd.getPiece()).size(), is(equalTo(before + 1)));
      f.assertEveryPieceMatchesTheEditedOne();
    });
  }

  /**
   * Containment: should the trait lists drift apart anyway, a piece whose trait at the
   * edited position is a different kind of trait is left alone rather than handed a type
   * string its decoder cannot read, which is the NoSuchElementException reported in
   * vassalengine/vassal#15027, #13607 and #12377.
   */
  @Test
  public void aPieceWhoseTraitDoesNotMatchIsLeftAloneBug15027() {
    withStubModule(() -> {
      final Fixture f = new Fixture(1);
      final PieceDefiner sub = subDefiner(f.mpd, 0);

      // Force the drift this fix is meant to prevent: the editor holds a Marker at
      // position 3 where this piece now holds the Embellishment.
      sub.inUseModel.removeElementAt(1);

      assertThat(sub.edit(3), is(false));
    });
  }
}
