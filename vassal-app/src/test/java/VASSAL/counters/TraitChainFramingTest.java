/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Point;

import org.junit.jupiter.api.Test;

import VASSAL.build.MockModuleTest;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.lang.Pair;

/**
 * Tests of the framing of a piece's trait chain: the flat framing written
 * by {@link Decorator#getType()} and {@link Decorator#getState()}, the
 * nested framing VASSAL wrote before 3.8, and the decoding of both by
 * {@link BasicCommandEncoder#createPiece(String)},
 * {@link Decorator#setState(String)} and
 * {@link Decorator#mergeState(String, String)}.
 */
public class TraitChainFramingTest extends MockModuleTest {

  private static final String BASIC_TYPE = BasicPiece.ID + ";;;Unit"; // NON-NLS

  /** A basic piece with {@code n} Markers over it, each with a distinct key and value. */
  private static GamePiece chain(int n) {
    final BasicPiece basic = new BasicPiece(BASIC_TYPE);
    basic.setPosition(new Point(17, 23));
    GamePiece p = basic;
    for (int i = n; i >= 1; --i) {
      p = marker("key" + i, "value" + i, p); // NON-NLS
    }
    return p;
  }

  private static Marker marker(String key, String value, GamePiece inner) {
    final Marker m = new Marker(Marker.ID + key, inner);
    m.values = new String[] {value};
    return m;
  }

  /** The type of a chain framed the way VASSAL 3.7 and earlier framed it. */
  private static String nestedType(GamePiece p) {
    if (p instanceof Decorator) {
      final Decorator d = (Decorator) p;
      return new SequenceEncoder(d.myGetType(), '\t').append(nestedType(d.getInner())).getValue();
    }
    return p.getType();
  }

  /** The state of a chain framed the way VASSAL 3.7 and earlier framed it. */
  private static String nestedState(GamePiece p) {
    if (p instanceof Decorator) {
      final Decorator d = (Decorator) p;
      return new SequenceEncoder(d.myGetState(), '\t').append(nestedState(d.getInner())).getValue();
    }
    return p.getState();
  }

  private static long backslashes(String s) {
    return s.chars().filter(c -> c == '\\').count();
  }

  private static int topLevelTokens(String s) {
    int n = 0;
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
    while (st.hasMoreTokens()) {
      st.nextToken();
      ++n;
    }
    return n;
  }

  @Test
  public void flatFramingHasOneTokenPerTraitAndNoEscapeGrowth() {
    final int n = 300;
    final GamePiece p = chain(n);
    final String type = p.getType();
    final String state = p.getState();

    assertEquals(n + 1, topLevelTokens(type));
    assertEquals(n + 1, topLevelTokens(state));
    assertEquals(0, backslashes(type));
    assertEquals(0, backslashes(state));

    // The nested framing encoded every inner level again, so it is longer
    // however SequenceEncoder marks a delimiter inside a token. With the
    // backslash escaping, the tab after the i-th trait carried i-1
    // backslashes, n(n-1)/2 in all.
    final String nestedType = nestedType(p);
    final String nestedState = nestedState(p);
    assertTrue(nestedType.length() > type.length());
    assertTrue(nestedState.length() > state.length());
    if (backslashes(nestedType) > 0) {
      assertEquals((long) n * (n - 1) / 2, backslashes(nestedType));
      assertEquals((long) n * (n - 1) / 2, backslashes(nestedState));
    }
  }

  @Test
  public void flatFramingRoundTrips() {
    final GamePiece p = chain(5);
    final GamePiece q = new BasicCommandEncoder().createPiece(p.getType());
    q.setState(p.getState());

    assertEquals(p.getType(), q.getType());
    assertEquals(p.getState(), q.getState());
    assertEquals("value1", q.getProperty("key1")); // NON-NLS
    assertEquals("value5", q.getProperty("key5")); // NON-NLS
    assertEquals(new Point(17, 23), q.getPosition());
  }

  @Test
  public void nestedFramingStillDecodes() {
    final GamePiece p = chain(5);
    final GamePiece q = new BasicCommandEncoder().createPiece(nestedType(p));
    q.setState(nestedState(p));

    assertEquals(p.getType(), q.getType());
    assertEquals(p.getState(), q.getState());
    assertEquals("value3", q.getProperty("key3")); // NON-NLS
    assertEquals(new Point(17, 23), q.getPosition());
  }

  @Test
  public void singleTraitIsIdenticalInBothFramings() {
    final GamePiece p = chain(1);
    assertEquals(nestedType(p), p.getType());
    assertEquals(nestedState(p), p.getState());
  }

  @Test
  public void basicPieceAloneIsUnchanged() {
    final GamePiece p = chain(0);
    assertEquals(BASIC_TYPE, p.getType());
    final GamePiece q = new BasicCommandEncoder().createPiece(p.getType());
    q.setState(p.getState());
    assertEquals(p.getState(), q.getState());
    assertEquals(new Point(17, 23), q.getPosition());
  }

  @Test
  public void delimitersInsideSegmentsAreEscapedOnce() {
    final BasicPiece basic = new BasicPiece(BasicPiece.ID + ";;;Name\twith\ttabs"); // NON-NLS
    final GamePiece p = marker("a", "x\ty", marker("b", "\\starts with backslash", marker("c", "'quoted'", basic))); // NON-NLS
    final GamePiece q = new BasicCommandEncoder().createPiece(p.getType());
    q.setState(p.getState());

    assertEquals(p.getType(), q.getType());
    assertEquals(p.getState(), q.getState());
    assertEquals("x\ty", q.getProperty("a")); // NON-NLS
    assertEquals("\\starts with backslash", q.getProperty("b")); // NON-NLS
    assertEquals("'quoted'", q.getProperty("c")); // NON-NLS
    assertEquals("Name\twith\ttabs", q.getProperty(BasicPiece.BASIC_NAME)); // NON-NLS

    // and the same content written the old way decodes to the same piece
    final GamePiece r = new BasicCommandEncoder().createPiece(nestedType(p));
    r.setState(nestedState(p));
    assertEquals(p.getType(), r.getType());
    assertEquals(p.getState(), r.getState());
  }

  @Test
  public void mergeStateAppliesChangesInEitherFraming() {
    final GamePiece p = chain(3);
    final String before = p.getState();
    final String nestedBefore = nestedState(p);

    ((Marker) ((Decorator) p).getInner()).values[0] = "changed"; // NON-NLS
    Decorator.getInnermost(p).setPosition(new Point(1, 2));
    final String after = p.getState();
    final String nestedAfter = nestedState(p);
    assertNotEquals(before, after);

    final GamePiece q = new BasicCommandEncoder().createPiece(p.getType());
    q.setState(before);
    ((StateMergeable) q).mergeState(after, before);
    assertEquals(after, q.getState());
    assertEquals("changed", q.getProperty("key2")); // NON-NLS
    assertEquals(new Point(1, 2), q.getPosition());

    final GamePiece r = new BasicCommandEncoder().createPiece(p.getType());
    r.setState(nestedBefore);
    ((StateMergeable) r).mergeState(nestedAfter, nestedBefore);
    assertEquals(after, r.getState());
    assertEquals(new Point(1, 2), r.getPosition());
  }

  /**
   * A trait that frames the chain itself, the way custom traits copied
   * from an old Decorator do.
   */
  public static class LegacyMarker extends Marker {
    public static final String ID = "legacymark;"; // NON-NLS

    public LegacyMarker(String type, GamePiece inner) {
      super(type, inner);
    }

    @Override
    public String myGetType() {
      return ID + super.myGetType().substring(Marker.ID.length());
    }

    @Override
    public String getType() {
      return new SequenceEncoder(myGetType(), '\t').append(piece.getType()).getValue();
    }

    @Override
    public String getState() {
      return new SequenceEncoder(myGetState(), '\t').append(piece.getState()).getValue();
    }

    @Override
    public void setState(String newState) {
      final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, '\t');
      mySetState(st.nextToken());
      piece.setState(st.nextToken());
    }

    @Override
    public void mergeState(String newState, String oldState) {
      final SequenceEncoder.Decoder stNew = new SequenceEncoder.Decoder(newState, '\t');
      final String myNewState = stNew.nextToken();
      final String innerNewState = stNew.nextToken();
      final SequenceEncoder.Decoder stOld = new SequenceEncoder.Decoder(oldState, '\t');
      final String myOldState = stOld.nextToken();
      final String innerOldState = stOld.nextToken();
      if (!myOldState.equals(myNewState)) {
        mySetState(myNewState);
      }
      ((StateMergeable) piece).mergeState(innerNewState, innerOldState);
    }
  }

  /** The custom command encoder such a module would carry. */
  public static class LegacyEncoder extends BasicCommandEncoder {
    @Override
    public Decorator createDecorator(String type, GamePiece inner) {
      if (type.startsWith(LegacyMarker.ID)) {
        return new LegacyMarker(Marker.ID + type.substring(LegacyMarker.ID.length()), inner);
      }
      return super.createDecorator(type, inner);
    }
  }

  @Test
  public void legacyFramedTraitInTheChainRoundTrips() {
    // outer (flat) -> legacy -> inner (flat) -> basic
    final BasicPiece basic = new BasicPiece(BASIC_TYPE);
    basic.setPosition(new Point(5, 6));
    final GamePiece inner = marker("in1", "iv1", marker("in2", "iv2", basic)); // NON-NLS
    final LegacyMarker legacy = new LegacyMarker(Marker.ID + "leg", inner); // NON-NLS
    legacy.values = new String[] {"lv"}; // NON-NLS
    final GamePiece p = marker("out1", "ov1", marker("out2", "ov2", legacy)); // NON-NLS

    // The link into the legacy trait is nested, so the traits below it do not appear at top level
    assertEquals(3, topLevelTokens(p.getType()));
    assertEquals(3, topLevelTokens(p.getState()));

    final GamePiece q = new LegacyEncoder().createPiece(p.getType());
    q.setState(p.getState());
    assertEquals(p.getType(), q.getType());
    assertEquals(p.getState(), q.getState());
    assertEquals("ov2", q.getProperty("out2")); // NON-NLS
    assertEquals("lv", q.getProperty("leg")); // NON-NLS
    assertEquals("iv2", q.getProperty("in2")); // NON-NLS
    assertEquals(new Point(5, 6), q.getPosition());

    // mergeState through the legacy trait
    final String before = p.getState();
    basic.setPosition(new Point(7, 8));
    ((Marker) ((Decorator) inner).getInner()).values[0] = "iv2b"; // NON-NLS
    final String after = p.getState();
    ((StateMergeable) q).mergeState(after, before);
    assertEquals(after, q.getState());
    assertEquals("iv2b", q.getProperty("in2")); // NON-NLS
    assertEquals(new Point(7, 8), q.getPosition());
  }

  @Test
  public void legacyFramedTraitOutermostRoundTrips() {
    final BasicPiece basic = new BasicPiece(BASIC_TYPE);
    final GamePiece inner = marker("in1", "iv1", marker("in2", "iv2", basic)); // NON-NLS
    final LegacyMarker p = new LegacyMarker(Marker.ID + "leg", inner); // NON-NLS
    p.values = new String[] {"lv"}; // NON-NLS

    assertEquals(2, topLevelTokens(p.getType()));
    final GamePiece q = new LegacyEncoder().createPiece(p.getType());
    q.setState(p.getState());
    assertEquals(p.getType(), q.getType());
    assertEquals(p.getState(), q.getState());
    assertEquals("iv1", q.getProperty("in1")); // NON-NLS
  }

  @Test
  public void splitChainTellsTheFramingsApart() {
    assertEquals(Pair.of("a", null), Decorator.splitChain("a"));
    assertEquals(Pair.of("a", "b"), Decorator.splitChain("a\tb"));
    assertEquals(Pair.of("a", ""), Decorator.splitChain("a\t"));
    assertEquals(Pair.of("", "b"), Decorator.splitChain("\tb"));
    // nested: the rest is one escaped token
    assertEquals(Pair.of("a", "b\tc"), Decorator.splitChain("a\tb\\\tc"));
    assertEquals(Pair.of("a", "b\tc\\\td"), Decorator.splitChain("a\tb\\\tc\\\\\td"));
    // flat: the rest is handed on as it is
    assertEquals(Pair.of("a", "b\tc"), Decorator.splitChain("a\tb\tc"));
    assertEquals(Pair.of("a", "b\tc\\\td"), Decorator.splitChain("a\tb\tc\\\td"));
    // own segment escaping is honoured
    assertEquals(Pair.of("a\tb", "c\td"), Decorator.splitChain("a\\\tb\tc\td"));
    assertEquals(Pair.of("\\x", "c"), Decorator.splitChain("'\\x'\tc"));
    assertNull(Decorator.splitChain("a").second);
    assertTrue(Decorator.splitChain("a\tb\tc").second.contains("\t"));
  }
}
