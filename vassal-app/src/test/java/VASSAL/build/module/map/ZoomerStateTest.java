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
package VASSAL.build.module.map;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * {@link Zoomer.State} guards against a zoom factor its caller should not have passed.
 * The guard covered zero and negative factors but not infinite or NaN ones, which is how
 * an infinite factor reached every later repaint: {@code (int) (width * Infinity)} is
 * {@code Integer.MAX_VALUE}, and {@code ScaleOpBitmapImpl.fixSize()} then refuses the image
 * with "Dimensions (width=2147483647 height=2147483647) are too large".
 */
public class ZoomerStateTest {

  private static final double[] LEVELS = {0.5, 1.0, 2.0};
  private static final int INITIAL = 1;   // 1.0

  private static Zoomer.State state() {
    return new Zoomer.State(LEVELS.clone(), INITIAL);
  }

  @Test
  public void anInfiniteZoomIsNotStored() {
    final Zoomer.State s = state();
    s.setZoom(Double.POSITIVE_INFINITY);
    assertTrue(Double.isFinite(s.getZoom()), "zoom was " + s.getZoom());
    assertTrue(s.getZoom() > 0.0, "zoom was " + s.getZoom());
  }

  @Test
  public void aNaNZoomIsNotStored() {
    final Zoomer.State s = state();
    s.setZoom(Double.NaN);
    assertTrue(Double.isFinite(s.getZoom()), "zoom was " + s.getZoom());
    assertTrue(s.getZoom() > 0.0, "zoom was " + s.getZoom());
  }

  @Test
  public void anInfiniteZoomLeavesTheLevelInRange() {
    // Arrays.binarySearch(levels, Infinity) reports the insertion point past the end,
    // so the level index ran off the end of the level array as well.
    final Zoomer.State s = state();
    s.setZoom(Double.POSITIVE_INFINITY);
    assertTrue(s.getLevel() < s.getLevelCount(),
               "level " + s.getLevel() + " of " + s.getLevelCount());
    assertTrue(s.getLevel() >= 0, "level " + s.getLevel());
  }

  @Test
  public void anInfiniteZoomLeavesTheExistingZoomAlone() {
    final Zoomer.State s = state();
    s.setZoom(2.0);
    final double before = s.getZoom();
    s.setZoom(Double.POSITIVE_INFINITY);
    assertEquals(before, s.getZoom(), 0.0);
  }

  @Test
  public void aValidZoomIsStillStored() {
    final Zoomer.State s = state();
    s.setZoom(2.0);
    assertEquals(2.0, s.getZoom(), 0.0);
    s.setZoom(1.5);
    assertEquals(1.5, s.getZoom(), 0.0);
  }
}
