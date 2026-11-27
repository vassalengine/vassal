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
package VASSAL.launch;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class ModuleVersionMatcherTest {

  private static String choose(String saved, String... installed) {
    final List<String> l = Arrays.asList(installed);
    return ModuleVersionMatcher.choose(saved, l, Function.identity());
  }

  @Test
  void exactVersionIsPreferredOverNewerPatch() {
    assertEquals("1.0.2", choose("1.0.2", "1.0.5", "1.0.2", "1.0.3"));
  }

  @Test
  void newestNewerPatchInSameLineIsChosen() {
    assertEquals("1.0.5", choose("1.0.2", "1.0.3", "1.0.5", "1.0.4"));
  }

  @Test
  void olderVersionIsRefused() {
    assertNull(choose("1.0.2", "1.0.1", "1.0"));
  }

  @Test
  void minorJumpIsRefused() {
    assertNull(choose("1.0", "1.1", "1.1.1"));
  }

  @Test
  void majorJumpIsRefused() {
    assertNull(choose("1.0", "2.0"));
  }

  @Test
  void newerPatchIsChosenButNotNewerMinor() {
    assertEquals("1.0.1", choose("1.0", "1.1", "1.0.1", "0.9"));
  }

  @Test
  void sameVersionWrittenDifferentlyCountsAsExact() {
    assertEquals("1.0.0", choose("1.0", "1.0.1", "1.0.0"));
  }

  @Test
  void missingMinorIsTreatedAsZero() {
    assertEquals("2.0.3", choose("2", "2.0.3", "2.1"));
  }

  @Test
  void prefixMustMatch() {
    assertEquals("v1.0.3", choose("v1.0.1", "1.0.4", "v1.0.3"));
    assertNull(choose("Beta 1.0", "1.0.1"));
  }

  @Test
  void multiDigitNumbersAreComparedNumerically() {
    assertEquals("1.0.10", choose("1.0.9", "1.0.10"));
    assertNull(choose("1.2", "1.10"));
  }

  @Test
  void versionWithoutNumbersMatchesOnlyItself() {
    assertEquals("beta", choose("beta", "1.0", "beta"));
    assertNull(choose("beta", "gamma"));
  }

  @Test
  void emptyOrMissingSavedVersionMatchesOnlyEmpty() {
    assertNull(choose("", "1.0"));
    assertNull(choose(null, "1.0"));
  }

  @Test
  void unknownCandidateVersionIsSkipped() {
    final List<String> l = Arrays.asList(null, "1.0.1");
    assertEquals("1.0.1", ModuleVersionMatcher.choose("1.0", l, Function.identity()));
  }

  @Test
  void nothingInstalled() {
    assertNull(choose("1.0"));
  }

  @Test
  void fullVersionAppendsNumericExtras() {
    assertEquals("1.2.3.4", ModuleVersionMatcher.fullVersion("1.2", "3", "4"));
    assertEquals("1.2.3", ModuleVersionMatcher.fullVersion("1.2", "3", "x"));
    assertEquals("1.2", ModuleVersionMatcher.fullVersion("1.2", "x", "4"));
    assertEquals("1.2", ModuleVersionMatcher.fullVersion("1.2", "", ""));
    assertEquals("", ModuleVersionMatcher.fullVersion(null));
  }
}
