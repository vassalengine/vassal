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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.tools.version.VersionUtils;

/**
 * Chooses which installed version of a module should open a saved game or
 * log, given the module version recorded in the save.
 *
 * <ol>
 *   <li>The exact version recorded in the save, if it is installed.</li>
 *   <li>Otherwise the newest installed version which is newer than the
 *       recorded one but in the same major.minor line, so a save made with
 *       1.0.2 may open with 1.0.5, but never with 1.0.1, 1.1 or 2.0.</li>
 *   <li>Otherwise nothing: the caller should refuse to open the save.</li>
 * </ol>
 *
 * <p>Module versions are free text. A version is split into a leading
 * non-numeric part (such as "v" or "Beta "), which must match exactly, and
 * the dotted numbers which follow it. A recorded version with no numbers in
 * it matches only itself.</p>
 */
public final class ModuleVersionMatcher {
  private static final Pattern VERSION =
    Pattern.compile("^(\\D*)(\\d+)(?:\\.(\\d+))?"); //NON-NLS

  private ModuleVersionMatcher() {}

  /**
   * The version string of a module as the Module Manager shows it: the
   * version, followed by Other 1 and Other 2 when those start with a digit.
   *
   * @param md module metadata, from the module itself or from the copy kept
   *           in a saved game
   * @return the full version, or "" if {@code md} is null
   */
  public static String fullVersion(AbstractMetaData md) {
    if (md == null) {
      return "";
    }
    return fullVersion(md.getVersion(), md.getExtra1(), md.getExtra2());
  }

  static String fullVersion(String version, String extra1, String extra2) {
    final String v = version == null ? "" : version;
    if (startsWithDigit(extra1)) {
      if (startsWithDigit(extra2)) {
        return v + "." + extra1 + "." + extra2;
      }
      return v + "." + extra1;
    }
    return v;
  }

  private static boolean startsWithDigit(String s) {
    return s != null && !s.isEmpty() && s.charAt(0) >= '0' && s.charAt(0) <= '9';
  }

  /**
   * @param saved      the module version recorded in the save
   * @param candidates installed modules with the save's module name
   * @param versionOf  gets the version of a candidate
   * @return the candidate to open the save with, or null if none is
   *         acceptable
   */
  public static <T> T choose(String saved,
                             Collection<T> candidates,
                             Function<T, String> versionOf) {
    final String want = saved == null ? "" : saved.trim();

    T same = null;
    final List<T> newer = new ArrayList<>();
    for (final T c : candidates) {
      final String have = versionOf.apply(c);
      if (have == null) {
        continue;
      }
      if (have.trim().equals(want)) {
        return c;
      }
      switch (comparePatch(want, have.trim())) {
      case 0:
        // the same version written differently, such as 1.0 and 1.0.0
        if (same == null) {
          same = c;
        }
        break;
      case 1:
        newer.add(c);
        break;
      default:
        break;
      }
    }

    if (same != null) {
      return same;
    }

    T best = null;
    for (final T c : newer) {
      if (best == null ||
          VersionUtils.compareVersions(versionOf.apply(c).trim(),
                                       versionOf.apply(best).trim()) > 0) {
        best = c;
      }
    }
    return best;
  }

  /**
   * @return 1 if {@code have} is newer than {@code want} in the same prefix,
   *         major and minor line, 0 if it is the same version written
   *         differently, and -1 if it is older or in another line
   */
  static int comparePatch(String want, String have) {
    final Matcher w = VERSION.matcher(want);
    final Matcher h = VERSION.matcher(have);
    if (!w.find() || !h.find()) {
      return -1;
    }

    if (!w.group(1).equals(h.group(1)) ||
        number(w.group(2)) != number(h.group(2)) ||
        number(w.group(3)) != number(h.group(3))) {
      return -1;
    }

    final int c = VersionUtils.compareVersions(have, want);
    return c > 0 ? 1 : (c == 0 ? 0 : -1);
  }

  private static long number(String s) {
    if (s == null) {
      return 0;
    }
    try {
      return Long.parseLong(s);
    }
    catch (NumberFormatException e) {
      return -1;
    }
  }
}
