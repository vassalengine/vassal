/*
 * Copyright (c) 2026 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.io;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.SystemUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests the filename-encoding repair applied to child processes.
 *
 * @see ProcessLauncher#filenameEncodingIsUsable()
 */
public class ProcessLauncherLocaleTest {

  private static final String ENC = "sun.jnu.encoding";

  private void withEncoding(String enc, Runnable r) {
    final String old = System.getProperty(ENC);
    try {
      if (enc == null) {
        System.clearProperty(ENC);
      }
      else {
        System.setProperty(ENC, enc);
      }
      r.run();
    }
    finally {
      if (old == null) {
        System.clearProperty(ENC);
      }
      else {
        System.setProperty(ENC, old);
      }
    }
  }

  @Test
  public void asciiEncodingIsUnusable() {
    // what a C/POSIX locale, or one naming missing locale data, yields
    withEncoding("ANSI_X3.4-1968",
      () -> assertFalse(ProcessLauncher.filenameEncodingIsUsable()));
    withEncoding("US-ASCII",
      () -> assertFalse(ProcessLauncher.filenameEncodingIsUsable()));
  }

  @Test
  public void utf8EncodingIsUsable() {
    withEncoding("UTF-8",
      () -> assertTrue(ProcessLauncher.filenameEncodingIsUsable()));
  }

  @Test
  public void legacyEightBitEncodingIsLeftAlone() {
    // a legacy system round-trips its own filenames; overriding its
    // locale would be the regression rather than the fix
    withEncoding("ISO-8859-1",
      () -> assertTrue(ProcessLauncher.filenameEncodingIsUsable()));
    withEncoding("windows-1252",
      () -> assertTrue(ProcessLauncher.filenameEncodingIsUsable()));
  }

  @Test
  public void unknownOrAbsentEncodingIsLeftAlone() {
    withEncoding(null,
      () -> assertTrue(ProcessLauncher.filenameEncodingIsUsable()));
    withEncoding("no-such-charset",
      () -> assertTrue(ProcessLauncher.filenameEncodingIsUsable()));
  }

  @Test
  public void unusableEncodingSetsChildLocale() {
    withEncoding("ANSI_X3.4-1968", () -> {
      final Map<String, String> env = new HashMap<>();
      ProcessLauncher.ensureUsableLocale(env);
      if (SystemUtils.IS_OS_WINDOWS) {
        // filenames reach the OS as UTF-16; the locale does not govern them
        assertNull(env.get("LC_ALL"));
      }
      else {
        assertEquals("C.UTF-8", env.get("LC_ALL"));
      }
    });
  }

  @Test
  public void usableEncodingLeavesChildEnvironmentUntouched() {
    withEncoding("UTF-8", () -> {
      final Map<String, String> env = new HashMap<>();
      env.put("LC_ALL", "en_GB.UTF-8");
      ProcessLauncher.ensureUsableLocale(env);
      assertEquals("en_GB.UTF-8", env.get("LC_ALL"));
      assertEquals(1, env.size());
    });
  }
}
