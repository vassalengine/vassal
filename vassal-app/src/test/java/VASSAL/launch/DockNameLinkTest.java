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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class DockNameLinkTest {

  @Test
  public void fileNameKeepsOrdinaryNames() {
    assertEquals("Commands & Colors: Napoleonics".replace(':', '-'),
      DockNameLink.fileName("Commands & Colors: Napoleonics"));
    assertEquals("Here I Stand", DockNameLink.fileName("  Here I Stand "));
  }

  @Test
  public void fileNameIsMadeSafe() {
    assertEquals("A-B-C", DockNameLink.fileName("A/B\\C"));
    assertEquals("hidden", DockNameLink.fileName("..hidden"));
    assertEquals("ab", DockNameLink.fileName("a\nb"));
    assertEquals("VASSAL", DockNameLink.fileName(null));
    assertEquals("VASSAL", DockNameLink.fileName("  "));
    assertTrue(DockNameLink.fileName("x".repeat(300)).length() <= 80);
  }

  @Test
  public void linkIsMadeAndRemoved(@TempDir Path tmp) throws Exception {
    final Path java = Files.createFile(tmp.resolve("java"));
    final Path link = DockNameLink.create(tmp, "Paths of Glory", java);
    assertNotNull(link);
    assertEquals("Paths of Glory", link.getFileName().toString());
    assertTrue(Files.isSymbolicLink(link));
    assertEquals(java.toAbsolutePath(), Files.readSymbolicLink(link));

    DockNameLink.delete(link);
    assertFalse(Files.exists(link, LinkOption.NOFOLLOW_LINKS));
    assertFalse(Files.exists(link.getParent()));
    assertTrue(Files.exists(java));

    // tolerates null
    DockNameLink.delete(null);
  }
}
