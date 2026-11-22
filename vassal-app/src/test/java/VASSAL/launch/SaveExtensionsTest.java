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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import VASSAL.build.module.GameState;
import VASSAL.tools.SequenceEncoder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SaveExtensionsTest {
  private static final char ESC = 27;

  private static String ext(String name, String version) {
    return "EXT\t" + new SequenceEncoder('\t').append(name).append(version).getValue();
  }

  /** Joins commands the way GameModule.encode does. */
  private static String join(String first, String... rest) {
    final SequenceEncoder se = new SequenceEncoder(first, ESC);
    for (final String r : rest) {
      se.append(r);
    }
    return se.getValue();
  }

  private static String names(List<SaveExtensions.Extension> l) {
    final StringBuilder sb = new StringBuilder();
    for (final SaveExtensions.Extension e : l) {
      if (sb.length() > 0) sb.append(", ");
      sb.append(e);
    }
    return sb.toString();
  }

  @Test
  void findsTopLevelAndNestedRegistrations() {
    final String nested = join("begin_save", ext("Maps", "1.2"), "+/piece/stuff\twith\ttabs");
    final String save = join(nested, ext("Variant B", "2.0"), "end_save");
    assertEquals("Maps 1.2, Variant B 2.0", names(SaveExtensions.parse(save)));
  }

  @Test
  void repeatsAreDropped() {
    final String save = join("begin_save", ext("Maps", "1.2"), ext("Maps", "1.2"));
    assertEquals("Maps 1.2", names(SaveExtensions.parse(save)));
  }

  @Test
  void noRegistrations() {
    assertTrue(SaveExtensions.parse(join("begin_save", "end_save")).isEmpty());
    assertTrue(SaveExtensions.parse("").isEmpty());
    assertTrue(SaveExtensions.parse(null).isEmpty());
  }

  @Test
  void readsSaveFile(@TempDir Path dir) throws IOException {
    final File f = dir.resolve("game.vsav").toFile();
    try (ZipOutputStream zout = new ZipOutputStream(Files.newOutputStream(f.toPath()))) {
      zout.putNextEntry(new ZipEntry(GameState.SAVEFILE_ZIP_ENTRY));
      final OutputStream out = GameState.compressSavedGame(zout);
      out.write(join("begin_save", ext("Maps", "1.2"), "end_save").getBytes(StandardCharsets.UTF_8));
      out.close();
    }
    assertEquals("Maps 1.2", names(SaveExtensions.read(f)));
  }

  @Test
  void compareReportsMissingInactiveAndOlder() {
    final List<SaveExtensions.Extension> needed = List.of(
      new SaveExtensions.Extension("A", "1.0"),
      new SaveExtensions.Extension("B", "1.0"),
      new SaveExtensions.Extension("C", "2.1"),
      new SaveExtensions.Extension("D", "1.0"),
      new SaveExtensions.Extension("E", "1.0"));
    final List<SaveExtensions.Extension> active = List.of(
      new SaveExtensions.Extension("C", "2.0"),
      new SaveExtensions.Extension("D", "1.0"),
      new SaveExtensions.Extension("E", "1.1"));
    final List<SaveExtensions.Extension> installed = List.of(
      new SaveExtensions.Extension("B", "1.0"),
      new SaveExtensions.Extension("C", "2.0"),
      new SaveExtensions.Extension("D", "1.0"),
      new SaveExtensions.Extension("E", "1.1"));

    final List<SaveExtensions.Shortfall> s =
      SaveExtensions.compare(needed, active, installed);
    assertEquals(3, s.size());
    assertEquals("A", s.get(0).needed.name);
    assertEquals(SaveExtensions.Problem.MISSING, s.get(0).problem);
    assertEquals("B", s.get(1).needed.name);
    assertEquals(SaveExtensions.Problem.INACTIVE, s.get(1).problem);
    assertEquals("C", s.get(2).needed.name);
    assertEquals(SaveExtensions.Problem.OLDER, s.get(2).problem);
    assertEquals("2.0", s.get(2).installedVersion);
  }

  @Test
  void nameOfDropsFileExtension() {
    assertEquals("Maps v2", SaveExtensions.nameOf(new File("/x/Maps v2.vmdx")));
    assertEquals("noext", SaveExtensions.nameOf(new File("noext")));
  }
}
