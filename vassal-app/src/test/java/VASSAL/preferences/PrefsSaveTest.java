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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.preferences;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

import VASSAL.tools.ReadErrorDialog;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

/**
 * save() reads the file on disk before merging into it, so the unchecked
 * IllegalArgumentException that Properties.load() throws for a malformed file
 * escaped from there too --- and save() is declared to throw IOException, so
 * nothing on the way out was expecting it. A user whose preferences file had
 * gone bad could not change any preference until they deleted the file.
 *
 * ReadOnlyPrefsTest covers the reading half of the same problem.
 */
public class PrefsSaveTest {
  @TempDir
  Path dir;

  /** A bad \\uxxxx escape: what every reported corrupt preferences file contained. */
  private static final String MALFORMED = "maximumHeap=1024\nbad=\\uZZZZ\n"; //NON-NLS

  @Test
  public void aCorruptFileDoesNotStopTheSave() throws IOException {
    final File f = write(MALFORMED);

    try (MockedStatic<ReadErrorDialog> dialog = mockStatic(ReadErrorDialog.class)) {
      final Prefs p = new Prefs(mock(PrefsEditor.class), f);
      assertDoesNotThrow(p::save, "a corrupt file must not stop preferences being saved");
    }

    // and what is on disk now can be read
    final Properties after = new Properties();
    try (var in = Files.newInputStream(f.toPath())) {
      assertDoesNotThrow(() -> after.load(in), "the rewritten file must be readable");
    }
  }

  @Test
  public void theUnreadableFileIsKept() throws IOException {
    final File f = write(MALFORMED);

    try (MockedStatic<ReadErrorDialog> dialog = mockStatic(ReadErrorDialog.class)) {
      new Prefs(mock(PrefsEditor.class), f).save();
    }

    final Path kept = f.toPath().resolveSibling(f.getName() + ".corrupted"); //NON-NLS
    assertTrue(Files.exists(kept), "the file nobody could parse must not be silently discarded");
    assertEquals(MALFORMED, Files.readString(kept, StandardCharsets.ISO_8859_1),
      "the kept copy must be the bytes that were there, unaltered");
  }

  @Test
  public void aWellFormedFileIsNotCopiedAside() throws IOException {
    final File f = write("maximumHeap=1024\n"); //NON-NLS

    try (MockedStatic<ReadErrorDialog> dialog = mockStatic(ReadErrorDialog.class)) {
      new Prefs(mock(PrefsEditor.class), f).save();
    }

    assertFalse(Files.exists(f.toPath().resolveSibling(f.getName() + ".corrupted")), //NON-NLS
      "nothing is wrong with this file, so nothing should be kept aside");
  }

  @Test
  public void savingWithNoFileYetStillWorks() {
    final File f = dir.resolve("V_Global").toFile(); //NON-NLS

    try (MockedStatic<ReadErrorDialog> dialog = mockStatic(ReadErrorDialog.class)) {
      final Prefs p = new Prefs(mock(PrefsEditor.class), f);
      assertDoesNotThrow(p::save, "the first save of a new preferences file must work");
    }
    assertFalse(Files.exists(f.toPath().resolveSibling(f.getName() + ".corrupted"))); //NON-NLS
  }

  private File write(String content) throws IOException {
    final Path p = dir.resolve("V_Global"); //NON-NLS
    Files.writeString(p, content, StandardCharsets.ISO_8859_1);
    return p.toFile();
  }
}
