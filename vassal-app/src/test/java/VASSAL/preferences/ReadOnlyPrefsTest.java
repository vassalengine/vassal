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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;

import VASSAL.tools.ReadErrorDialog;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

public class ReadOnlyPrefsTest {
  @TempDir
  Path dir;

  @Test
  public void valuesAreReadFromAWellFormedFile() throws IOException {
    final File f = dir.resolve("V_Global").toFile(); //NON-NLS
    Files.writeString(f.toPath(), "maximumHeap=1024\n", StandardCharsets.ISO_8859_1); //NON-NLS

    try (MockedStatic<ReadErrorDialog> dialog = mockStatic(ReadErrorDialog.class)) {
      final ReadOnlyPrefs p = new ReadOnlyPrefs(f);
      assertThat(p.getStoredValue("maximumHeap"), is("1024")); //NON-NLS
      dialog.verifyNoInteractions();
    }
  }

  @Test
  public void aMissingFileIsNotAnError() {
    final File f = dir.resolve("V_Global").toFile(); //NON-NLS

    try (MockedStatic<ReadErrorDialog> dialog = mockStatic(ReadErrorDialog.class)) {
      final ReadOnlyPrefs p = new ReadOnlyPrefs(f);
      assertThat(p.getStoredValue("maximumHeap"), is(nullValue())); //NON-NLS
      dialog.verifyNoInteractions();
    }
  }

  /**
   * Properties.load() throws IllegalArgumentException, not IOException, for
   * a file with a bad \\uxxxx escape. That must be reported as a read error
   * like any other unreadable prefs file, not escape to the caller.
   */
  @Test
  public void aCorruptFileIsReportedAsAReadError() throws IOException {
    final File f = dir.resolve("V_Global").toFile(); //NON-NLS
    Files.writeString(f.toPath(), "maximumHeap=1024\nbad=\\uZZZZ\n", StandardCharsets.ISO_8859_1); //NON-NLS

    try (MockedStatic<ReadErrorDialog> dialog = mockStatic(ReadErrorDialog.class)) {
      new ReadOnlyPrefs(f);
      dialog.verify(() -> ReadErrorDialog.error(any(IOException.class), eq(f)));
    }
  }
}
