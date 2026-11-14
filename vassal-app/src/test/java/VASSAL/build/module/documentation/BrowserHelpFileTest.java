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

package VASSAL.build.module.documentation;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class BrowserHelpFileTest {

  /**
   * Builds an archive the way BrowserHelpFile.packContents does: an entry per
   * file, with the sub-folder in the entry's name, and no directory entries.
   */
  private static byte[] helpArchive(Map<String, String> entries) throws IOException {
    final ByteArrayOutputStream bos = new ByteArrayOutputStream();
    try (ZipOutputStream out = new ZipOutputStream(bos)) {
      for (final Map.Entry<String, String> e : entries.entrySet()) {
        out.putNextEntry(new ZipEntry(e.getKey()));
        out.write(e.getValue().getBytes(StandardCharsets.UTF_8));
        out.closeEntry();
      }
    }
    return bos.toByteArray();
  }

  /**
   * An HTML Help File folder holding a sub-folder must unpack whole: the
   * archive carries no directory entries, so a file is the only sign that its
   * sub-folder exists.
   *
   * @see <a href="https://github.com/vassalengine/vassal/issues/15234">#15234</a>
   */
  @Test
  public void extractsFilesInSubFolders(@TempDir File dir) throws IOException {
    final Map<String, String> entries = new LinkedHashMap<>();
    entries.put("help.html", "<html>help</html>"); //NON-NLS
    entries.put("images/changePawn.png", "not really a png"); //NON-NLS
    entries.put("style.css", "body { }"); //NON-NLS

    final Path target = new File(dir, "How_to_use_this_module").toPath(); //NON-NLS
    BrowserHelpFile.extractTo(
      new ZipInputStream(new ByteArrayInputStream(helpArchive(entries))), target);

    for (final Map.Entry<String, String> e : entries.entrySet()) {
      final Path f = target.resolve(e.getKey());
      assertTrue(Files.exists(f), e.getKey() + " should have been extracted"); //NON-NLS
      assertArrayEquals(e.getValue().getBytes(StandardCharsets.UTF_8), Files.readAllBytes(f),
        e.getKey() + " should have been extracted intact"); //NON-NLS
    }
  }

  /** An entry name must not be able to write outside the help directory. */
  @Test
  public void refusesEntriesOutsideTheHelpDirectory(@TempDir File dir) throws IOException {
    final Map<String, String> entries = new LinkedHashMap<>();
    entries.put("../escaped.txt", "should not be written"); //NON-NLS

    final Path target = new File(dir, "help").toPath(); //NON-NLS
    assertThrows(IOException.class, () -> BrowserHelpFile.extractTo(
      new ZipInputStream(new ByteArrayInputStream(helpArchive(entries))), target));

    assertFalse(Files.exists(new File(dir, "escaped.txt").toPath()), //NON-NLS
      "nothing should have been written outside the help directory"); //NON-NLS
  }
}
