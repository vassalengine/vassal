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
package VASSAL.build.module.metadata;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.is;

import VASSAL.build.GameModule;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class MetaDataFactoryTest {
  @TempDir
  Path dir;

  private File archiveWithBuildFile(String buildFile) throws IOException {
    final File f = dir.resolve("odd.vmod").toFile(); //NON-NLS
    try (ZipOutputStream zout = new ZipOutputStream(Files.newOutputStream(f.toPath()))) {
      zout.putNextEntry(new ZipEntry(GameModule.BUILDFILE));
      zout.write(buildFile.getBytes(StandardCharsets.UTF_8));
      zout.closeEntry();
    }
    return f;
  }

  /**
   * A build file with no metadata is sniffed for the module or extension
   * element in its first ten lines. One shorter than that, which names
   * neither, is not a VASSAL file; it used to be a NullPointerException.
   */
  @Test
  public void aShortBuildFileWithoutAModuleElementIsNotAVassalFile() throws IOException {
    final File f = archiveWithBuildFile("<?xml version=\"1.0\"?>\n<Something/>\n"); //NON-NLS

    assertThat(MetaDataFactory.buildMetaData(f), is(nullValue()));
  }
}
