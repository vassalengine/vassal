/*
 *
 * Copyright (c) 2026 by the VASSAL Development Team
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

package VASSAL.tools;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import VASSAL.tools.bug.CustomCode;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Checks that a class which a {@code DataArchive} loaded out of a module archive is identifiable
 * as module-supplied code from the stack trace of anything it throws.
 */
public class DataArchiveTest {

  private static final String FIXTURE = ModuleCodeFixture.class.getName();

  @Test
  public void testModuleLoaderNameIsThePrefixedFileName() {
    assertEquals(
      DataArchive.MODULE_LOADER_PREFIX + "Some Module.vmod",
      DataArchive.moduleLoaderName("/home/somebody/modules/Some Module.vmod")
    );

    // Not every archive knows its file name when its loader is constructed.
    assertEquals(
      DataArchive.MODULE_LOADER_PREFIX, DataArchive.moduleLoaderName(null)
    );
  }

  @Test
  public void testAFailureInACustomClassNamesTheModuleItCameFrom(@TempDir Path dir)
      throws Exception {

    final Path vmod = dir.resolve("Great Battles.vmod");
    writeClassTo(vmod, FIXTURE);

    try (DataArchive archive = new DataArchive(vmod.toString())) {
      // findClass, not loadClass: loadClass would find the test classpath's copy first.
      final Class<?> c = archive.findClass(FIXTURE);

      final Throwable thrown = invokeAndCatch(c, "boom");

      // The frame says for itself which archive the class came out of ...
      final StackTraceElement frame = thrown.getStackTrace()[0];
      assertEquals(FIXTURE, frame.getClassName());
      assertEquals(
        DataArchive.MODULE_LOADER_PREFIX + "Great Battles.vmod",
        frame.getClassLoaderName()
      );

      // ... in the printed trace a reader sees ...
      assertTrue(
        ThrowableUtils.getStackTrace(thrown)
          .contains(DataArchive.MODULE_LOADER_PREFIX + "Great Battles.vmod//" + FIXTURE),
        "the printed trace should name the archive"
      );

      // ... and the failure is attributed to the module rather than to VASSAL.
      assertEquals(FIXTURE, CustomCode.blame(thrown));
    }
  }

  /** Writes a one-entry zip holding the compiled bytes of {@code cname}. */
  private static void writeClassTo(Path zip, String cname) throws Exception {
    final String path = cname.replace('.', '/') + ".class";
    try (InputStream in = DataArchiveTest.class.getClassLoader().getResourceAsStream(path);
         OutputStream os = Files.newOutputStream(zip);
         ZipOutputStream zos = new ZipOutputStream(os)) {
      assertNotNull(in, path + " should be on the test classpath");
      zos.putNextEntry(new ZipEntry(path));
      in.transferTo(zos);
      zos.closeEntry();
    }
  }

  private static Throwable invokeAndCatch(Class<?> c, String method) throws Exception {
    try {
      c.getMethod(method).invoke(null);
    }
    catch (InvocationTargetException e) {
      return e.getCause();
    }
    throw new AssertionError(method + " did not throw");
  }
}
