/*
 * Copyright (c) 2020 by Joel Uckelman
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

package VASSAL.tools.io;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import static org.junit.jupiter.api.Assertions.*;

import org.apache.commons.io.IOUtils;

public class ZipArchiveTest {
  private static final String OUT_DIR = "target/test-classes";

  private String name;

  @BeforeEach
  public void beforeEach(TestInfo info) {
    name = info.getTestMethod().toString();
  }

  @AfterEach
  public void cleanup() throws IOException {
    Files.deleteIfExists(testArchivePath());
    Files.deleteIfExists(Path.of(testArchivePath().toString() + ".bak"));
  }

  private Path testArchivePath() {
    return Path.of(
      OUT_DIR,
      getClass().getSimpleName() + "_" + name + ".zip"
    );
  }

  @Test
  public void testGetName() throws IOException {
    final Path p = testArchivePath();
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      assertEquals(p.toString(), z.getName());
    }
  }

  @Test
  public void testGetFile() throws IOException {
    final Path p = testArchivePath();
    try (ZipArchive z =  new ZipArchive(p.toFile())) {
      assertEquals(p.toFile(), z.getFile());
    }
  }

  @Test
  public void testUnmodifiedAfterOpening() throws IOException {
    final Path p = testArchivePath();
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      assertFalse(z.isModified());
    }
  }

  private void assertNoFile(String name, ZipArchive z) throws IOException {
    assertFalse(z.contains(name));
    assertFalse(z.getFiles().contains(name));

    try {
      z.getSize(name);
      fail("Expected FileNotFoundException");
    }
    catch (FileNotFoundException | NoSuchFileException e) {
      // expected
    }

    try {
      z.getMTime(name);
      fail("Expected FileNotFoundException");
    }
    catch (FileNotFoundException | NoSuchFileException e) {
      // expected
    }

    try (InputStream in = z.getInputStream(name)) {
      fail("Expected FileNotFoundException");
    }
    catch (FileNotFoundException | NoSuchFileException e) {
      // expected
    }
  }

  private void assertFile(String name, byte[] data, ZipArchive z) throws IOException {
    assertTrue(z.contains(name));
    assertTrue(z.getFiles().contains(name));
    assertEquals(data.length, z.getSize(name));
    assertTrue(z.getMTime(name) > 0);
    try (InputStream in = z.getInputStream(name)) {
      assertArrayEquals(data, IOUtils.toByteArray(in));
    }
  }

  private void assertFile(String name, Path extPath, ZipArchive z) throws IOException {
    assertTrue(z.contains(name));
    assertTrue(z.getFiles().contains(name));
    assertEquals(Files.size(extPath), z.getSize(name));
    assertTrue(z.getMTime(name) > 0);
    try (InputStream act_in = z.getInputStream(name);
         InputStream exp_in = Files.newInputStream(extPath)) {
      assertTrue(IOUtils.contentEquals(exp_in, act_in));
    }
  }

  @Test
  public void testAddRemoveBytes() throws IOException {
    final String name = "bytes!";
    final byte[] data = new byte[]{ 0, 42, 11 };

    final Path p = testArchivePath();

    // add a file
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      // file isn't there
      assertFalse(z.isModified());
      assertNoFile(name, z);

      z.add(name, data);

      // file is there now
      assertTrue(z.isModified());
      assertFile(name, data, z);
    }

    try (ZipArchive z = new ZipArchive(p.toFile())) {
      // reopen, check the file was written correctly
      assertFalse(z.isModified());
      assertFile(name, data, z);

      // remove the file
      z.remove(name);

      // check that the file is gone
      assertTrue(z.isModified());
      assertNoFile(name, z);
    }

    // reopen, check that the removed file is gone
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      assertFalse(z.isModified());
      assertNoFile(name, z);
    }
  }

  @Test
  public void testAddRemoveOutputStream() throws IOException {
    final String name = "bytes!";
    final byte[] data = new byte[]{ 0, 42, 11 };

    final Path p = testArchivePath();

    // add a file
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      // file isn't there
      assertFalse(z.isModified());
      assertNoFile(name, z);

      try (OutputStream out = z.getOutputStream(name)) {
        out.write(data);
      }

      // file is there now
      assertTrue(z.isModified());
      assertFile(name, data, z);
    }

    try (ZipArchive z = new ZipArchive(p.toFile())) {
      // reopen, check the file was written correctly
      assertFalse(z.isModified());
      assertFile(name, data, z);

      // remove the file
      z.remove(name);

      // check that the file is gone
      assertTrue(z.isModified());
      assertNoFile(name, z);
    }

    // reopen, check that the removed file is gone
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      assertFalse(z.isModified());
      assertNoFile(name, z);
    }
  }

  @Test
  public void testAddRemoveFile() throws IOException {
    final String name = "pom.xml";
    final Path extPath = Path.of(name);
    final Path p = testArchivePath();

    // add a file
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      // file isn't there
      assertFalse(z.isModified());
      assertNoFile(name, z);

      z.add(name, extPath.toFile());

      // file is there now
      assertTrue(z.isModified());
      assertFile(name, extPath, z);
    }

    try (ZipArchive z = new ZipArchive(p.toFile())) {
      // reopen, check the file was written correctly
      assertFalse(z.isModified());
      assertFile(name, extPath, z);

      // remove the file
      z.remove(name);

      // check that the file is gone
      assertTrue(z.isModified());
      assertNoFile(name, z);
    }

    // reopen, check that the removed file is gone
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      assertFalse(z.isModified());
      assertNoFile(name, z);
    }
  }

  @Test
  public void testGetFilesSubdir() throws IOException {
    final byte[] data = new byte[0];
    final Path p = testArchivePath();

    final Set<String> files = Set.of("1", "a/b/c", "a/d");

    try (ZipArchive z = new ZipArchive(p.toFile())) {
      // add some files
      z.add("1", data);
      z.add("a/b/c", data);
      z.add("a/d", data);

      // check the file lists
      assertEquals(files, Set.copyOf(z.getFiles()));
      assertEquals(files, Set.copyOf(z.getFiles("")));
      assertEquals(Set.of("a/b/c", "a/d"), Set.copyOf(z.getFiles("a")));
      assertEquals(Set.of("a/b/c"), Set.copyOf(z.getFiles("a/b")));

      // directories, not files
      assertFalse(z.contains("a"));
      assertFalse(z.contains("a/b"));
    }
  }

  @Test
  public void testIsClosedAfterClose() throws IOException {
    final Path p = testArchivePath();
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      z.close();
      assertTrue(z.isClosed());
    }
  }

  @Test
  public void testCloseIsIdempotent() throws IOException {
    final Path p = testArchivePath();
    try (ZipArchive z = new ZipArchive(p.toFile())) {
      z.close();
      assertTrue(z.isClosed());
      z.close();
      assertTrue(z.isClosed());
      z.close();
      assertTrue(z.isClosed());
    }
  }

}
