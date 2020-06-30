/*
 *
 * Copyright (c) 2010 by Joel Uckelman
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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.junit.Test;

import static org.junit.Assert.*;

public class TailerTest {

  private static final String EXISTS = "src/test/resources/TailerTest.txt";
  private static final String NOTEXISTS = "src/test/resources/notexists";

  @Test
  public void testGetFile() {
    final File file = new File(EXISTS);
    final Tailer tailer = new Tailer(file);
    assertEquals(file, tailer.getFile());
  }

  @Test
  public void testIsTailingTrue() throws IOException {
    final File file = new File(EXISTS);
    final Tailer tailer = new Tailer(file);
    tailer.start();
    assertTrue(tailer.isTailing());
    tailer.stop();
  }

  @Test
  public void testIsTailingFalse() {
    final File file = new File(EXISTS);
    final Tailer tailer = new Tailer(file);
    assertFalse(tailer.isTailing());
  }

  @Test(expected=IOException.class)
  public void testNoFile() throws IOException {
    final File file = new File(NOTEXISTS);
    final Tailer tailer = new Tailer(file);
    tailer.start();
  }

  @Test(expected=IOException.class)
  public void testDirectory() throws IOException {
    final File file = new File(".");
    final Tailer tailer = new Tailer(file);
    tailer.start();
  }

  @Test
  public void testTailer() throws InterruptedException, IOException {
    final File file = new File(EXISTS);

    final StringBuilder sb_tailer = new StringBuilder();

    final Tailer tailer = new Tailer(file);
    tailer.addEventListener((src, str) -> sb_tailer.append(str));
    tailer.start();

    // give the Tailer time to work
    Thread.sleep(1000L);

    tailer.stop();

    final String actual = sb_tailer.toString();

    String expected;
    try (FileInputStream in = new FileInputStream(file)) {
      expected = IOUtils.toString(in, Charset.defaultCharset()).substring(0, actual.length());
    }

    // compare whatever the Tailer had time to read
    assertEquals(expected, actual);
  }
}
