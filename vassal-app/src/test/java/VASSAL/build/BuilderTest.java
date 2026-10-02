/*
 * Copyright (c) 2026 by Joel Uckelman
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
package VASSAL.build;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;

import VASSAL.build.Builder;

import static org.junit.jupiter.api.Assertions.*;

public class BuilderTest {
  @Test
  public void testLengthPrefixedString() throws IOException {
    // Check that the character used for delimiting the length by
    // SequenceEncoder in length-prefixed strings round-trips through XML
    // reading and writing.
    final String in_xml = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n<x>\n\uE0001\uE000b</x>\n";

    final ByteArrayInputStream in = new ByteArrayInputStream(
      in_xml.getBytes(StandardCharsets.UTF_8)
    );

    final String out_xml = Builder.toString(Builder.createDocument(in));

    assertEquals(in_xml, out_xml);
  }
}
