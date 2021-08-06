/*
 * Copyright (c) 2021 by Joel Uckelman
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

package VASSAL.tools.deprecation;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class Processor {

  private static final byte[] ZIPSIG = { 0x50, 0x4B, 0x03, 0x04 };

  private static boolean isZipArchive(Path p) {
    try (InputStream in = Files.newInputStream(p)) {
      return Arrays.equals(in.readNBytes(4), ZIPSIG);
    }             
    catch (IOException e) {
      return false;
    }
  }

  public static void process(Walker walker, byte[] b) {
    walker.setInput(b);
    walker.walk();
  }

  public static void process(Walker walker, InputStream in) throws IOException {
    walker.setInput(in);
    walker.walk();
  }

  public static void process(Walker walker, ZipFile zf) throws IOException {
    final Enumeration<? extends ZipEntry> entries = zf.entries();
    while (entries.hasMoreElements()) {
      final ZipEntry ze = entries.nextElement();
      if (ze.getName().endsWith(".class")) {
        try (InputStream in = zf.getInputStream(ze)) {
          process(walker, in);
        }
      }
    }
  }

  public static void process(Walker walker, String src) throws IOException {
    // recursively walk the first arg, looking for class files
    try (Stream<Path> s = Files.walk(Paths.get(src))
                               .filter(Files::isRegularFile)) {
      for (final Path p: (Iterable<Path>)s::iterator) {
        if (p.getFileName().toString().endsWith(".class")) {
          try (InputStream in = Files.newInputStream(p)) {
            process(walker, in);
          }
        }
        else if (isZipArchive(p)) {
          try (ZipFile zf = new ZipFile(p.toFile())) {
            process(walker, zf);
          }
        }
      }
    }
  }

  public static void readCompSet(InputStream in, Consumer<String[]> callback) throws IOException {
    try (InputStreamReader isr = new InputStreamReader(in, StandardCharsets.UTF_8)) {
      try (BufferedReader br = new BufferedReader(isr)) {
        String line;
        while ((line = br.readLine()) != null) {
          if (!line.isEmpty()) {
            callback.accept(line.split("\t"));
          }
        }
      }
    }
  }
}
