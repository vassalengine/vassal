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

import java.io.InputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class DeprecationWriter {

  private static final byte[] ZIPSIG = { 0x50, 0x4B, 0x03, 0x04 };

  private static boolean isZipArchive(Path p) {
    try (InputStream in = Files.newInputStream(p)) {
      return Arrays.equals(in.readNBytes(4), ZIPSIG);
    }             
    catch (IOException e) {
      return false;
    }
  }

  public static void main(String[] args) throws IOException {
    final DeprecationWalker d = new DeprecationWalker();

    if (args.length != 1 && args.length != 2) {
      throw new IllegalArgumentException();
    }

    try (PrintStream ps = args.length == 1 ? System.out : new PrintStream(args[1], StandardCharsets.UTF_8)) {
      d.setCallback((n, s, r) -> ps.println(n + "\t" + s + "\t" + r));

      // recursively walk the first arg, looking for class files
      try (Stream<Path> s = Files.walk(Paths.get(args[0]))
                                 .filter(Files::isRegularFile)) {
        for (final Path p: (Iterable<Path>)s::iterator) {
          if (p.getFileName().toString().endsWith(".class")) {
            d.setInput(Files.readAllBytes(p));
            d.walk();
          }
          else if (isZipArchive(p)) {
            try (ZipFile zf = new ZipFile(p.toFile())) {
              final Enumeration<? extends ZipEntry> entries = zf.entries();
              while (entries.hasMoreElements()) {
                final ZipEntry ze = entries.nextElement();
                if (ze.getName().endsWith(".class")) {
                  try (InputStream in = zf.getInputStream(ze)) { 
                    d.setInput(in);
                    d.walk();
                  }
                }
              }
            }
          }
        }
      }
    }
  }
} 
