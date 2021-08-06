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

import java.io.BufferedInputStream;
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

  // oh seriously?! these have to be cast?
  private static final byte[] CLASSSIG = {
    (byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE
  };

  private static boolean checkSig(InputStream in, byte[] sig) {
    try {
      return Arrays.equals(in.readNBytes(sig.length), sig);
    }
    catch (IOException e) {
      return false;
    }
  }

  private static boolean isZipArchive(Path p) {
    try (InputStream in = Files.newInputStream(p)) {
      return checkSig(in, ZIPSIG);
    }
    catch (IOException e) {
      return false;
    }
  }

  private static boolean isClassFile(Path p) {
    try (InputStream in = Files.newInputStream(p)) {
      return checkSig(in, CLASSSIG);
    }
    catch (IOException e) {
      return false;
    }
  }

  private static boolean isClassFile(InputStream in) {
    try {
      in.mark(CLASSSIG.length);
      final boolean ret = checkSig(in, CLASSSIG);
      in.reset();
      return ret;
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
        try (InputStream in = new BufferedInputStream(zf.getInputStream(ze))) {
          if (isClassFile(in)) {
            process(walker, in);
          }
        }
        catch (IOException e) {
          System.err.println("Failed reading " + ze.getName());
          throw e;
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
          if (isClassFile(p)) {
            try (InputStream in = new BufferedInputStream(Files.newInputStream(p))) {
              if (isClassFile(in)) {
                process(walker, in);
              }
            }
            catch (IOException e) {
              System.err.println("Failed reading " + p);
              throw e;
            }
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
