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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.apache.commons.lang3.tuple.Pair;

public class RemovalAndDeprecationChecker {
  private final DependencyWalker walker;
  private final Set<String> removed;
  private final Set<String> deprecated;

  private String thisClass;

  public RemovalAndDeprecationChecker() throws IOException {
    walker = new DependencyWalker();
    removed = readRemoved();
    deprecated = readDeprecated();
  }

  private Set<String> readRemoved() throws IOException {
    final Set<String> r = new HashSet<>();
    try (InputStream in = getClass().getResourceAsStream("/removed")) {
      try (InputStreamReader isr = new InputStreamReader(in, StandardCharsets.UTF_8)) {
        try (BufferedReader br = new BufferedReader(isr)) {
          String line;
          while ((line = br.readLine()) != null) {
            line = line.strip();
            if (!line.isEmpty()) {
              r.add(line);
            }
          }
        }
      }
    }
    return r;
  }

  private Set<String> readDeprecated() throws IOException {
    // deprecated is tab-separated: name  since  forRemoval
    final Set<String> d = new HashSet<>();
    try (InputStream in = getClass().getResourceAsStream("/deprecated")) {
      try (InputStreamReader isr = new InputStreamReader(in, StandardCharsets.UTF_8)) {
        try (BufferedReader br = new BufferedReader(isr)) {
          String line;
          while ((line = br.readLine()) != null) {
            line = line.strip();
            if (!line.isEmpty()) {
              final String[] cols = line.split("\t");
              if ("true".equals(cols[2])) { // for removal
                d.add(cols[0]);
              }
            }
          }
        }
      }
    }
    return d;
  }

  private Pair<Set<String>, Set<String>> walk() {
    final Set<String> r = new HashSet<>();
    final Set<String> d = new HashSet<>();

    final Consumer<String> callback = s -> {
      if (removed.contains(s)) {
        r.add(s);
      }
      else if (deprecated.contains(s)) {
        d.add(s);
      }
    };

    walker.setClassCallback(callback);
    walker.setMethodCallback(callback);
    walker.setFieldCallback(callback);

    walker.walk();
    return Pair.of(r, d);
  }

  public Pair<Set<String>, Set<String>> check(byte[] classFile) {
    walker.setInput(classFile);
    return walk();
  }

  public Pair<Set<String>, Set<String>> check(InputStream in) throws IOException {
    walker.setInput(in);
    return walk();
  }

  public Pair<Set<String>, Set<String>> check(String className) throws IOException {
    walker.setInput(className);
    return walk();
  }

  public Pair<Map<String, Set<String>>, Map<String, Set<String>>> check(ZipFile zf) throws IOException {
    final Map<String, Set<String>> r = new HashMap<>();
    final Map<String, Set<String>> d = new HashMap<>();

    walker.setThisClassCallback(s -> thisClass = s);

    final Enumeration<? extends ZipEntry> entries = zf.entries();
    while (entries.hasMoreElements()) {
      final ZipEntry ze = entries.nextElement();
      if (ze.getName().endsWith(".class")) {
        try (InputStream in = zf.getInputStream(ze)) {
          final Pair<Set<String>, Set<String>> p = check(in);
          // store the results by class
          if (!p.getLeft().isEmpty()) {
            r.put(thisClass, p.getLeft());
          }
          if (!p.getRight().isEmpty()) {
            d.put(thisClass, p.getRight());
          }
        }
      }
    }
    return Pair.of(r, d);
  }

  public static String formatResult(Map<String, Set<String>> dmap) {
    // a => b
    final StringBuilder sb = new StringBuilder();
    final List<String> dependers = new ArrayList<>(dmap.keySet());
    Collections.sort(dependers);

    for (final String dhead: dependers) {
      final List<String> ds = new ArrayList<>(dmap.get(dhead));
      Collections.sort(ds);
      for (final String dtail: ds) {
        sb.append(dhead).append(" => ").append(dtail).append('\n');
      }
    }

    return sb.toString();
  }
}
