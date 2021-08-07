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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.zip.ZipFile;

import org.apache.commons.lang3.tuple.Pair;

public class RemovalAndDeprecationChecker {
  private final DependencyWalker walker;
  private final Set<String> removed;
  private final Set<String> deprecated;

  public RemovalAndDeprecationChecker() throws IOException {
    walker = new DependencyWalker();
    removed = readRemoved();
    deprecated = readDeprecated();
  }

  private Set<String> readRemoved() throws IOException {
    final Set<String> r = new HashSet<>();
    try (InputStream in = getClass().getResourceAsStream("/removed")) {
      Processor.readCompSet(in, cols -> r.add(cols[0]));
    }
    return r;
  }

  private Set<String> readDeprecated() throws IOException {
    // deprecated is tab-separated: name  since  forRemoval
    final Set<String> d = new HashSet<>();
    try (InputStream in = getClass().getResourceAsStream("/deprecated")) {
      Processor.readCompSet(in, cols -> {
        if ("true".equals(cols[2])) { // forRemoval
          d.add(cols[0]);
        }
      });
    }
    return d;
  }

  public Pair<Map<String, Set<String>>, Map<String, Set<String>>> check(ZipFile zf) throws IOException {
    final Map<String, Set<String>> rmap = new HashMap<>();
    final Map<String, Set<String>> dmap = new HashMap<>();

    final Set<String> removed_used = new HashSet<>();
    final Set<String> deprecated_used = new HashSet<>();

    final Consumer<String> callback = s -> {
      if (removed.contains(s)) {
        removed_used.add(s);
      }
      else if (deprecated.contains(s)) {
        deprecated_used.add(s);
      }
    };

    walker.setClassCallback(callback);
    walker.setMethodCallback(callback);
    walker.setFieldCallback(callback);

    walker.setThisClassBeginCallback(s -> {
      removed_used.clear();
      deprecated_used.clear();
    });

    walker.setThisClassEndCallback(s -> {
      if (!removed_used.isEmpty()) {
        rmap.put(s, new HashSet<>(removed_used));
      }

      if (!deprecated_used.isEmpty()) {
        dmap.put(s, new HashSet<>(deprecated_used));
      }
    });

    Processor.process(walker, zf);
    return Pair.of(rmap, dmap);
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
