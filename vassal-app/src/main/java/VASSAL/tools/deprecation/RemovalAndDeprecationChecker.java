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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.zip.ZipFile;

import org.apache.commons.lang3.tuple.Pair;

public class RemovalAndDeprecationChecker {
  private final DependencyWalker walker;
  private final Map<String, String> removed;
  private final Map<String, String> deprecated;

  public RemovalAndDeprecationChecker() throws IOException {
    walker = new DependencyWalker();
    removed = readRemoved();
    deprecated = readDeprecated();
  }

  private Map<String, String> readRemoved() throws IOException {
    final Map<String, String> r = new HashMap<>();
    try (InputStream in = getClass().getResourceAsStream("/removed")) {
      // cols are item name, version when removed
      Processor.readCompSet(in, cols -> r.put(cols[0], cols[1]));
    }
    return r;
  }

  private Map<String, String> readDeprecated() throws IOException {
    // deprecated is tab-separated: name  since  forRemoval
    final Map<String, String> d = new HashMap<>();
    try (InputStream in = getClass().getResourceAsStream("/deprecated")) {
      Processor.readCompSet(in, cols -> {
        if ("true".equals(cols[2])) { // forRemoval
          // item name, deprecation date
          d.put(cols[0], cols[1]);
        }
      });
    }
    return d;
  }

  public Pair<Map<String, Map<String, String>>, Map<String, Map<String, String>>> check(ZipFile zf) throws IOException {
    final Map<String, Map<String, String>> rmap = new HashMap<>();
    final Map<String, Map<String, String>> dmap = new HashMap<>();

    final Map<String, String> removed_used = new HashMap<>();
    final Map<String, String> deprecated_used = new HashMap<>();

    final Consumer<String> callback = s -> {
      final String version = removed.get(s);
      if (version != null) {
        removed_used.put(s, version);
      }
      else {
        final String when = deprecated.get(s);
        if (when != null) {
          deprecated_used.put(s, when);
        }
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
        rmap.put(s, new HashMap<>(removed_used));
      }

      if (!deprecated_used.isEmpty()) {
        dmap.put(s, new HashMap<>(deprecated_used));
      }
    });

    Processor.process(walker, zf);
    return Pair.of(rmap, dmap);
  }

  public static String formatResult(Map<String, Map<String, String>> dmap) {
    // a => b
    final StringBuilder sb = new StringBuilder();
    final List<String> dependers = new ArrayList<>(dmap.keySet());
    Collections.sort(dependers);

    for (final String dhead: dependers) {
      final Map<String, String> dtmap = dmap.get(dhead);
      final List<String> ds = new ArrayList<>(dtmap.keySet());
      Collections.sort(ds);
      for (final String dtail: ds) {
        sb.append('\n').append(dhead).append(" => ").append(dtail).append(", ").append(dtmap.get(dtail));
      }
    }

    return sb.toString();
  }
}
