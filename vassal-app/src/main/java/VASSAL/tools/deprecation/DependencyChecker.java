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
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

public class DependencyChecker {
  public static void main(String[] args) throws IOException {

    if (args.length != 2 && args.length != 4) {
      throw new IllegalArgumentException();
    }

    String infile;
    String outfile;
    String compfile;

    if ("-o".equals(args[0])) {
      outfile = args[1];
      compfile = args[2];
      infile = args[3];
    }
    else {
      outfile = null;
      compfile = args[0];
      infile = args[1];
    }

    // load the comparison set
    final Set<String> comp = new HashSet<>();
    try (InputStream in = Files.newInputStream(Paths.get(compfile))) {
      Processor.readCompSet(in, cols -> comp.add(cols[0]));
    }

    final DependencyWalker walker = new DependencyWalker();

    final Set<String> deps = new HashSet<>();

    final Consumer<String> collect = s -> {
      if (comp.contains(s)) {
        deps.add(s);
      }
    };

    walker.setClassCallback(collect);
    walker.setMethodCallback(collect);
    walker.setFieldCallback(collect);

    try (PrintStream ps = outfile == null ? System.out : new PrintStream(outfile, StandardCharsets.UTF_8)) {
      walker.setThisClassEndCallback(s -> {
        if (!deps.isEmpty()) {
          ps.println(s);
          final String[] darr = deps.toArray(new String[0]);
          Arrays.sort(darr);
          for (final String dep: darr) {
            ps.println("  " + dep);
          }
          ps.println("");
          deps.clear();
        }
      });

      Processor.process(walker, infile);
    }
  }
}
