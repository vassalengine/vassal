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
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;

public class DeprecationWriter {
  public static void main(String[] args) throws IOException {

    if (args.length != 1 && args.length != 3) {
      throw new IllegalArgumentException();
    }

    String infile;
    String outfile;

    if ("-o".equals(args[0])) {
      outfile = args[1];
      infile = args[2];
    }
    else {
      outfile = null;
      infile = args[0];
    }

    final DeprecationWalker d = new DeprecationWalker();

    try (PrintStream ps = args.length == 1 ? System.out : new PrintStream(outfile, StandardCharsets.UTF_8)) {
      d.setCallback((n, s, r) -> ps.println(n + "\t" + s + "\t" + r));
      Processor.process(d, infile);
    }
  }
} 
