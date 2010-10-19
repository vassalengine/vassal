/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.io;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.junit.Ignore;

@Ignore
public class ProcessCallableTestEchoer {
  public static void main(String[] args) throws Exception {
    final BufferedReader r =
      new BufferedReader(new InputStreamReader(System.in));

    System.out.println(r.readLine());
    System.err.println(r.readLine());
    System.exit(Integer.parseInt(r.readLine()));
  }
}
