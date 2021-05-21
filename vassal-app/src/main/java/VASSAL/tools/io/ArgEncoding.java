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

package VASSAL.tools.io;

import java.io.File;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.apache.commons.lang3.SystemUtils;

public class ArgEncoding {
  // ProcessBuilder on Windows misencodes arguments containing Unicode code
  // points outside of the windows-1252 encoding. These utility methods are
  // here for URL-encoding and -decoding arguments as a workaround.

  private ArgEncoding() {}

  private static boolean aboveFF(String s) {
    return s.chars().anyMatch(c -> c > 0xFF);
  }

  public static boolean requires(String s) {
    return SystemUtils.IS_OS_WINDOWS && s != null && aboveFF(s);
  }

  public static String encode(String a) {
    return URLEncoder.encode(a, StandardCharsets.UTF_8);
  }

  public static String decode(String a) {
    return URLDecoder.decode(a, StandardCharsets.UTF_8);
  }

  public static File decode(boolean encoded, String a) {
    return new File(encoded ? URLDecoder.decode(a, StandardCharsets.UTF_8) : a);
  }
}
