/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.tools;

import java.io.ByteArrayOutputStream;

/**
 * Utility class for decoding URL-encoded strings
 *
 * @deprecated Use {@link java.net.URLDecoder} instead.
 */
@Deprecated public class Decoder {

  public static String URLdecode(String s) {
    ByteArrayOutputStream out = new ByteArrayOutputStream(s.length());

    for (int i = 0; i < s.length(); ++i) {
      char c = s.charAt(i);
      if (c == '+') {
        c = ' ';
      }
      else if (c == '%') {
        try {
          c = (char) Integer.parseInt(s.substring(i + 1, i + 3), 16);
          i += 2;
        }
        catch (Exception e) {
          return s;
        }
      }
      out.write(c);
    }
    return out.toString();
  }
}
