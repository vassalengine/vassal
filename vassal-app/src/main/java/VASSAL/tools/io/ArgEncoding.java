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
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;

import org.apache.commons.lang3.SystemUtils;

/**
 * URL-encodes and -decodes arguments which would not survive the trip to a
 * child process on Windows.
 *
 * <p>We pass the command line to {@code CreateProcessW} as UTF-16, so it
 * leaves this JVM intact, but the child's launcher reads it back with
 * {@code GetCommandLineA}. That converts through the system ANSI code page
 * and substitutes {@code ?} for every character the code page has no room
 * for, so the child is handed a path it cannot open. Encoding such an
 * argument keeps it inside ASCII for the crossing; the receiving end decodes
 * it after {@code --encoded-args}.</p>
 *
 * <p>Which characters survive is therefore a property of the machine's ANSI
 * code page, not a fixed set. {@code sun.jnu.encoding} is derived from
 * {@code GetACP()} during JVM startup, so we can ask it directly. The older
 * test here assumed the code page was always windows-1252 and passed
 * anything below U+0100 through raw, which mangled every accented letter on
 * the many Windows systems whose code page is not windows-1252 -- a module
 * in a folder named {@code La Bataille d'Auerst}&#x00e6;{@code dt} reached
 * the image tiler as {@code Auerst?dt} and could not be tiled at all
 * (vassalengine/vassal#14193).</p>
 *
 * <p>This is kept to Windows deliberately. On Unix the parent writes
 * arguments in the default charset -- UTF-8 since JEP 400 -- while the child
 * decodes them with {@code sun.jnu.encoding}, so the question there is
 * whether those two charsets agree, which is a different predicate from this
 * one. The Unix failure is a broken C library locale and is dealt with by
 * repairing the locale rather than by encoding arguments.</p>
 */
public class ArgEncoding {

  private ArgEncoding() {}

  /**
   * The charset an argument must survive: the one the child's launcher will
   * convert the command line through.
   */
  private static final Charset ARG_CHARSET = argCharset();

  private static Charset argCharset() {
    final String enc = System.getProperty("sun.jnu.encoding"); //NON-NLS
    if (enc != null) {
      try {
        return Charset.forName(enc);
      }
      catch (IllegalCharsetNameException | UnsupportedCharsetException e) {
        // not a charset we have; fall through to the conservative answer
      }
    }

    // We cannot tell what the child will decode with, so assume the
    // narrowest thing it could be. Everything non-ASCII is then encoded,
    // which costs a little length and is never wrong: the receiving end
    // decodes whatever we encode.
    return StandardCharsets.US_ASCII;
  }

  /**
   * @param s the argument, which may be null
   * @param cs the charset the argument must survive
   * @return whether {@code s} must be encoded to cross intact
   */
  static boolean requires(String s, Charset cs) {
    // CharsetEncoder is not thread-safe, so take a fresh one each time.
    return s != null && !cs.newEncoder().canEncode(s);
  }

  /**
   * @param s the argument, which may be null
   * @return whether {@code s} must be encoded to reach a child process intact
   */
  public static boolean requires(String s) {
    return SystemUtils.IS_OS_WINDOWS && requires(s, ARG_CHARSET);
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
