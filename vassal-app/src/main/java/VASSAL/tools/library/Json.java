/*
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.tools.library;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * A minimal JSON reader — just enough to read the game library's project
 * documents.
 *
 * <p>Hand-rolled rather than pulled in: adding a JSON dependency for one
 * read-only document is not worth it. The grammar implemented here is all of
 * RFC 8259 except that numbers are kept as {@link Double} (nothing in these
 * documents needs more), which keeps it to something reviewable.</p>
 *
 * <p>Values map to {@code Map<String,Object>}, {@code List<Object>},
 * {@link String}, {@link Double}, {@link Boolean} and {@code null}. The typed
 * accessors do the casting and return a default rather than throwing, because
 * a field the library stops sending should degrade rather than break the
 * application.</p>
 *
 * @since 3.8.0
 */
public final class Json {

  private final String src;
  private int pos;

  private Json(String src) {
    this.src = src;
  }

  /**
   * Parses a whole document.
   *
   * @param text the JSON text
   * @return the parsed value
   * @throws IllegalArgumentException if malformed
   */
  public static Object parse(String text) {
    final Json p = new Json(text);
    p.ws();
    final Object v = p.value();
    p.ws();
    if (p.pos < p.src.length()) {
      throw new IllegalArgumentException("trailing content at offset " + p.pos); //NON-NLS
    }
    return v;
  }

  // ---- typed accessors -----------------------------------------------------

  /** An object's map, or an empty map when {@code v} is not an object. */
  @SuppressWarnings("unchecked")
  public static Map<String, Object> obj(Object v) {
    return v instanceof Map ? (Map<String, Object>) v : new LinkedHashMap<>();
  }

  /** An array's list, or an empty list when {@code v} is not an array. */
  @SuppressWarnings("unchecked")
  public static List<Object> arr(Object v) {
    return v instanceof List ? (List<Object>) v : new ArrayList<>();
  }

  /** A string value, or {@code dflt} when {@code v} is not a string. */
  public static String str(Object v, String dflt) {
    return v instanceof String ? (String) v : dflt;
  }

  /** A numeric value, or {@code dflt} when {@code v} is not a number. */
  public static long num(Object v, long dflt) {
    return v instanceof Double ? ((Double) v).longValue() : dflt;
  }

  /** {@code get(root, "packages")} — a field of an object, or {@code null}. */
  public static Object get(Object v, String field) {
    return obj(v).get(field);
  }

  // ---- parser --------------------------------------------------------------

  private Object value() {
    if (pos >= src.length()) {
      throw err("unexpected end of input"); //NON-NLS
    }
    final char c = src.charAt(pos);
    switch (c) {
    case '{':
      return object();
    case '[':
      return array();
    case '"':
      return string();
    case 't':
      expect("true"); //NON-NLS
      return Boolean.TRUE;
    case 'f':
      expect("false"); //NON-NLS
      return Boolean.FALSE;
    case 'n':
      expect("null"); //NON-NLS
      return null;
    default:
      return number();
    }
  }

  private Map<String, Object> object() {
    final Map<String, Object> out = new LinkedHashMap<>();
    pos++;                                  // '{'
    ws();
    if (peek() == '}') {
      pos++;
      return out;
    }
    while (true) {
      ws();
      if (peek() != '"') {
        throw err("expected a field name"); //NON-NLS
      }
      final String k = string();
      ws();
      if (peek() != ':') {
        throw err("expected ':'"); //NON-NLS
      }
      pos++;
      ws();
      out.put(k, value());
      ws();
      final char c = peek();
      pos++;
      if (c == '}') {
        return out;
      }
      if (c != ',') {
        throw err("expected ',' or '}'"); //NON-NLS
      }
    }
  }

  private List<Object> array() {
    final List<Object> out = new ArrayList<>();
    pos++;                                  // '['
    ws();
    if (peek() == ']') {
      pos++;
      return out;
    }
    while (true) {
      ws();
      out.add(value());
      ws();
      final char c = peek();
      pos++;
      if (c == ']') {
        return out;
      }
      if (c != ',') {
        throw err("expected ',' or ']'"); //NON-NLS
      }
    }
  }

  private String string() {
    pos++;                                  // opening quote
    final StringBuilder sb = new StringBuilder();
    while (true) {
      if (pos >= src.length()) {
        throw err("unterminated string"); //NON-NLS
      }
      final char c = src.charAt(pos++);
      if (c == '"') {
        return sb.toString();
      }
      if (c != '\\') {
        sb.append(c);
        continue;
      }
      final char e = src.charAt(pos++);
      switch (e) {
      case '"':
        sb.append('"');
        break;
      case '\\':
        sb.append('\\');
        break;
      case '/':
        sb.append('/');
        break;
      case 'b':
        sb.append('\b');
        break;
      case 'f':
        sb.append('\f');
        break;
      case 'n':
        sb.append('\n');
        break;
      case 'r':
        sb.append('\r');
        break;
      case 't':
        sb.append('\t');
        break;
      case 'u':
        sb.append((char) Integer.parseInt(src.substring(pos, pos + 4), 16));
        pos += 4;
        break;
      default:
        throw err("bad escape \\" + e); //NON-NLS
      }
    }
  }

  private Double number() {
    final int start = pos;
    while (pos < src.length() && "+-.eE0123456789".indexOf(src.charAt(pos)) >= 0) { //NON-NLS
      pos++;
    }
    if (start == pos) {
      throw err("expected a value"); //NON-NLS
    }
    try {
      return Double.valueOf(src.substring(start, pos));
    }
    catch (NumberFormatException e) {
      throw err("bad number " + src.substring(start, pos)); //NON-NLS
    }
  }

  private void expect(String word) {
    if (!src.startsWith(word, pos)) {
      throw err("expected " + word); //NON-NLS
    }
    pos += word.length();
  }

  private char peek() {
    if (pos >= src.length()) {
      throw err("unexpected end of input"); //NON-NLS
    }
    return src.charAt(pos);
  }

  private void ws() {
    while (pos < src.length() && Character.isWhitespace(src.charAt(pos))) {
      pos++;
    }
  }

  private IllegalArgumentException err(String msg) {
    return new IllegalArgumentException("JSON: " + msg + " at offset " + pos); //NON-NLS
  }
}
