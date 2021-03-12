/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Joel Uckelman
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

import java.awt.Color;
import java.awt.event.InputEvent;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

import javax.swing.KeyStroke;

import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringArrayConfigurer;

/**
 * Encodes a sequence of Strings into a single String with a given delimiter.
 * Escapes the delimiter character if it occurs in the element strings.
 *
 * This is a very handy class for storing structured data into flat text and
 * quite a bit faster than parsing an XML document.
 *
 * For example, a structure such as {A,{B,C}} can be encoded with
 *
 * <pre>
 * new SequenceEncoder("A",',').append(new SequenceEncoder("B",',').append("C").getValue()).getValue()
 * </pre>
 *
 * which returns <code>A,B\,C</code>
 *
 * and then extracted with
 *
 * <pre>
 * SequenceEncoder.Decoder st = new SequenceEncoder.Decoder("A,B\,C",',');
 * String A = st.nextToken();
 * SequenceEncoder.Decoder BC = new SequenceEncoder.Decoder(st.nextToken(),',');
 * String B = BC.nextToken();
 * String C = BC.nextToken();
 * </pre>
 */
public class SequenceEncoder {
  private StringBuilder buffer;
  private final char delim;

  // Ugly delimiters: The characters in UGLY can occur in what's returned
  // by String.valueOf() for boolean, int, long, and double---that is,
  // anything which looks like a number (possibly in scientific notation,
  // e.g., 1E-6) but also true, false, Infinity, and NaN. When the delimiter
  // is none of these characters, we can hand these primitive types directly
  // to the StringBuilder without doing any escaping.
  //
  // These characters are all terrible choices for delimiters anyway, so
  // hopefully no one uses them, but we have to check just in case.
  private static final String UGLY = "-.0123456789EINaefilnrstuy"; //NON-NLS
  private final boolean uglyDelim;

  public SequenceEncoder(char delimiter) {
    delim = delimiter;
    uglyDelim = UGLY.indexOf(delim) != -1;
  }

  public SequenceEncoder(String val, char delimiter) {
    this(delimiter);
    append(val);
  }

  private void startBufferOrAddDelimiter() {
    if (buffer == null) {
      buffer = new StringBuilder();
    }
    else {
      buffer.append(delim);
    }
  }

  public SequenceEncoder append(String s) {
    startBufferOrAddDelimiter();

    if (s == null || s.isEmpty()) {
      return this;
    }

    if (s.charAt(0) == '\\' ||
        (s.charAt(0) == '\'' && s.charAt(s.length() - 1) == '\'')) {
      buffer.append('\'');
      appendEscapedString(s);
      buffer.append('\'');
    }
    else {
      appendEscapedString(s);
    }

    return this;
  }

  private void appendEscapedChar(char c) {
    if (c == delim) {
      buffer.append('\\');
    }
    buffer.append(c);
  }

  public SequenceEncoder append(char c) {
    startBufferOrAddDelimiter();

    if (c == '\\' || c == '\'') {
      buffer.append('\'');
      appendEscapedChar(c);
      buffer.append('\'');
    }
    else {
      appendEscapedChar(c);
    }

    return this;
  }

  public SequenceEncoder append(int i) {
    if (uglyDelim) {
      return append(String.valueOf(i));
    }
    startBufferOrAddDelimiter();
    buffer.append(i);
    return this;
  }

  public SequenceEncoder append(long l) {
    if (uglyDelim) {
      return append(String.valueOf(l));
    }
    startBufferOrAddDelimiter();
    buffer.append(l);
    return this;
  }

  public SequenceEncoder append(double d) {
    if (uglyDelim) {
      return append(String.valueOf(d));
    }
    startBufferOrAddDelimiter();
    buffer.append(d);
    return this;
  }

  public SequenceEncoder append(boolean b) {
    if (uglyDelim) {
      return append(String.valueOf(b));
    }
    startBufferOrAddDelimiter();
    buffer.append(b);
    return this;
  }

  public SequenceEncoder append(KeyStroke stroke) {
    return append(HotKeyConfigurer.encode(stroke));
  }

  public SequenceEncoder append(NamedKeyStroke stroke) {
    return append(NamedHotKeyConfigurer.encode(stroke));
  }

  public SequenceEncoder append(Color color) {
    return append(ColorConfigurer.colorToString(color));
  }

  public SequenceEncoder append(String[] s) {
    return append(StringArrayConfigurer.arrayToString(s));
  }

  public SequenceEncoder append(PropertyExpression p) {
    return append(p.getExpression());
  }

  public String getValue() {
    return buffer != null ? buffer.toString() : null;
  }

  private void appendEscapedString(String s) {
    int begin = 0;
    int end = s.indexOf(delim);

    while (begin <= end) {
      buffer.append(s, begin, end).append('\\');
      begin = end;
      end = s.indexOf(delim, end + 1);
    }

    buffer.append(s, begin, s.length());
  }

  public static class Decoder implements Iterator<String> {
    private String val;
    private final char delim;

    private StringBuilder buf;

    private int start;
    private final int stop;

    public Decoder(String value, char delimiter) {
      val = value;
      delim = delimiter;

      start = 0;
      stop = val != null ? val.length() : 0;
    }

    public Decoder(Decoder d) {
      val = d.val;
      delim = d.delim;

      start = d.start;
      stop = d.stop;
    }

    public boolean hasMoreTokens() {
      return val != null;
    }

    public String nextToken() {
      if (!hasMoreTokens()) throw new NoSuchElementException();

      if (start == stop) {
        // token for "null" is the empty string
        val = null;
        return "";
      }

      if (buf != null) {
        buf.setLength(0);
      }

      String tok = null;
      int i = start;
      for ( ; i < stop; ++i) {
        if (val.charAt(i) == delim) {
          if (i > 0 && val.charAt(i - 1) == '\\') {
            // escaped delimiter; piece together the token
            if (buf == null) {
              buf = new StringBuilder();
            }
            buf.append(val, start, i - 1);
            start = i;
          }
          else {
            // real delimiter
            if (buf == null || buf.length() == 0) {
              // no escapes; take the token whole
              tok = val.substring(start, i);
            }
            else {
              // had an earlier escape; cobble on the end
              buf.append(val, start, i);
            }
            start = i + 1;
            break;
          }
        }
      }

      if (start < i) {
        // i == stop; we reached the end without a delimiter
        if (buf == null || buf.length() == 0) {
          // no escapes; take the token whole
          tok = val.substring(start);
        }
        else {
          // had an earlier escape; cobble on the end
          buf.append(val, start, stop);
        }
        val = null;
      }

      return unquote(tok != null ? tok : buf).intern();
    }

    private String unquote(CharSequence cs) {
      // strip enclosure by single quotes
      final int len = cs.length();
      return (
        len > 1 && cs.charAt(0) == '\'' && cs.charAt(len - 1) == '\'' ?
        cs.subSequence(1, len - 1) : cs
      ).toString();
    }

    @Override
    public boolean hasNext() {
      return hasMoreTokens();
    }

    @Override
    public String next() {
      return nextToken();
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }

    public Decoder copy() {
      return new Decoder(this);
    }

    /**
     * Parse the next token into an integer
     * @param defaultValue Return this value if no more tokens, or next token doesn't parse to an integer
     * @return next token as an integer, or defaultValue if it didn't exist or didn't parse
     */
    public int nextInt(int defaultValue) {
      if (val != null) {
        try {
          defaultValue = Integer.parseInt(nextToken());
        }
        catch (NumberFormatException e) {
          // no action
        }
      }
      return defaultValue;
    }

    public long nextLong(long defaultValue) {
      if (val != null) {
        try {
          defaultValue = Long.parseLong(nextToken());
        }
        catch (NumberFormatException e) {
          // no action
        }
      }
      return defaultValue;
    }

    public double nextDouble(double defaultValue) {
      if (val != null) {
        try {
          defaultValue = Double.parseDouble(nextToken());
        }
        catch (NumberFormatException e) {
          // no action
        }
      }
      return defaultValue;
    }

    public boolean nextBoolean(boolean defaultValue) {
      return val != null ? "true".equals(nextToken()) : defaultValue; //NON-NLS
    }

    /**
     * Return the first character of the next token
     * @param defaultValue Return this value if no more tokens, or if next token has zero length
     * @return next token if a character is available, or defaultValue if no more tokens or the token has zero length
     */
    public char nextChar(char defaultValue) {
      if (val != null) {
        final String s = nextToken();
        defaultValue = s.length() > 0 ? s.charAt(0) : defaultValue;
      }
      return defaultValue;
    }

    public KeyStroke nextKeyStroke(char defaultValue) {
      return nextKeyStroke(
        KeyStroke.getKeyStroke(defaultValue, InputEvent.CTRL_DOWN_MASK));
    }

    public Color nextColor(Color defaultValue) {
      if (val != null) {
        final String s = nextToken();
        if (s.length() > 0) {
          defaultValue = ColorConfigurer.stringToColor(s);
        }
        else {
          defaultValue = null;
        }
      }
      return defaultValue;
    }

    public KeyStroke nextKeyStroke(KeyStroke defaultValue) {
      if (val != null) {
        final String s = nextToken();
        if (s.length() == 0) {
          defaultValue = null;
        }
        else if (s.indexOf(',') < 0) {
          defaultValue =
            KeyStroke.getKeyStroke(s.charAt(0), InputEvent.CTRL_DOWN_MASK);
        }
        else {
          defaultValue = HotKeyConfigurer.decode(s);
        }
      }
      return defaultValue;
    }

    public NamedKeyStroke nextNamedKeyStroke(char defaultValue) {
      return nextNamedKeyStroke(NamedKeyStroke.of(defaultValue, InputEvent.CTRL_DOWN_MASK));
    }

    public NamedKeyStroke nextNamedKeyStroke() {
      return nextNamedKeyStroke(NamedKeyStroke.NULL_KEYSTROKE);
    }

    public NamedKeyStroke nextNamedKeyStroke(NamedKeyStroke defaultValue) {
      if (val != null) {
        final String s = nextToken();
        if (s.length() == 0) {
          defaultValue = null;
        }
        else if (s.indexOf(',') < 0) {
          defaultValue = NamedKeyStroke.of(s.charAt(0), InputEvent.CTRL_DOWN_MASK);
        }
        else {
          defaultValue = NamedHotKeyConfigurer.decode(s);
        }
      }
      return defaultValue == null ? NamedKeyStroke.NULL_KEYSTROKE : defaultValue;
    }

    /**
     * Return the next token, or the default value if there are no more tokens
     * @param defaultValue default value in case there are no more tokens
     * @return next token, or the default value if no more tokens
     */
    public String nextToken(String defaultValue) {
      return val != null ? nextToken() : (defaultValue != null ? defaultValue.intern() : null);
    }

    public String[] nextStringArray(int minLength) {
      String[] retVal;
      if (val != null) {
        retVal = StringArrayConfigurer.stringToArray(nextToken());
      }
      else {
        retVal = new String[0];
      }

      if (retVal.length < minLength) {
        retVal = Arrays.copyOf(retVal, minLength);
      }

      return retVal;
    }
  }

  public static void main(String[] args) {
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final String arg : args) {
      se.append(arg);
    }
    System.out.println(se.getValue());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(se.getValue(), ',');
    while (st.hasMoreTokens()) {
      System.out.println(st.nextToken());
    }
  }
}
