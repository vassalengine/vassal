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

  /**
   * contains characters that might be part of a string representation of an int, long, double or boolean
   */
  private static final String UGLY = "-.0123456789EINaefilnrstuy";

  private StringBuilder buffer;
  private final char delimiter;
  private final boolean uglyDelimiter;

  public SequenceEncoder(char delimiter) {
    this.delimiter = delimiter;
    uglyDelimiter = UGLY.indexOf(delimiter) >= 0;
  }

  public SequenceEncoder(String val, char delimiter) {
    this(delimiter);
    append(val);
  }

  public SequenceEncoder append(String s) {
    startBufferOrAddDelimiter();

    if (s != null) {
      if (s.endsWith("\\") || (s.startsWith("'") && s.endsWith("'"))) {
        buffer.append('\'');
        appendEscapedString(s);
        buffer.append('\'');
      }
      else {
        appendEscapedString(s);
      }
    }

    return this;
  }

  public SequenceEncoder append(char c) {
    if (c == '\\' || c == '\'') {
      buffer.append('\'');
      if (c == delimiter) {
        buffer.append('\\');
      }
      buffer.append(c);
      buffer.append('\'');
    }
    else {
      if (c == delimiter) {
        buffer.append('\\');
      }
      buffer.append(c);
    }
    return this;
  }

  public SequenceEncoder append(int i) {
    if (uglyDelimiter) {
      return append(String.valueOf(i));
    }
    startBufferOrAddDelimiter();
    buffer.append(i);
    return this;
  }

  public SequenceEncoder append(long l) {
    if (uglyDelimiter) {
      return append(String.valueOf(l));
    }
    startBufferOrAddDelimiter();
    buffer.append(l);
    return this;
  }

  public SequenceEncoder append(double d) {
    if (uglyDelimiter) {
      return append(String.valueOf(d));
    }
    startBufferOrAddDelimiter();
    buffer.append(d);
    return this;
  }

  public SequenceEncoder append(boolean b) {
    if (uglyDelimiter) {
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

  /**
   * start the buffer, or add delimiter after previous token
   */
  private void startBufferOrAddDelimiter() {
    if (buffer == null) {
      buffer = new StringBuilder();
    }
    else {
      buffer.append(delimiter);
    }
  }

  public String getValue() {
    return buffer != null ? buffer.toString() : null;
  }

  private void appendEscapedString(String s) {
    int begin = 0;
    int end = s.indexOf(delimiter);

    while (begin <= end) {
      buffer.append(s, begin, end).append('\\');
      begin = end;
      end = s.indexOf(delimiter, end + 1);
    }

    buffer.append(s.substring(begin));
  }

  public static class Decoder implements Iterator<String> {
    private String val;
    private final char delimit;

    public Decoder(String value, char delimiter) {
      val = value;
      delimit = delimiter;
    }

    public boolean hasMoreTokens() {
      return val != null;
    }

    public String nextToken() {
      if (!hasMoreTokens()) throw new NoSuchElementException();

      String value;

      final int i = val.indexOf(delimit);
      if (i < 0) {
        value = val;
        val = null;
      }
      else {
        final StringBuilder buffer = new StringBuilder();
        int begin = 0;
        int end = i;
        while (begin < end) {
          if (val.charAt(end - 1) == '\\') {
            buffer.append(val, begin, end - 1);
            begin = end;
            end = val.indexOf(delimit, end + 1);
          }
          else {
            break;
          }
        }

        if (end < 0) {
          buffer.append(val.substring(begin));
          val = null;
        }
        else {
          buffer.append(val, begin, end);
          val = end >= val.length() - 1 ? "" : val.substring(end + 1);
        }

        value = buffer.toString();
      }

      if (value.startsWith("'") && value.endsWith("'") && value.length() > 1) {
        value = value.substring(1, value.length() - 1);
      }

      return value;
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
      return new Decoder(val, delimit);
    }

    /**
     * Parse the next token into an integer
     * @param defaultValue Return this value if no more tokens, or next token doesn't parse to an integer
     * @return next int token
     */
    public int nextInt(int defaultValue) {
      if (val != null) {
        try {
          defaultValue = Integer.parseInt(nextToken());
        }
        catch (NumberFormatException e) {
          // do nothing, this can happen and is handled by the surrounding code
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
          // do nothing, this can happen and is handled by the surrounding code
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
          // do nothing, this can happen and is handled by the surrounding code
        }
      }
      return defaultValue;
    }

    public boolean nextBoolean(boolean defaultValue) {
      return val != null ? "true".equals(nextToken()) : defaultValue;
    }

    /**
     * Return the first character of the next token
     * @param defaultValue Return this value if no more tokens, or if next token has zero length
     * @return first character of the next token
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
      return nextNamedKeyStroke(NamedKeyStroke.getNamedKeyStroke(defaultValue, InputEvent.CTRL_DOWN_MASK));
    }

    public NamedKeyStroke nextNamedKeyStroke() {
      return nextNamedKeyStroke(NamedKeyStroke.NULL_KEYSTROKE);
    }

    public NamedKeyStroke nextNamedKeyStroke(NamedKeyStroke defaultValue) {
      if (val != null) {
        String s = nextToken();
        if (s.length() == 0) {
          defaultValue = null;
        }
        else if (s.indexOf(',') < 0) {
          defaultValue = NamedKeyStroke.getNamedKeyStroke(s.charAt(0), InputEvent.CTRL_DOWN_MASK);
        }
        else {
          defaultValue = NamedHotKeyConfigurer.decode(s);
        }
      }
      return defaultValue == null ? NamedKeyStroke.NULL_KEYSTROKE : defaultValue;
    }

    /**
     * Return the next token, or the default value if there are no more tokens
     * @param defaultValue value to return if there are no more tokens
     * @return next token, or the default value
     */
    public String nextToken(String defaultValue) {
      return val != null ? nextToken() : defaultValue;
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

}
