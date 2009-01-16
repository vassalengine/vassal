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

import java.awt.Color;
import java.awt.event.InputEvent;
import java.util.NoSuchElementException;
import javax.swing.KeyStroke;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringArrayConfigurer;

/**
 * Encodes a sequence of Strings into a single String with a given delimiter.
 * Escapes the delimiter character if it occurs in the element strings.
 *
 * This is a very handy class for storing structured data into flat text and quite a bit faster than parsing
 * an XML document.
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
  private char delimit;

  public SequenceEncoder(char delimiter) {
    this(null, delimiter);
  }

  public SequenceEncoder(String val, char delimiter) {
    delimit = delimiter;
    if (val != null) {
      append(val);
    }
  }

  public SequenceEncoder append(String s) {
    if (buffer == null) {
      buffer = new StringBuilder();
      appendEscapedString(s);
    }
    else {
      buffer.append(delimit);
      appendEscapedString(s);
    }
    return this;
  }

  public SequenceEncoder append(char c) {
    return append(String.valueOf(c));
  }

  public SequenceEncoder append(int i) {
    return append(String.valueOf(i));
  }

  public SequenceEncoder append(long l) {
    return append(String.valueOf(l));
  }

  public SequenceEncoder append(double d) {
    return append(String.valueOf(d));
  }

  public SequenceEncoder append(boolean b) {
    return append(String.valueOf(b));
  }

  public SequenceEncoder append(KeyStroke stroke) {
    String s = HotKeyConfigurer.encode(stroke);
    return append(s != null ? s : "");
  }

  public SequenceEncoder append(Color color) {
    String s = ColorConfigurer.colorToString(color);
    return append(s != null ? s : "");
  }

  public SequenceEncoder append(String[] s) {
    return append(StringArrayConfigurer.arrayToString(s));
  }

  public String getValue() {
    return buffer != null ? buffer.toString() : null;
  }

  private void appendEscapedString(String s) {
    final String ss = (s == null ? "" : s);
    int begin = 0;
    int end = ss.indexOf(delimit);
    int length = buffer.length();

    while (begin <= end) {
      buffer.append(ss.substring(begin, end)).append('\\');
      begin = end;
      end = ss.indexOf(delimit, end + 1);
    }
    buffer.append(ss.substring(begin));
    if (ss.endsWith("\\")
        || (ss.startsWith("\'")
        && ss.endsWith("\'"))) {
      buffer.insert(length, "'").append("'");
    }
  }

  public static class Decoder {
    private String val;
    private char delimit;

    public Decoder(String value, char delimiter) {
      val = value;
      delimit = delimiter;
    }

    public boolean hasMoreTokens() {
      return val != null;
    }

    public String nextToken() {
      if (!hasMoreTokens()) {
        throw new NoSuchElementException();
      }
      String value = val;
      int i = val.indexOf(delimit);
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
            buffer.append(val.substring(begin, end - 1));
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
          buffer.append(val.substring(begin, end));
          val = end >= val.length() - 1 ? "" : val.substring(end + 1);
        }
        value = buffer.toString();
      }
      if (value.startsWith("'")
          && value.endsWith("'")) {
        value = value.substring(1, value.length() - 1);
      }
      return value;
    }

    public Decoder copy() {
      return new Decoder(val, delimit);
    }

    /**
     * Parse the next token into an integer
     * @param defaultValue Return this value if no more tokens, or next token doesn't parse to an integer
     * @return
     */
    public int nextInt(int defaultValue) {
      if (val != null) {
        try {
          defaultValue = Integer.parseInt(nextToken());
        }
        catch (NumberFormatException e) {
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
        }
      }
      return defaultValue;
    }

    public boolean nextBoolean(boolean defaultValue) {
      if (val != null) {
        defaultValue = "true".equals(nextToken());
      }
      return defaultValue;
    }

    /**
     * Return the first character of the next token
     * @param defaultValue Return this value if no more tokens, or if next token has zero length
     * @return
     */
    public char nextChar(char defaultValue) {
      if (val != null) {
        String s = nextToken();
        defaultValue = s.length() > 0 ? s.charAt(0) : defaultValue;
      }
      return defaultValue;
    }

    public KeyStroke nextKeyStroke(char defaultValue) {
      return nextKeyStroke(KeyStroke.getKeyStroke(defaultValue, InputEvent.CTRL_MASK));
    }

    public Color nextColor(Color defaultValue) {
      if (val != null) {
        String s = nextToken();
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
        String s = nextToken();
        if (s.length() == 0) {
          defaultValue = null;
        }
        else if (s.indexOf(',') < 0) {
          defaultValue = KeyStroke.getKeyStroke(s.charAt(0), InputEvent.CTRL_MASK);
        }
        else {
          defaultValue = HotKeyConfigurer.decode(s);
        }
      }
      return defaultValue;
    }

    /**
     * Return the next token, or the default value if there are no more tokens
     * @param defaultValue
     * @return
     */
    public String nextToken(String defaultValue) {
      if (val != null) {
        defaultValue = nextToken();
      }
      return defaultValue;
    }

    public String[] nextStringArray(int minLength) {
      String[] retVal = new String[0];
      if (val != null) {
        retVal = StringArrayConfigurer.stringToArray(nextToken());
      }
      if (retVal.length < minLength) {
        retVal = ArrayUtils.copyOf(retVal, minLength);
      }
      return retVal;
    }
  }

  public static void main(String args[]) {
    SequenceEncoder se = new SequenceEncoder(',');
    for (int i = 0; i < args.length; ++i) {
      se.append(args[i]);
    }
    System.out.println(se.getValue());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(se.getValue(), ',');
    while (st.hasMoreTokens()) {
      System.out.println(st.nextToken());
    }
  }
}
