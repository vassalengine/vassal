/*
 *
 * Copyright (c) 2009 by Brent Easton
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
package VASSAL.tools;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.util.NoSuchElementException;
import javax.swing.KeyStroke;

import VASSAL.configure.PropertyExpression;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class SequenceEncoderTest {

  /**
   * Test basic Sequence Encoder/Decoder functionality
   */

  @Test
  public void testEncodeDecodeBoolean() {
    final boolean VALUE = true;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextBoolean(false));
  }

  @Test
  public void testEncodeDecodeInt() {
    final int VALUE = 42;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextInt(999));
  }

  @Test
  public void testEncodeDecodeDouble() {
    final double VALUE = 3.1415926535;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(Double.toString(VALUE), Double.toString(sd.nextDouble(99.9)));
  }

  @Test
  public void testEncodeDecodeDoubleNAN() {
    final double VALUE = Double.NaN;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(Double.toString(VALUE), Double.toString(sd.nextDouble(99.9)));
  }

  @Test
  public void testEncodeDecodeDoubleInfinity() {
    final double VALUE = Double.POSITIVE_INFINITY;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(Double.toString(VALUE), Double.toString(sd.nextDouble(99.9)));
  }

  @Test
  public void testEncodeDecodeDoubleNegativeInfinity() {
    final double VALUE = Double.NEGATIVE_INFINITY;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(Double.toString(VALUE), Double.toString(sd.nextDouble(99.9)));
  }

  @Test
  public void testEncodeDecodeLong() {
    final long VALUE = 167772173;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextLong(999));
  }

  @Test
  public void testEncodeDecodeColor() {
    final Color VALUE = new Color(32, 145, 212);
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextColor(Color.RED));
  }

  @Test
  public void testEncodeDecodeKeyStroke() {
    final KeyStroke VALUE = KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_DOWN_MASK);
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextKeyStroke('X'));
  }

  @Test
  @SuppressWarnings("deprecation")
  public void testDecodeKeyStrokeWithDeprecatedModifier() {
    final String IN = KeyEvent.VK_F10 + "\\," + KeyEvent.CTRL_MASK;
    final KeyStroke OUT = KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_DOWN_MASK);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(IN, ',');
    assertEquals(OUT, sd.nextKeyStroke('X'));
  }

  @Test
  public void testEncodeDecodeNamedKeyStroke_1() {
    final NamedKeyStroke VALUE = NamedKeyStroke.of(KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_DOWN_MASK));
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextNamedKeyStroke());
  }

  @Test
  public void testEncodeDecodeNamedKeyStroke_2() {
    final NamedKeyStroke VALUE = NamedKeyStroke.of("#Control");
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextNamedKeyStroke());
  }

  @Test
  @SuppressWarnings("deprecation")
  public void testDecodeNamedKeyStrokeWithDeprecatedModifier() {
    final String IN = KeyEvent.VK_F10 + "\\," + KeyEvent.CTRL_MASK;
    final NamedKeyStroke OUT = NamedKeyStroke.of(KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_DOWN_MASK));
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(IN, ',');
    assertEquals(OUT, sd.nextNamedKeyStroke('X'));
  }

  @Test
  public void testEncodeDecodeString() {
    final String VALUE = "How many ,'s in this sentence?\n";
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextToken());
  }

  @Test
  public void testEncodeDecodeStringStartingWithDelim() {
    final String VALUE = ",hahahahah";
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, sd.nextToken());
  }

  @Test
  public void testEncodeDecodeStringArray() {
    final String[] VALUE = {"line 1", "line 2,", "line 3'", "line 4\n"};
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertArrayEquals(VALUE, sd.nextStringArray(0));
  }

  @Test
  public void testEncodeDecodePropertyExpression() {
    final PropertyExpression VALUE = new PropertyExpression("PieceName>=2");
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(VALUE, new PropertyExpression(sd.nextToken()));
  }

  @Test
  public void testEncodeDecodeMulti() {

    final boolean booleanIn = true;
    final int intIn = 42;
    final double doubleIn = 3.1415926535;
    final long longIn = 16777217;
    final Color colorIn = new Color(32, 145, 212);
    final KeyStroke keyStrokeIn = KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_DOWN_MASK);
    final NamedKeyStroke namedKeyStrokein1 = NamedKeyStroke.of(keyStrokeIn);
    final NamedKeyStroke namedKeyStrokein2 = NamedKeyStroke.of("#control");
    final String stringIn = "How many ,'s in this sentence?\n";
    final String[] stringArrayIn = {"line 1", "line 2,", "line 3'", "line 4\n"};
    final PropertyExpression propertyExpressionIn = new PropertyExpression("PieceName>=2");

    final SequenceEncoder se = new SequenceEncoder(',');
    se.append(booleanIn)
      .append(intIn)
      .append(doubleIn)
      .append(longIn)
      .append(colorIn)
      .append(keyStrokeIn)
      .append(namedKeyStrokein1)
      .append(namedKeyStrokein2)
      .append(stringIn)
      .append(stringArrayIn)
      .append(propertyExpressionIn);

    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');

    assertEquals(booleanIn, sd.nextBoolean(false));
    assertEquals(intIn, sd.nextInt(999));
    assertEquals(Double.toString(doubleIn),
                 Double.toString(sd.nextDouble(99.9)));
    assertEquals(longIn, sd.nextLong(999));
    assertEquals(colorIn, sd.nextColor(Color.RED));
    assertEquals(keyStrokeIn, sd.nextKeyStroke('X'));
    assertEquals(namedKeyStrokein1, sd.nextNamedKeyStroke());
    assertEquals(namedKeyStrokein2, sd.nextNamedKeyStroke());
    assertEquals(stringIn, sd.nextToken());
    assertArrayEquals(stringArrayIn, sd.nextStringArray(0));
    assertEquals(propertyExpressionIn, new PropertyExpression(sd.nextToken()));

    // Should be nothing left - should throw a NoSuchElementException
    assertThrows(NoSuchElementException.class, () -> sd.nextToken());
  }

  @Test
  public void testSingleQuoteBug2481() {
    // NB: This input can only be produced by hand-editing,
    // not by SequenceEncoder.
    final String bad = "stuff,'";

    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(bad, ',');

    assertEquals("stuff", sd.nextToken());
    assertEquals("'", sd.nextToken());
  }

  @Test
  public void testInitialNullBug3465() {
    // SequenceEncoder was failing to include initial null in sequence
    // Nulls are decoded as empty strings
    final String value2 = "value";
    final char delim = ';';

    final SequenceEncoder se = new SequenceEncoder(null, delim);
    se.append(value2);

    final SequenceEncoder.Decoder sd =
      new SequenceEncoder.Decoder(se.getValue(), delim);

    assertEquals("", sd.nextToken());
    assertEquals(value2, sd.nextToken());
  }

  @Test
  public void testUnquote() {
     final String q = "'12345'";
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(q, ',');
    assertEquals("12345", sd.nextToken());
  }

  @Test
  public void testDecoderCopyFromStart() {
    final char delim = ',';

    final SequenceEncoder se = new SequenceEncoder(delim);
    se.append(1).append("blah blah blah,,,").append((String) null).append(42);

    final SequenceEncoder.Decoder sd1 =
      new SequenceEncoder.Decoder(se.getValue(), delim);

    final SequenceEncoder.Decoder sd2 = sd1.copy();

    assertEquals(sd1.nextInt(-1), sd2.nextInt(-2));
    assertEquals(sd1.nextToken("x"), sd2.nextToken("y"));
    assertEquals(sd1.nextToken("x"), sd2.nextToken("y"));
    assertEquals(sd1.nextInt(-1), sd2.nextInt(-2));
    assertFalse(sd1.hasNext());
    assertFalse(sd2.hasNext());
  }

  @Test
  public void testDecoderCopyFromMiddle() {
    final char delim = ',';

    final SequenceEncoder se = new SequenceEncoder(delim);
    se.append(1).append("blah blah blah,,,").append((String) null).append(42);

    final SequenceEncoder.Decoder sd1 =
      new SequenceEncoder.Decoder(se.getValue(), delim);

    sd1.nextToken();
    sd1.nextToken();

    final SequenceEncoder.Decoder sd2 = sd1.copy();

    assertEquals(sd1.nextToken("x"), sd2.nextToken("y"));
    assertEquals(sd1.nextInt(-1), sd2.nextInt(-2));
    assertFalse(sd1.hasNext());
    assertFalse(sd2.hasNext());
  }
}
