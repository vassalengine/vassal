/*
 * $Id$
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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.util.Arrays;
import java.util.NoSuchElementException;

import javax.swing.KeyStroke;

import org.junit.Test;

import VASSAL.configure.PropertyExpression;

public class SequenceEncoderTest {
  
  /**
   * Test basic Sequence Encoder/Decoder functionality
   */
  
  @Test
  public void testEncodeDecodeBoolean() {
    final boolean VALUE = true;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextBoolean(false), VALUE);
  }

  @Test
  public void testEncodeDecodeInt() {
    final int VALUE = 42;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextInt(999), VALUE);
  }
  
  @Test
  public void testEncodeDecodeDouble() {
    final double VALUE = 3.1415926535;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(Double.toString(sd.nextDouble(99.9)), Double.toString(VALUE));
  }
  
  @Test
  public void testEncodeDecodeLong() {
    final long VALUE = 167772173;
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextLong(999), VALUE);
  }
  
  @Test
  public void testEncodeDecodeColor() {
    final Color VALUE = new Color(32, 145, 212);
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextColor(Color.red), VALUE);
  }

  @Test
  public void testEncodeDecodeKeyStroke() {
    final KeyStroke VALUE = KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_MASK);
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextKeyStroke('X'), VALUE);
  }

  @Test
  public void testEncodeDecodeNamedKeyStroke_1() {
    final NamedKeyStroke VALUE = new NamedKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_MASK));
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextNamedKeyStroke(), VALUE);
  }
  
  @Test
  public void testEncodeDecodeNamedKeyStroke_2() {
    final NamedKeyStroke VALUE = new NamedKeyStroke("#Control");
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextNamedKeyStroke(), VALUE);
  }
  
  @Test
  public void testEncodeDecodeString() {
    final String VALUE = "How many ,'s in this sentence?\n";
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(sd.nextToken(), VALUE);
  }
  
  @Test
  public void testEncodeDecodeStringArray() {
    final String[] VALUE = new String[] {"line 1", "line 2,", "line 3'", "line 4\n"};
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertArrayEquals(sd.nextStringArray(0), VALUE);
  }
  
  @Test
  public void testEncodeDecodePropertyExpression() {
    final PropertyExpression VALUE = new PropertyExpression("PieceName>=2");
    final SequenceEncoder se = new SequenceEncoder(',').append(VALUE);
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(se.getValue(), ',');
    assertEquals(new PropertyExpression(sd.nextToken()), VALUE);
  }
  
  @Test(expected=NoSuchElementException.class)
  public void testEncodeDecodeMulti() {
    
    final boolean booleanIn = true;
    final int intIn = 42;
    final double doubleIn = 3.1415926535; 
    final long longIn = 16777217;
    final Color colorIn = new Color(32, 145, 212);
    final KeyStroke keyStrokeIn = KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.CTRL_MASK);
    final NamedKeyStroke namedKeyStrokein1 = new NamedKeyStroke(keyStrokeIn);
    final NamedKeyStroke namedKeyStrokein2 = new NamedKeyStroke("#control");
    final String stringIn = "How many ,'s in this sentence?\n";
    final String[] stringArrayIn = new String[] {"line 1", "line 2,", "line 3'", "line 4\n"};
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
    
    assertTrue(
        booleanIn == sd.nextBoolean(false) &&
        intIn == sd.nextInt(999) &&
        Double.toString(doubleIn).equals(Double.toString(sd.nextDouble(99.9))) &&
        longIn == sd.nextLong(999) &&
        colorIn.equals(sd.nextColor(Color.red)) &&
        keyStrokeIn.equals(sd.nextKeyStroke('X')) &&
        namedKeyStrokein1.equals(sd.nextNamedKeyStroke()) &&
        namedKeyStrokein2.equals(sd.nextNamedKeyStroke()) &&
        stringIn.equals(sd.nextToken()) &&
        Arrays.equals(stringArrayIn, sd.nextStringArray(0)) &&
        propertyExpressionIn.equals(new PropertyExpression(sd.nextToken()))
     );
    
    sd.nextToken(); // Should be nothing left - should throw a NoSuchElementException
  }
}