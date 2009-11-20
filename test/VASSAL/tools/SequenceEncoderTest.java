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

import java.awt.Color;
import java.awt.event.KeyEvent;

import javax.swing.KeyStroke;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import VASSAL.configure.PropertyExpression;

public class SequenceEncoderTest {
  
  /**
   * Test basic Sequence Encoder/Decoder functionality
   */
  @Test
  public void testEncodeDecode() {
    
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
    
    assertEquals(booleanIn, sd.nextBoolean(false));
    assertEquals(intIn, sd.nextInt(999));
    assertEquals(Double.toString(doubleIn), Double.toString(sd.nextDouble(99.9)));
    assertEquals(longIn, sd.nextLong(999));
    assertEquals(colorIn, sd.nextColor(Color.red));
    assertEquals(keyStrokeIn, sd.nextKeyStroke('X'));
    assertTrue(namedKeyStrokein1.equals(sd.nextNamedKeyStroke()));
    assertTrue(namedKeyStrokein2.equals(sd.nextNamedKeyStroke()));
    assertEquals(stringIn, sd.nextToken());
    assertArrayEquals(stringArrayIn, sd.nextStringArray(0));
    assertEquals(propertyExpressionIn, new PropertyExpression(sd.nextToken()));
  }
}