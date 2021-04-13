/*
 * $Id: SimpleFutureTest.java 7250 2010-09-20 22:51:28Z uckelman $
 *
 * Copyright (c) 2010 by Joel Uckelman
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

package VASSAL.tools.concurrent.listener;

import VASSAL.tools.lang.Pair;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class EventAccumulatorTest {
  @Test
  public void testEvents() {
    final EventAccumulator<Integer> ea = new EventAccumulator<Integer>();
    for (int i = 0; i < 10; ++i) {
      ea.receive(this, i);
    }

    int i = 0;
    for (Pair<Object,Integer> e : ea.events()) {
      assertSame(this, e.first);
      assertEquals(i, e.second.intValue());
      ++i;
    }
  }
}
