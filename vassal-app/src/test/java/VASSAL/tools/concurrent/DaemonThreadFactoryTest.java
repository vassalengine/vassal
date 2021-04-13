/*
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

package VASSAL.tools.concurrent;

import java.util.concurrent.ThreadFactory;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class DaemonThreadFactoryTest {
  @Test
  public void testNewThread() {
    final String basename = "foo";
    final ThreadFactory tf = new DaemonThreadFactory(basename);

    final Runnable r = new Runnable() {
      public void run() {}
    };

    final Thread t0 = tf.newThread(r);
    assertEquals(basename + "-0", t0.getName());
    assertTrue(t0.isDaemon());

    final Thread t1 = tf.newThread(r);
    assertEquals(basename + "-1", t1.getName());
    assertTrue(t1.isDaemon());
  }
}
