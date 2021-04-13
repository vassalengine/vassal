/*
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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

package VASSAL.property;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PropertyTest {
  @Test
  public void testEqualsTrue() {
    final Property<Boolean> p1 = new Property<Boolean>("foo", Boolean.class);
    final Property<Boolean> p2 = new Property<Boolean>("foo", Boolean.class);
    final Property<Boolean> p3 = new Property<Boolean>("foo", Boolean.class,
                                                              Boolean.FALSE);

    assertEquals(p1, p1);
    assertEquals(p1, p2);
    assertEquals(p1, p3);
  }

  @Test
  public void testEqualsFalse() {
    final Property<Boolean> p1 = new Property<Boolean>("foo", Boolean.class);
    final Property<Integer> p2 = new Property<Integer>("foo", Integer.class);
    final Property<Boolean> p3 = new Property<Boolean>("bar", Boolean.class);

    assertFalse(p1.equals(null));
    assertFalse(p1.equals(Boolean.TRUE));
    assertFalse(p1.equals(p2));
    assertFalse(p1.equals(p3));
  }

  @Test
  public void testHashCodeEquals() {
    final Property<Boolean> p1 = new Property<Boolean>("foo", Boolean.class);
    final Property<Boolean> p2 = new Property<Boolean>("foo", Boolean.class);

    assertEquals(p1.hashCode(), p1.hashCode());
    assertEquals(p1.hashCode(), p2.hashCode());
  }
}
