/*
 * Copyright 2020 Vassal Development Team
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

package VASSAL.counters;

import VASSAL.tools.NamedKeyStroke;
import java.lang.reflect.InvocationTargetException;

import org.junit.jupiter.api.Test;

public class PivotTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Pivot trait = new Pivot();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Complex test
    trait = new Pivot();
    trait.key = NamedKeyStroke.of("xyzzy");
    trait.pivotX = 5;
    trait.pivotY = 12;
    trait.fixedAngle = true;
    trait.angle = 42.1;
    trait.description = "plover";
    serializeTest("Complex Test", trait); // NON-NLS


  }
}
