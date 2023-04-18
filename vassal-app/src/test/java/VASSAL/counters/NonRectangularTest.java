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

import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;

public class NonRectangularTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    NonRectangular trait = new NonRectangular();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new NonRectangular();
    trait.mySetType("nonRect2;1.0;dM -3.0 -9.0 L -3.0 -8.0 L -4.0 -8.0 L -4.0 -7.0 L -5.0 -7.0 L -5.0 -6.0 L -6.0 -6.0 L -6.0 -5.0 L -7.0 -5.0 L -7.0 -4.0 L -8.0 -4.0 L -8.0 -3.0 L -8.0 -2.0 L -8.0 -1.0 L -8.0 0.0 L -8.0 1.0 L -8.0 2.0 L -9.0 2.0 L -9.0 3.0 L -9.0 4.0 L -9.0 5.0 L -8.0 5.0 L -8.0 6.0 L -7.0 6.0 L -7.0 7.0 L -5.0 7.0 L -5.0 8.0 L -5.0 9.0 L -2.0 9.0 L -2.0 1.0 L 3.0 1.0 L 3.0 9.0 L 4.0 9.0 L 4.0 8.0 L 5.0 8.0 L 5.0 7.0 L 6.0 7.0 L 6.0 6.0 L 7.0 6.0 L 7.0 5.0 L 8.0 5.0 L 8.0 4.0 L 9.0 4.0 L 9.0 3.0 L 9.0 2.0 L 9.0 1.0 L 9.0 0.0 L 9.0 -1.0 L 9.0 -2.0 L 9.0 -3.0 L 9.0 -4.0 L 9.0 -5.0 L 7.0 -5.0 L 7.0 -6.0 L 6.0 -6.0 L 6.0 -7.0 L 4.0 -7.0 L 4.0 -8.0 L 2.0 -8.0 L 2.0 -9.0 Z N test.png"); // NON-NLS
    serializeTest("Complex trait", trait); // NON-NLS

  }
}
