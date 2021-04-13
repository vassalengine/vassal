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
    trait.mySetType("nonRect;ntest.png,m,-3,-9,l,-3,-8,l,-4,-8,l,-4,-7,l,-5,-7,l,-5,-6,l,-6,-6,l,-6,-5,l,-7,-5,l,-7,-4,l,-8,-4,l,-8,-3,l,-8,-2,l,-8,-1,l,-8,0,l,-8,1,l,-8,2,l,-9,2,l,-9,3,l,-9,4,l,-9,5,l,-8,5,l,-8,6,l,-7,6,l,-7,7,l,-5,7,l,-5,8,l,-5,9,l,-2,9,l,-2,10,l,3,10,l,3,9,l,4,9,l,4,8,l,5,8,l,5,7,l,6,7,l,6,6,l,7,6,l,7,5,l,8,5,l,8,4,l,9,4,l,9,3,l,9,2,l,9,1,l,9,0,l,9,-1,l,9,-2,l,9,-3,l,9,-4,l,9,-5,l,7,-5,l,7,-6,l,6,-6,l,6,-7,l,4,-7,l,4,-8,l,2,-8,l,2,-9,c"); // NON-NLS
    serializeTest("Complex trait", trait); // NON-NLS

  }
}
