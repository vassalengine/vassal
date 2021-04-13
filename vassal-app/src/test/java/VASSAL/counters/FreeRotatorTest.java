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

public class FreeRotatorTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    FreeRotator trait = new FreeRotator();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Fixed rotation test
    trait = new FreeRotator();
    trait.validAngles = new double[] { 0, 90, 180, 270 };
    trait.rotateCWKey = NamedKeyStroke.of("rotateCWKey");
    trait.rotateCCWKey = NamedKeyStroke.of("rotateCCWKey");
    trait.rotateCWText = "rotateCW";
    trait.rotateCCWText = "rotateCCW";
    trait.rotateRNDKey = NamedKeyStroke.of("rotateRNDKey");
    trait.rotateRNDText = "rotateRND";
    trait.name = "xyzzy";
    trait.angleIndex = 2;
    trait.description = "plover";
    serializeTest("Fixed Rotations", trait); // NON-NLS

    // Free Rotation test
    trait = new FreeRotator();
    trait.validAngles = new double[] { 42.0 };
    trait.setAngleKey = NamedKeyStroke.of("setAngleKey");
    trait.setAngleText = "setAngle";
    trait.rotateRNDText = "rotateRND";
    trait.name = "xyzzy";
    trait.description = "plover";
    serializeTest("Fixed Rotations", trait); // NON-NLS

  }
}
