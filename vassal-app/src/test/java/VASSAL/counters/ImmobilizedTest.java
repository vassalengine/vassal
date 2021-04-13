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

public class ImmobilizedTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Immobilized trait = new Immobilized();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new Immobilized();
    trait.shiftToSelect = true;
    trait.altToSelect = false;
    trait.neverSelect = false;
    trait.ignoreGrid = false;
    trait.neverMove = true;
    trait.moveIfSelected = false;
    trait.neverBandSelect = true;
    trait.altToBandSelect = false;
    serializeTest("Test 1", trait); // NON-NLS

    trait = new Immobilized();
    trait.shiftToSelect = false;
    trait.altToSelect = false;
    trait.neverSelect = true;
    trait.ignoreGrid = true;
    trait.neverMove = false;
    trait.moveIfSelected = true;
    trait.neverBandSelect = false;
    trait.altToBandSelect = true;
    serializeTest("Test 2", trait); // NON-NLS

    trait = new Immobilized();
    trait.shiftToSelect = false;
    trait.altToSelect = true;
    trait.neverSelect = false;
    trait.ignoreGrid = true;
    trait.neverMove = false;
    trait.moveIfSelected = true;
    trait.neverBandSelect = false;
    trait.altToBandSelect = true;
    serializeTest("Test 3", trait); // NON-NLS
  }
}
