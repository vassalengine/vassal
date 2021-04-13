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

import java.awt.Color;
import java.awt.Point;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.junit.jupiter.api.Test;

public class FootprintTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Footprint trait = new Footprint();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Complex test
    trait = new Footprint();
    trait.trailKey = NamedKeyStroke.of("testkey"); // NON-NLS
    trait.menuCommand = "xyzzy"; // NON-NLS
    trait.initiallyVisible = true;
    trait.globallyVisible = true;
    trait.circleRadius = 10;
    trait.fillColor = Color.blue;
    trait.lineColor = Color.cyan;
    trait.selectedTransparency = 42;
    trait.unSelectedTransparency = 84;
    trait.edgePointBuffer = 3;
    trait.edgeDisplayBuffer = 14;
    trait.lineWidth = 2;
    trait.trailKeyOn = NamedKeyStroke.of("keyOn"); // NON-NLS
    trait.trailKeyOff = NamedKeyStroke.of("keyOff"); // NON-NLS
    trait.trailKeyClear = NamedKeyStroke.of("keyClear"); // NON-NLS
    trait.description = "plover"; // NON-NLS

    trait.globalVisibility = true;
    trait.startMapId = "map1"; // NON-NLS
    trait.pointList = new ArrayList<>();
    trait.pointList.add(new Point(10, 20));
    trait.pointList.add(new Point(110, 120));
    trait.pointList.add(new Point(210, 220));

    serializeTest("Complex test", trait); // NON-NLS


  }

}
