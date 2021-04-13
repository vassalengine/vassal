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
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import org.junit.jupiter.api.Test;

public class HideableTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    Hideable trait = new Hideable();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new Hideable();
    trait.command = "testCommand"; // NON-NLS
    trait.hideKey = NamedKeyStroke.of("xyzzy"); // NON-NLS
    trait.bgColor = Color.blue;
    trait.transparency = 0.42f;
    trait.access = new PlayerAccess();

    trait.hiddenBy = "me";
    trait.description = "plover";
    serializeTest("Player Access", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new Hideable();
    trait.command = "testCommand"; // NON-NLS
    trait.hideKey = NamedKeyStroke.of("xyzzy"); // NON-NLS
    trait.bgColor = Color.blue;
    trait.transparency = 0.42f;
    trait.access = new SideAccess();

    trait.hiddenBy = "me";
    trait.description = "plover";
    serializeTest("Side Access", trait); // NON-NLS

    trait = new Hideable();
    trait.command = "testCommand"; // NON-NLS
    trait.hideKey = NamedKeyStroke.of("xyzzy"); // NON-NLS
    trait.bgColor = Color.blue;
    trait.transparency = 0.42f;
    trait.access = new SpecifiedSideAccess(Arrays.asList("A", "B"));

    trait.hiddenBy = "me";
    trait.description = "plover";
    serializeTest("SpecifiedSide Access", trait); // NON-NLS
  }
}
