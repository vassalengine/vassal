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

public class MenuSeparatorTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    MenuSeparator trait = new MenuSeparator();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new MenuSeparator();
    trait.desc = "plover";
    serializeTest("Description only", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new MenuSeparator();
    trait.key = NamedKeyStroke.of("xyzzy");
    serializeTest("Key Stroke only", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new MenuSeparator();
    trait.key = NamedKeyStroke.of("xyzzy");
    trait.desc = "plover";
    serializeTest("Description and Key Stroke", trait); // NON-NLS

  }
}
