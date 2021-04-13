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
import javax.swing.KeyStroke;
import org.junit.jupiter.api.Test;

public class TableInfoTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    TableInfo trait = new TableInfo();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Complex Trait
    trait = new TableInfo();
    trait.nCols = 2;
    trait.nRows = 3;
    trait.command = "xyzzy"; // NON-NLS
    trait.launchKey = NamedKeyStroke.of("plover"); // NON-NLS
    trait.values = "a,b,c,d,e,f"; // NON-NLS
    trait.description = "plugh";
    serializeTest("Complex trait", trait); // NON-NLS



  }
}
