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

public class ReportStateTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    ReportState trait = new ReportState();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    //
    trait = new ReportState();
    trait.keys = new NamedKeyStroke[] { NamedKeyStroke.of("key1"), NamedKeyStroke.of("key2") };
    trait.reportFormat = "xyzzy";
    trait.cycleDownKeys = new NamedKeyStroke[] { NamedKeyStroke.of("cycle1"), NamedKeyStroke.of("cycle2") };
    trait.cycleReportFormat = new String[] { "format1", "format2" };
    trait.description = "plover";

    serializeTest("Complext trait", trait); // NON-NLS

  }
}
