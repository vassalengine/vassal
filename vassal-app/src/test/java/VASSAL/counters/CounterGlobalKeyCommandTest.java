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

import VASSAL.configure.PropertyExpression;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.icon.IconFactory;
import java.lang.reflect.InvocationTargetException;
import javax.swing.ImageIcon;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class CounterGlobalKeyCommandTest extends DecoratorTest {

  @Test
  public void serialize() throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException {

    // Default piece
    CounterGlobalKeyCommand trait = new CounterGlobalKeyCommand();
    serializeTest("Default trait", trait); // NON-NLS

    trait = new CounterGlobalKeyCommand();
    trait.description = "abc";
    trait.key = NamedKeyStroke.of("xyzzy");
    trait.globalKey = NamedKeyStroke.of("plugh");
    trait.propertiesFilter = new PropertyExpression("{x==2}");
    trait.restrictRange = true;
    trait.range = 3;
    trait.rangeProperty = "test";
    trait.globalCommand.setReportSingle(true);
    trait.globalCommand.selectFromDeck = 3;
    serializeTest("Complex trait", trait);

    trait = new CounterGlobalKeyCommand();
    trait.description = "abc";
    trait.key = NamedKeyStroke.of("xyzzy");
    trait.globalKey = NamedKeyStroke.of("plugh");
    trait.propertiesFilter = new PropertyExpression("{x==2}");
    trait.restrictRange = true;
    trait.range = 3;
    trait.rangeProperty = "test";
    trait.globalCommand.setReportSingle(true);
    trait.globalCommand.selectFromDeck = 3;
    GlobalCommandTarget target = new GlobalCommandTarget(GlobalCommandTarget.GKCtype.COUNTER);
    target.setTargetType(GlobalCommandTarget.Target.MAP);
    target.setTargetMap("xyzzy");
    trait.target = target;
    serializeTest("Target", trait);
  }
}
