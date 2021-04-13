
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

import VASSAL.tools.LoopControl;
import VASSAL.tools.NamedKeyStroke;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;

public class TriggerActionSerializeTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    TriggerAction trait = new TriggerAction();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Complex Trait
    trait = new TriggerAction();
    trait.command = "xyzzy";
    trait.key = NamedKeyStroke.of("key");
    trait.propertyMatch.setExpression("{abc==2}");
    trait.watchKeys = new NamedKeyStroke[] { NamedKeyStroke.of("key1"), NamedKeyStroke.of("key2") };
    trait.actionKeys = new NamedKeyStroke[] { NamedKeyStroke.of("key3"), NamedKeyStroke.of("key4") };
    trait.loop = true;
    trait.preLoopKey = NamedKeyStroke.of("pre");
    trait.postLoopKey = NamedKeyStroke.of("post");
    trait.loopType = LoopControl.LOOP_COUNTED;
    trait.whileExpression.setExpression("{def==3}");
    trait.untilExpression.setExpression("{ghi==4}");
    trait.loopCount.setFormat("5");
    trait.index = true;
    trait.indexProperty = "index";
    trait.indexStart.setFormat("6");
    trait.indexStep.setFormat("8");

    serializeTest("Complex trait", trait); // NON-NLS



  }
}
