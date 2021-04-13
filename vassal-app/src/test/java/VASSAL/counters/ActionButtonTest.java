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
import java.awt.Rectangle;
import java.lang.reflect.InvocationTargetException;
import javax.swing.KeyStroke;

import org.junit.jupiter.api.Test;

public class ActionButtonTest extends DecoratorTest {


  @Test
  public void serializeTests() throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException {

    // Default piece
    ActionButton trait = new ActionButton();
    serializeTest("Default trait", trait); // NON-NLS

    // Set a command name and NamedKeyStroke
    trait = new ActionButton();
    trait.stroke = NamedKeyStroke.of("plugh"); // NON-NLS
    trait.description = "xyzzy"; // NON-NLS
    trait.bounds = new Rectangle(1, 2, 3, 4);
    serializeTest("Named KeyStroke", trait); // NON-NLS

    // Set a command name and KeyStroke
    trait = new ActionButton();
    trait.stroke = NamedKeyStroke.of(KeyStroke.getKeyStroke(65, 0));
    trait.description = "xyzzy"; // NON-NLS
    trait.bounds = new Rectangle(1, 2, 3, 4);
    serializeTest("KeyStroke", trait); // NON-NLS
  }
}
