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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;

public class UsePrototypeTest extends DecoratorTest {


  @Test
  public void serializeTests() throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException {

    // Default piece
    UsePrototype trait = new UsePrototype();
    serializeTest("Default trait", trait); // NON-NLS

    // Set a command name and NamedKeyStroke
    trait = new UsePrototype("prototype;abc", null); // NON-NLS
    assertThat(trait.getPrototypeName(), is(equalTo("abc"))); // NON-NLS
    serializeTest("Named prototype", trait); // NON-NLS

  }
}
