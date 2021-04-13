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

import VASSAL.script.expression.Expression;

import java.lang.reflect.InvocationTargetException;

import org.junit.jupiter.api.Test;

public class CalculatedPropertyTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    CalculatedProperty trait = new CalculatedProperty();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = new CalculatedProperty();
    trait.name = "testCommand"; // NON-NLS
    trait.expression = Expression.createExpression("{x * 2}"); // NON-NLS
    trait.description = "plover";
    serializeTest("Expression", trait); // NON-NLS

  }

  // Custom EditorTest. The CP Editor strips the {} from the Expression
  @Override
  public void editorTest(String test, Decorator referenceTrait) {

    // Save the original Type and State in case the Editor changes them
    final String originalType = referenceTrait.myGetType();
    final String originalState = referenceTrait.myGetState();

    // Create an instance of the Editor
    final PieceEditor editor = referenceTrait.getEditor();

    // Confirm that the Type and State encoded by the editor is the same as the original trait
    assertThat("Trait Edit Test (State): " + test, editor.getState(), is(equalTo(originalState))); // NON-NLS

    // The Type returned from the CP editor has the {} removed from the expression
    assertThat("Trait Edit Test (Type): " + test, editor.getType(), is(equalTo(originalType.replace("{", "").replace("}", "")))); // NON-NLS
  }

}
