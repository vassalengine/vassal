/*
 *
 * Copyright (c) 2011 by Bob Davison
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

import static org.junit.jupiter.api.Assertions.assertEquals;

import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.build.module.properties.PropertyChangerConfigurer;
import VASSAL.build.module.properties.PropertySetter;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.configure.DynamicKeyCommandListConfigurer;
import VASSAL.tools.NamedKeyStroke;
import java.awt.Component;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;


public class DynamicPropertyTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    DynamicProperty trait = new DynamicProperty();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    trait = new DynamicProperty ();
    trait.key = "xyzzy";
    trait.value = "plugh";
    trait.description = "Plover";
    serializeTest("Simple Property", trait); // NON-NLS

    trait = new DynamicProperty ();
    trait.key = "xyzzy";
    trait.value = "2";
    trait.numeric = true;
    trait.minValue = 5;
    trait.maxValue = 100;
    trait.wrap = true;
    trait.description = "Plover";
    serializeTest("Constraints", trait); // NON-NLS

    final DynamicProperty trait2 = new DynamicProperty ();
    trait2.key = "xyzzy";
    trait2.value = "plugh";
    trait2.description = "Plover";
    trait2.keyCommandListConfig = new DynamicKeyCommandListConfigurer(null, "Commands", trait2);
    BasicPiece piece = createBasicPiece ();
    trait2.setInner (piece);
    PropertyChanger changer = new PropertySetter ("3", new PropertyChangerConfigurer.Constraints () {
      @Override
      public boolean isWrap () {
        return false;
      }

      @Override
      public boolean isNumeric () {
        return false;
      }

      @Override
      public int getMaximumValue () {
        return 0;
      }

      @Override
      public int getMinimumValue () {
        return 0;
      }

      @Override
      public PropertySource getPropertySource () {
        return null;
      }

      @Override
      public Component getComponent () {
        return null;
      }

      @Override
      public Object getProperty (Object key) {
        return null;
      }

      @Override
      public Object getLocalizedProperty (Object key) {
        return null;
      }
    });
    DynamicProperty.DynamicKeyCommand command = new DynamicProperty.DynamicKeyCommand("test", new NamedKeyStroke ("plover"), piece, piece, changer);
    List<DynamicProperty.DynamicKeyCommand> commands = new ArrayList<> ();
    commands.add(command);
    trait2.keyCommands = commands.toArray(new DynamicProperty.DynamicKeyCommand[0]);
    trait2.keyCommandListConfig.getListValue().addAll(commands);
    serializeTest("Key Command", trait2); // NON-NLS
  }

  @Test
  public void testFormatInitialValueRFE3472() {
    // Initial value of DynamicProperty was not getting evaluated
    final String pieceName = "Test Piece";
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;" + pieceName + ";");
    final DynamicProperty dp = new DynamicProperty(DynamicProperty.ID, piece);

    dp.mySetState("$" + BasicPiece.PIECE_NAME + "$");

    assertEquals(pieceName, dp.getValue());
  }

  @Test
  public void testNonStringSetPropertyBug3479() {
    // If a dynamic property has a name of "Moved" then setProperty can be
    // called with a Boolean
    final String propName = "Moved";
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;");
    final DynamicProperty dp = new DynamicProperty(DynamicProperty.ID + propName, piece);

    dp.setProperty(propName, Boolean.TRUE);

    assertEquals(Boolean.TRUE.toString(), dp.getProperty(propName));
  }

  @Test
  public void testNullSetProperty() {
    // setProperty of null should result in getProperty of empty string
    final String propName = "Name";
    final BasicPiece piece = new BasicPiece(BasicPiece.ID + ";;;");
    final DynamicProperty dp = new DynamicProperty(DynamicProperty.ID + propName, piece);

    dp.setProperty(propName, null);

    assertEquals("", dp.getProperty(propName));
  }
}
