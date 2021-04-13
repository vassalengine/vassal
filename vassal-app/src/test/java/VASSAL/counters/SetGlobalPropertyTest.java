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

import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.build.module.properties.PropertyChangerConfigurer;
import VASSAL.build.module.properties.PropertySetter;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.tools.NamedKeyStroke;
import java.awt.Component;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

public class SetGlobalPropertyTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    SetGlobalProperty trait = new SetGlobalProperty();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Complex trait
    trait = new SetGlobalProperty();
    trait.key = "propname";
    trait.description = "xyzzy";
    trait.propertyLevel = SetGlobalProperty.CURRENT_ZONE;
    trait.numeric = true;
    trait.minValue = 42;
    trait.maxValue = 84;
    trait.wrap = true;

    BasicPiece piece = createBasicPiece ();
    trait.setInner (piece);
    PropertyChanger changer = new PropertySetter("3", new PropertyChangerConfigurer.Constraints () {
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

    DynamicProperty.DynamicKeyCommand command = new DynamicProperty.DynamicKeyCommand("test", NamedKeyStroke.of("plover"), piece, piece, changer);
    List<DynamicProperty.DynamicKeyCommand> commands = new ArrayList<>();
    commands.add(command);
    trait.keyCommandListConfig.setValue (commands);
    trait.keyCommands = commands.toArray(new DynamicProperty.DynamicKeyCommand[0]);

    serializeTest("Complex trait", trait); // NON-NLS

  }
}
