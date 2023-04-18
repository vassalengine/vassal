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

import VASSAL.tools.NamedKeyStroke;
import java.awt.Color;
import java.lang.reflect.InvocationTargetException;
import javax.swing.KeyStroke;
import org.junit.jupiter.api.Test;

public class AreaOfEffectTest extends DecoratorTest {

  @Test
  void serializeTests() throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException {

    // Default piece
    AreaOfEffect trait = new AreaOfEffect();
    serializeTest("Default trait", trait); // NON-NLS

    // Always active, no map shader, fixed radius
    trait = new AreaOfEffect();
    trait.transparencyColor = Color.CYAN;
    trait.transparencyLevel = 42;
    trait.radius = 3;
    trait.alwaysActive = true;
    trait.fixedRadius = true;
    trait.description = "plover";
    serializeTest("Always active, no map shader, fixed radius", trait); // NON-NLS

    // Always active, map shader, variable radius
    trait = new AreaOfEffect();
    trait.transparencyColor = Color.CYAN;
    trait.transparencyLevel = 42;
    trait.mapShaderName = "plugh"; // NON-NLS
    trait.radius = 3;
    trait.alwaysActive = true;
    trait.fixedRadius = false;
    trait.radiusMarker = "xyzzy"; // NON-NLS
    trait.description = "plover";
    serializeTest("Always active, map shader, variable radius", trait); // NON-NLS

    // NameKeyStroke, no map shader, fixed radius
    trait = new AreaOfEffect();
    trait.transparencyColor = Color.CYAN;
    trait.transparencyLevel = 42;
    trait.radius = 3;
    trait.alwaysActive = false;
    trait.activateCommand = "Activate"; // NON-NLS
    trait.activateKey = NamedKeyStroke.of("xyzzy"); // NON-NLS
    trait.fixedRadius = true;
    trait.description = "plover";
    serializeTest("NamedKeystroke, no map shader, fixed radius", trait); // NON-NLS

    // KeyStroke, no map shader, fixed radius
    trait = new AreaOfEffect();
    trait.transparencyColor = Color.CYAN;
    trait.transparencyLevel = 42;
    trait.radius = 3;
    trait.alwaysActive = false;
    trait.activateCommand = "Activate"; // NON-NLS
    trait.activateKey = NamedKeyStroke.of(KeyStroke.getKeyStroke(65, 0));
    trait.fixedRadius = true;
    trait.description = "plover";
    serializeTest("KeyStroke, no map shader, fixed radius", trait); // NON-NLS

    // Test new Name and on/off key fields
    trait = new AreaOfEffect();
    trait.transparencyColor = Color.CYAN;
    trait.transparencyLevel = 42;
    trait.radius = 3;
    trait.alwaysActive = false;
    trait.activateCommand = "Activate"; // NON-NLS
    trait.activateKey = NamedKeyStroke.of(KeyStroke.getKeyStroke(65, 0));
    trait.fixedRadius = true;
    trait.description = "plover";
    trait.name = "xyzzy";
    trait.onMenuText = "plugh";
    trait.onKey = NamedKeyStroke.of("turn on");
    trait.onMenuText = "off";
    trait.onKey = NamedKeyStroke.of("turn off");
    serializeTest("Name and on/off keys", trait); // NON-NLS
  }

  // Test that the <name>_Active property returns the correct value and reflects action of the On/Off/Toggle keys.
  // And works for both local and global visibility
  @Test
  void propertyTests() {
    propertyTests(false);
    propertyTests(true);
  }

  void propertyTests(boolean global) {
    final String testType = (global ? "Global" : "Local") + " Visibility: ";
    AreaOfEffect trait = new AreaOfEffect();
    final NamedKeyStroke toggleKey = NamedKeyStroke.of("Toggle");
    final NamedKeyStroke onKey = NamedKeyStroke.of("TurnOn");
    final NamedKeyStroke offKey = NamedKeyStroke.of("TurnOff");

    trait.transparencyColor = Color.CYAN;
    trait.transparencyLevel = 42;
    trait.radius = 3;
    trait.globallyVisible = global;
    trait.alwaysActive = false;
    trait.activateCommand = "Activate"; // NON-NLS
    trait.activateKey = toggleKey;
    trait.onKey = onKey;
    trait.offKey = offKey;
    trait.fixedRadius = true;
    trait.name = "TEST";
    trait.mySetType(trait.myGetType());
    trait.setInner(new DummyPiece());

    final String prop = trait.name + AreaOfEffect.ACTIVE;
    assertThat(testType + "Active property starts false", trait.getProperty(prop), is(equalTo("false")));

    trait.myKeyEvent(toggleKey.getKeyStroke());
    assertThat(testType + "Subsequent Toggle activates trait", trait.getProperty(prop), is(equalTo("true")));

    trait.myKeyEvent(toggleKey.getKeyStroke());
    assertThat(testType + "Subsequent Toggle de-activates trait", trait.getProperty(prop), is(equalTo("false")));

    trait.myKeyEvent(onKey.getKeyStroke());
    assertThat(testType + "On when Off activates trait", trait.getProperty(prop), is(equalTo("true")));

    trait.myKeyEvent(onKey.getKeyStroke());
    assertThat(testType + "On when On leaves trait activated", trait.getProperty(prop), is(equalTo("true")));

    trait.myKeyEvent(offKey.getKeyStroke());
    assertThat(testType + "Off when On deactivates trait", trait.getProperty(prop), is(equalTo("false")));

    trait.myKeyEvent(onKey.getKeyStroke());
    assertThat(testType + "Off when Off leaves trait deactivated", trait.getProperty(prop), is(equalTo("true")));

    trait = new AreaOfEffect();
    trait.transparencyColor = Color.CYAN;
    trait.transparencyLevel = 42;
    trait.radius = 3;
    trait.globallyVisible = global;
    trait.alwaysActive = true;
    trait.fixedRadius = true;
    trait.description = "plover";
    trait.name = "TEST";
    trait.mySetType(trait.myGetType());
    trait.setInner(new DummyPiece());
    assertThat(testType + "Active property true when always active", trait.getProperty(prop), is(equalTo("true")));
  }

  class DummyPiece extends BasicPiece {
    @Override
    public Object getPublicProperty(Object key) {
      return null;
    }
  }

}
