/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.awt.event.InputEvent;

import VASSAL.build.GameModule;
import VASSAL.build.MockModuleTest;
import VASSAL.build.module.properties.EnumeratedPropertyPrompt;
import VASSAL.build.module.properties.IncrementProperty;
import VASSAL.build.module.properties.PropertyPrompt;
import VASSAL.build.module.properties.PropertySetter;
import VASSAL.configure.DynamicKeyCommandListConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * Traits built from the same type string share their parsed type data;
 * their state stays their own; and what they write back is unchanged.
 */
public class TraitTypeCacheTest extends MockModuleTest {
  private TraitTypeCache cache;

  /** The mocked module hands out this test's own cache, as a real module hands out its own. */
  @BeforeEach
  public void giveTheModuleACache() {
    cache = new TraitTypeCache();
    when(GameModule.getGameModule().getTraitTypeCache()).thenReturn(cache);
  }

  private static final String MARKER = "mark;Nation,Type,Size"; // NON-NLS

  /** A Trigger Action's type string, built the way the editor builds it. */
  private static String triggerType(String command) {
    final TriggerAction t = new TriggerAction();
    t.name = "Fire"; // NON-NLS
    t.command = command;
    t.key = NamedKeyStroke.of('F', InputEvent.CTRL_DOWN_MASK);
    t.propertyMatch = new PropertyExpression("{Ammo > 0}"); // NON-NLS
    t.watchKeys = new NamedKeyStroke[] {NamedKeyStroke.of('P', InputEvent.CTRL_DOWN_MASK), NamedKeyStroke.of("named")}; // NON-NLS
    t.actionKeys = new NamedKeyStroke[] {NamedKeyStroke.of('R', InputEvent.CTRL_DOWN_MASK)};
    t.loop = true;
    t.loopCount = new FormattedString("3"); // NON-NLS
    return t.myGetType();
  }

  private static String restrictType() {
    final RestrictCommands r = new RestrictCommands();
    r.name = "Locked"; // NON-NLS
    r.propertyMatch = new PropertyExpression("{Locked==true}"); // NON-NLS
    r.watchKeys = new NamedKeyStroke[] {NamedKeyStroke.of('F', InputEvent.CTRL_DOWN_MASK), NamedKeyStroke.of('G', InputEvent.CTRL_DOWN_MASK)};
    return r.myGetType();
  }

  /** A two-level Layer with no images, so that no image is looked up. */
  private static String layerType() {
    final Embellishment e = new Embellishment();
    e.imageName = new String[] {"", ""};
    e.commonName = new String[] {"Front", "Back"}; // NON-NLS
    e.name = "Status"; // NON-NLS
    e.resetLevel = new FormattedString("1");
    return e.myGetType();
  }

  /** A Dynamic Property with four key commands, one of each kind of change. */
  private static String dynamicType(DynamicProperty target) {
    final DynamicProperty.DynamicKeyCommand[] cmds = {
      new DynamicProperty.DynamicKeyCommand("Increase", NamedKeyStroke.of('I', InputEvent.CTRL_DOWN_MASK), target, target, new IncrementProperty(null, "1", target)), // NON-NLS
      new DynamicProperty.DynamicKeyCommand("Set", NamedKeyStroke.of('S', InputEvent.CTRL_DOWN_MASK), target, target, new PropertySetter("5", target)), // NON-NLS
      new DynamicProperty.DynamicKeyCommand("Ask", NamedKeyStroke.of("ask"), target, target, new PropertyPrompt(target, "Strength?")), // NON-NLS
      new DynamicProperty.DynamicKeyCommand("Pick", NamedKeyStroke.of('P', InputEvent.CTRL_DOWN_MASK), target, target, new EnumeratedPropertyPrompt(target, "Choose", new String[] {"a", "b,c"}, target)) // NON-NLS
    };
    final String list = DynamicProperty.encodeKeyCommands(cmds);
    return DynamicProperty.ID + new SequenceEncoder(';').append("Strength").append("true,0,10,false").append(list).append("Strength of the unit").getValue(); // NON-NLS
  }

  /** The arrays are each instance's own (a subclass may write into them); their elements are shared. */
  private static void assertSharedElements(Object[] a, Object[] b) {
    assertNotSame(a, b);
    assertEquals(a.length, b.length);
    for (int i = 0; i < a.length; i++) {
      assertSame(a[i], b[i]);
    }
  }

  @Test
  public void triggerActionsOfOneTypeShareTheirParsedType() {
    final String TRIGGER = triggerType("Fire"); // NON-NLS
    final TriggerAction a = new TriggerAction(TRIGGER, new BasicPiece());
    final TriggerAction b = new TriggerAction(TRIGGER, new BasicPiece());
    assertSharedElements(a.watchKeys, b.watchKeys);
    assertSharedElements(a.actionKeys, b.actionKeys);
    assertSame(a.propertyMatch, b.propertyMatch);
    assertSame(a.whileExpression, b.whileExpression);
    assertSame(a.loopCount, b.loopCount);
    assertSame(a.indexStep, b.indexStep);
    assertEquals(TRIGGER, a.myGetType());
    assertEquals(TRIGGER, b.myGetType());
    assertNotSame(a.myGetKeyCommands(), b.myGetKeyCommands());

    // Changing one trait's match expression must not reach the other.
    a.setPropertyMatch("{Ammo > 5}"); // NON-NLS
    assertNotSame(a.propertyMatch, b.propertyMatch);
    assertEquals("{Ammo > 0}", b.propertyMatch.getExpression()); // NON-NLS

    final TriggerAction c = new TriggerAction(triggerType("Shoot"), new BasicPiece()); // NON-NLS
    assertNotSame(a.watchKeys, c.watchKeys);
  }

  @Test
  public void restrictCommandsShareTheirParsedType() {
    final String RESTRICT = restrictType();
    final RestrictCommands a = new RestrictCommands(RESTRICT, new BasicPiece());
    final RestrictCommands b = new RestrictCommands(RESTRICT, new BasicPiece());
    assertSharedElements(a.watchKeys, b.watchKeys);
    assertSame(a.propertyMatch, b.propertyMatch);
    assertEquals(RESTRICT, b.myGetType());
  }

  @Test
  public void markersShareKeysButNotValues() {
    final Marker a = new Marker(MARKER, new BasicPiece());
    final Marker b = new Marker(MARKER, new BasicPiece());
    assertSame(a.keys, b.keys);
    assertNotSame(a.values, b.values);
    a.mySetState("GE,Inf,XX"); // NON-NLS
    b.mySetState("US,Arm,X"); // NON-NLS
    assertEquals("GE", a.getProperty("Nation")); // NON-NLS
    assertEquals("US", b.getProperty("Nation")); // NON-NLS
    a.setProperty("Type", "Mech"); // NON-NLS
    assertEquals("Arm", b.getProperty("Type")); // NON-NLS
    assertEquals(MARKER, a.myGetType());
    assertEquals("GE,Mech,XX", a.myGetState()); // NON-NLS
  }

  @Test
  public void layersShareImagesAndPaintersButNotTheirLevel() {
    final String LAYER = layerType();
    final Embellishment a = new Embellishment(LAYER, new BasicPiece());
    final Embellishment b = new Embellishment(LAYER, new BasicPiece());
    assertSharedElements(a.imageName, b.imageName);
    assertSharedElements(a.commonName, b.commonName);
    assertSharedElements(a.imagePainter, b.imagePainter);
    assertNotSame(a.size, b.size); // bounds are handed out to callers, so each instance has its own
    assertSame(a.resetLevel, b.resetLevel);
    assertEquals(LAYER, a.myGetType());
    a.mySetState("2"); // NON-NLS
    b.mySetState("1"); // NON-NLS
    assertEquals("2", a.myGetState()); // NON-NLS
    assertEquals("1", b.myGetState()); // NON-NLS
  }

  @Test
  public void dynamicPropertyKeyCommandsRoundTripWithoutAConfigurer() {
    final DynamicProperty seed = new DynamicProperty(DynamicProperty.ID + "Strength;true,0,10,false;;", new BasicPiece()); // NON-NLS
    final String DYNAMIC = dynamicType(seed);

    final DynamicProperty p = new DynamicProperty(DYNAMIC, new BasicPiece());
    assertEquals(DYNAMIC, p.myGetType());
    assertEquals(4, p.keyCommands.length);
    assertEquals("Increase", p.keyCommands[0].getName()); // NON-NLS
    assertEquals(NamedKeyStroke.of('I', InputEvent.CTRL_DOWN_MASK), p.keyCommands[0].getNamedKeyStroke());
    assertSame(p, p.keyCommands[0].getTarget());
    assertEquals("b,c", ((EnumeratedPropertyPrompt) p.keyCommands[3].getPropChanger()).getValidValues()[1]); // NON-NLS

    // The encoding is exactly what the editor's configurer writes for the same list.
    final String list = DynamicProperty.encodeKeyCommands(p.keyCommands);
    final DynamicKeyCommandListConfigurer viaConfigurer = new DynamicKeyCommandListConfigurer(null, "", p);
    viaConfigurer.setValue(list);
    assertEquals(viaConfigurer.getValueString(), list);
    assertEquals("", DynamicProperty.encodeKeyCommands(DynamicProperty.decodeKeyCommands("", p)));

    // Two instances have their own commands (they name their piece) and their own value.
    final DynamicProperty q = new DynamicProperty(DYNAMIC, new BasicPiece());
    assertNotSame(p.keyCommands, q.keyCommands);
    p.mySetState("7"); // NON-NLS
    q.mySetState("2"); // NON-NLS
    assertEquals("7", p.myGetState()); // NON-NLS
    assertEquals("2", q.myGetState()); // NON-NLS
  }

  @Test
  public void cacheIsPerDataClassAndClearable() {
    assertEquals(0, cache.size(String[].class));
    new Marker(MARKER, new BasicPiece());
    new Marker(MARKER, new BasicPiece());
    new Marker("mark;Other", new BasicPiece()); // NON-NLS
    assertEquals(2, cache.size(String[].class));
    cache.clear();
    assertEquals(0, cache.size(String[].class));
  }

  @Test
  public void withoutAModuleCacheTraitsStillParse() {
    when(GameModule.getGameModule().getTraitTypeCache()).thenReturn(null);
    final Marker a = new Marker(MARKER, new BasicPiece());
    final Marker b = new Marker(MARKER, new BasicPiece());
    assertEquals(MARKER, a.myGetType());
    assertNotSame(a.keys, b.keys);
  }
}
