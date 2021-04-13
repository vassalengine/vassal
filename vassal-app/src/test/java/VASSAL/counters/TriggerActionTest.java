/*
 *
 * Copyright (c) 2009 by Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.counters;

import java.awt.event.KeyEvent;
import java.util.stream.Stream;

import VASSAL.build.module.Map;
import VASSAL.tools.NamedKeyStroke;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

public class TriggerActionTest {

  /**
   * Regression test for Bug 2900930: Trigger Actions not disabled when Property
   * Expression fails. Test the enabled state of the returned Key Command, under
   * various combinations of true/false match expression and counter on a map or
   * not. The KeyCommand should only be enabled if both the Match Expression is
   * true (or empty) and the counter is on a map.
   */

  final static String TRUE_EXPRESSION = "BasicName=";
  final static String FALSE_EXPRESSION = "BasicName=xyzzy";
  final static String TRUE_BSH_EXPRESSION = "{BasicName==\"\"}";
  final static String FALSE_BSH_EXPRESSION = "{BasicName==\"xyzzy\"}";
  final static Map MAP = mock(Map.class);
  static TriggerAction trigger;

  private static Stream<Arguments> addFixture() {
    return Stream.of(
      Arguments.of("", null, false, "No Match expression, Unit not on map, KeyCommand should be disabled"),
      Arguments.of("", MAP, true, "No Match expression, Unit on map, KeyCommand should be enabled"),
      Arguments.of(FALSE_EXPRESSION, null, false, "Classic Match expression false, Unit not on map, KeyCommand should be disabled"),
      Arguments.of(FALSE_EXPRESSION, MAP, false, "Classic Match expression false, unit on map, KeyCommand should be disabled"),
      Arguments.of(TRUE_EXPRESSION, null, false, "Classic Match expression true, Unit not on map, KeyCommand should be disabled"),
      Arguments.of(TRUE_EXPRESSION, MAP, true, "Classic Match expression true, Unit on map, KeyCommand should be enabled"),
      Arguments.of(FALSE_BSH_EXPRESSION, null, false, "Beanshell Match expression false, Unit not on map, KeyCommand should be disabled"),
      Arguments.of(FALSE_BSH_EXPRESSION, MAP,  false, "Beanshell Match expression false, unit on map, KeyCommand should be disabled"),
      Arguments.of(TRUE_BSH_EXPRESSION, null, false, "Beanshell Match expression true, Unit not on map, KeyCommand should be disabled"),
      Arguments.of(TRUE_BSH_EXPRESSION, MAP, true, "Beanshell Match expression true, Unit on map, KeyCommand should be enabled"));
  }

  /**
   * Creates a standard Trigger
   */
  @BeforeEach
  public void setup() {
    trigger = new TriggerAction();
    trigger.setInner(new BasicPiece());
    trigger.setCommandName("Trigger");
    trigger.name = "Plover";
    trigger.setKey(NamedKeyStroke.of('T', KeyEvent.CTRL_DOWN_MASK));
  }

  /**
   * Sets the property expression and map and checks the enabled state of the generated KeyCommand.
   */
  @ParameterizedTest
  @MethodSource("addFixture")
  public void testBugRegression_2900930(String propertyExpression, Map map, boolean result, String message) {
    trigger.setPropertyMatch(propertyExpression);
    trigger.setMap(map);
    final boolean enabled = trigger.myGetKeyCommands()[0].isEnabled();
    assertEquals(result, enabled, message);
  }

}
