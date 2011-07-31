/*
 * $Id$
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

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import java.awt.event.KeyEvent;
import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import VASSAL.build.module.Map;
import VASSAL.tools.NamedKeyStroke;

@RunWith(value = Parameterized.class)
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
  final static Map MAP = mock(Map.class);
  static TriggerAction trigger;

  String propertyExpression;
  Map map;
  boolean result;
  String message;

  public TriggerActionTest(String propertyExpression, Map map, boolean result, String message) {
    this.propertyExpression = propertyExpression;
    this.map = map;
    this.result = result;
    this.message = message;
  }

  @Parameters
  // Test Parameters:
  //  Property - Property Expression, either empty, or a known True or False expression
  //  Map - Either null, or a mocked Map object.
  //  Result - Expected Enabled state of the Trigger KeyCommand.
  //  Message - Test description/failure message
  public static Collection<Object[]> data() {
    return Arrays.asList(new Object[][] {
        // Property         Map   Result  Message
        { "",               null, false,  "No Match expression, Unit not on map, KeyCommand should be disabled"     },
        { "",               MAP,  true,   "No Match expression, Unit on map, KeyCommand should be enabled"  },
        { FALSE_EXPRESSION, null, false,  "Match expression false, Unit not on map, KeyCommand should be disabled" },
        { FALSE_EXPRESSION, MAP,  false,  "Match expression false, unit on map, KeyCommand should be disabled" },
        { TRUE_EXPRESSION,  null, false,  "Match expression true, Unit not on map, KeyCommand should be disabled" },
        { TRUE_EXPRESSION,  MAP,  true,   "Match expression true, Unit on map, KeyCommand should be enabled" }
    });
  }

  @Before
  // Create a standard Trigger
  public void setup() {
    trigger = new TriggerAction();
    trigger.setInner(new BasicPiece());
    trigger.setCommandName("Trigger");
    trigger.setKey(NamedKeyStroke.getNamedKeyStroke('T', KeyEvent.CTRL_MASK));
  }

  @Test
  // Set the property expression and map and check the enabled state of the generated KeyCommand.
  public void testBugRegression_2900930() {
    trigger.setPropertyMatch(propertyExpression);
    trigger.setMap(map);
    final boolean enabled = trigger.myGetKeyCommands()[0].isEnabled();
    assertEquals(message, result, enabled);
  }

}
