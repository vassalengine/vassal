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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.awt.event.KeyEvent;

import org.junit.Before;
import org.junit.Test;

import VASSAL.build.module.Map;
import VASSAL.tools.NamedKeyStroke;

public class TriggerActionTest {

  /**
   * Regression test for Bug 2900930: Trigger Actions not disabled when Property
   * Expression fails Test the enabled state of the returned Key Command, under
   * various combinations of true/false match expression and counter on a map or
   * not. The Key Command should only be enabled if both the Match Expression is
   * true and the counter is on a map.
   */

  final static String TRUE_EXPRESSION = "BasicName=";
  final static String FALSE_EXPRESSION = "BasicName=xyzzy";
  final static Map map = new Map(false);
  static TriggerAction trigger;

  @Before
  public void setup() {
    trigger = new TriggerAction();
    trigger.setInner(new BasicPiece());
    trigger.setCommandName("Trigger");
    trigger.setKey(NamedKeyStroke.getNamedKeyStroke('T', KeyEvent.CTRL_MASK));
  }  
  
  @Test
  public void testBugRegression_2900930_1() {

    trigger.setPropertyMatch("");
    trigger.setMap(null);
    KeyCommand[] k = trigger.myGetKeyCommands();
    assertFalse("No Match expression, Unit not on map, KeyCommand should be disabled", k[0].isEnabled());
  }

  @Test
  public void testBugRegression_2900930_2() {

    trigger.setPropertyMatch("");
    trigger.setMap(map);
    KeyCommand[] k = trigger.myGetKeyCommands();
    assertTrue("No Match expression, Unit on map, KeyCommand should be enabled", k[0].isEnabled());
  }

  @Test
  public void testBugRegression_2900930_3() {

    trigger.setPropertyMatch(FALSE_EXPRESSION);
    trigger.setMap(null);
    KeyCommand[] k = trigger.myGetKeyCommands();
    assertFalse("Match expression false, Unit not on map, KeyCommand should be disabled", k[0].isEnabled());
  }

  @Test
  public void testBugRegression_2900930_4() {

    trigger.setPropertyMatch(FALSE_EXPRESSION);
    trigger.setMap(map);
    KeyCommand[] k = trigger.myGetKeyCommands();
    assertFalse("Match expression false, unit on map, KeyCommand should be disabled", k[0].isEnabled());
  }

  @Test
  public void testBugRegression_2900930_5() {

    trigger.setPropertyMatch(TRUE_EXPRESSION);
    trigger.setMap(null);
    KeyCommand[] k = trigger.myGetKeyCommands();
    assertFalse("Match expression true, Unit not on map, KeyCommand should be disabled", k[0].isEnabled());
  }

  @Test
  public void testBugRegression_2900930_6() {

    trigger.setPropertyMatch(TRUE_EXPRESSION);
    trigger.setMap(map);
    KeyCommand[] k = trigger.myGetKeyCommands();
    assertTrue("Match expression true, Unit on map, KeyCommand should be enabled", k[0].isEnabled());
  }
}
