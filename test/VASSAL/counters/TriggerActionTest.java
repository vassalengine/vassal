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

import org.junit.Test;

import VASSAL.build.module.Map;
import VASSAL.tools.NamedKeyStroke;

public class TriggerActionTest {
  
  /**
   * Regression test for Bug 2900930:  Trigger Actions not disabled when Property Expression fails
   * Test the enabled state of the returned Key Command, under various combinations of true/false
   * match expression and counter on a map or not. The Key Command should only be enabled if both the Match 
   * Expression is true and the counter is on a map.
   */
  @Test
  public void testBugRegression_2900930() {
    
    final String trueExpression = "xyzzy=a";
    final String falseExpression = "xyzzy=b";
    final Map map = new Map(false);
    
    final TriggerAction trigger = new TriggerAction();
    trigger.setInner(new BasicPiece());
    trigger.setCommandName("Trigger"); // Set a command name
    trigger.setKey(NamedKeyStroke.getNamedKeyStroke('T', KeyEvent.CTRL_MASK)); // Set a Trigger command
    trigger.setProperty("xyzzy", "a"); // Set a known property

    // No Match expression, not on map, should be disabled.
    trigger.setPropertyMatch("");
    trigger.setMap(null);    
    KeyCommand[] k = trigger.myGetKeyCommands();
    assertFalse(k[0].isEnabled());
    
    // No Match expression, on map, should be enabled.
    trigger.setPropertyMatch("");
    trigger.setMap(map);    
    k = trigger.myGetKeyCommands();
    assertTrue(k[0].isEnabled());
    
    // Match expression false, not on map, should be disabled.
    trigger.setPropertyMatch(falseExpression);  
    trigger.setMap(null);          
    k = trigger.myGetKeyCommands();
    assertFalse(k[0].isEnabled());
    
    // Match expression false, on map, should be disabled
    trigger.setPropertyMatch(falseExpression);
    trigger.setMap(map);        
    k = trigger.myGetKeyCommands();
    assertFalse(k[0].isEnabled());
    
    // Match expression true, not on map, should be disabled
    trigger.setPropertyMatch(trueExpression);
    trigger.setMap(null);
    k = trigger.myGetKeyCommands();
    assertFalse(k[0].isEnabled());
    
    // Match expression true, on map, should be enabled
    trigger.setPropertyMatch(trueExpression);
    trigger.setMap(map);
    k = trigger.myGetKeyCommands();
    assertTrue(k[0].isEnabled());
  }

}