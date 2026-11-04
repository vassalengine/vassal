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

import VASSAL.build.module.Map;
import VASSAL.tools.NamedKeyStroke;
import java.awt.event.KeyEvent;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

public class ReturnToDeckTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    ReturnToDeck trait = createTrait();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Set a Command and Named KeyStroke
    trait = createTrait();
    trait.returnCommand = "Plugh";
    trait.returnKey = NamedKeyStroke.of("xyzzy");
    trait.deckId = "aDeck";
    trait.selectDeckPrompt = "prompt";
    trait.description = "plover";
    serializeTest("Complex trait", trait); // NON-NLS

  }

  /**
   * Regression test for issue 14081: Return to Deck must be disabled on a
   * piece that is not on a map (i.e., in a palette), as Clone, Delete and
   * Send To Location are. Otherwise the palette's own piece is put into the
   * Deck, and every later piece from that palette shares its UniqueID.
   */
  @Test
  public void disabledWhenNotOnMap() {
    final ReturnToDeck trait = createCommandTrait();
    assertFalse(trait.myGetKeyCommands()[0].isEnabled());
    assertNull(trait.myKeyEvent(trait.returnKey.getKeyStroke()));
  }

  @Test
  public void enabledWhenOnMap() {
    final ReturnToDeck trait = createCommandTrait();
    trait.setMap(mock(Map.class));
    assertTrue(trait.myGetKeyCommands()[0].isEnabled());
  }

  private ReturnToDeck createTrait() {
    return new ReturnToDeck();
  }

  private ReturnToDeck createCommandTrait() {
    final ReturnToDeck trait = createTrait();
    trait.setInner(new BasicPiece());
    trait.returnCommand = "Return";
    trait.returnKey = NamedKeyStroke.of('R', KeyEvent.CTRL_DOWN_MASK);
    return trait;
  }
}
