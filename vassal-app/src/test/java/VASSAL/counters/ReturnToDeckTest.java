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

import static org.junit.jupiter.api.Assertions.assertEquals;
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
    final ReturnToDeck trait = createCommandTrait(new BasicPiece());
    assertFalse(command(trait).isEnabled());
    assertNull(trait.myKeyEvent(trait.returnKey.getKeyStroke()));
  }

  @Test
  public void enabledWhenOnMap() {
    final ReturnToDeck trait = createCommandTrait(new BasicPiece());
    trait.setMap(mock(Map.class));
    assertTrue(command(trait).isEnabled());
  }

  /** The case this change alters beyond the palette: an empty Mat with no map */
  @Test
  public void emptyMatDisabledWhenNotOnMap() {
    final ReturnToDeck trait = createCommandTrait(createMat());
    assertFalse(command(trait).isEnabled());
  }

  @Test
  public void emptyMatEnabledWhenOnMap() {
    final ReturnToDeck trait = createCommandTrait(createMat());
    trait.setMap(mock(Map.class));
    assertTrue(command(trait).isEnabled());
  }

  /** Unchanged by this fix: a Mat carrying cargo stays disabled on a map */
  @Test
  public void matWithCargoDisabledWhenOnMap() {
    final Mat mat = createMat();
    mat.addCargo(new Delete());
    final ReturnToDeck trait = createCommandTrait(mat);
    trait.setMap(mock(Map.class));
    assertFalse(command(trait).isEnabled());
  }

  private ReturnToDeck createTrait() {
    return new ReturnToDeck();
  }

  private ReturnToDeck createCommandTrait(GamePiece inner) {
    final ReturnToDeck trait = createTrait();
    trait.setInner(inner);
    trait.returnCommand = "Return";
    trait.returnKey = NamedKeyStroke.of('R', KeyEvent.CTRL_DOWN_MASK);
    return trait;
  }

  private Mat createMat() {
    final Mat mat = new Mat();
    mat.setInner(new BasicPiece());
    return mat;
  }

  private KeyCommand command(ReturnToDeck trait) {
    final KeyCommand[] commands = trait.myGetKeyCommands();
    assertEquals(1, commands.length);
    return commands[0];
  }
}
