package VASSAL.counters;

import static org.junit.Assert.assertEquals;

import java.awt.Rectangle;

import org.junit.Test;

import VASSAL.tools.NamedKeyStroke;


public class ActionButtonSerializeTest extends SerializeTest<ActionButton> {
  @Test
  public void serialize() throws Exception {
    ActionButton button = new ActionButton();
    button.bounds = new Rectangle(1,2,3,4);
    button.description = "testDesc";
    button.stroke = new NamedKeyStroke("A");
    super.serializeTest(ActionButton.class, button);
  }

  @Override
  void assertSame(ActionButton button1, ActionButton button2) {
    assertEquals(button1.bounds, button2.bounds);
    assertEquals(button1.stroke, button2.stroke);
    assertEquals(button1.description, button2.description);
  }
}
