package VASSAL.counters;

import java.awt.Rectangle;

import VASSAL.tools.NamedKeyStroke;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ActionButtonSerializeTest extends SerializeTest<ActionButton> {
  @Test
  public void serialize() throws Exception {
    ActionButton button = new ActionButton();
    button.bounds = new Rectangle(1,2,3,4);
    button.description = "testDesc";
    button.stroke = NamedKeyStroke.of("A");
    super.serializeTest(ActionButton.class, button);
  }

  @Override
  void assertSame(ActionButton button1, ActionButton button2) {
    assertEquals(button1.bounds, button2.bounds);
    assertEquals(button1.stroke, button2.stroke);
    assertEquals(button1.description, button2.description);
  }
}
