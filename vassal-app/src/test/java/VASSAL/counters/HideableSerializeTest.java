package VASSAL.counters;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.awt.Color;

import org.junit.jupiter.api.Test;

import VASSAL.tools.NamedKeyStroke;


public class HideableSerializeTest extends SerializeTest<Hideable> {
  @Test
  public void serialize() throws Exception {
    Hideable hideable = new Hideable();
    hideable.hideKey = NamedKeyStroke.of("A");
    hideable.command = "testCommand";
    hideable.bgColor = Color.black;
    hideable.access = PlayerAccess.getInstance();
    super.serializeTest(Hideable.class, hideable);
  }

  @Override
  void assertSame(Hideable hideable1, Hideable hideable2) {
    assertEquals(hideable1.hideKey, hideable2.hideKey);
    assertEquals(hideable1.command, hideable2.command);
    assertEquals(hideable1.bgColor, hideable2.bgColor);
    assertEquals(hideable1.access, hideable2.access);
  }
}
