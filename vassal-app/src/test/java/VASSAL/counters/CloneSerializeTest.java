package VASSAL.counters;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import VASSAL.tools.NamedKeyStroke;


public class CloneSerializeTest extends SerializeTest<Clone> {
  @Test
  public void serialize() throws Exception {
    Clone clone = new Clone();
    clone.commandName = "testCommand";
    clone.key = NamedKeyStroke.of("A");
    super.serializeTest(Clone.class, clone);
  }

  @Override
  void assertSame(Clone clone1, Clone clone2) {
    assertEquals(clone1.commandName, clone2.commandName);
    assertEquals(clone1.key, clone2.key);
  }
}
