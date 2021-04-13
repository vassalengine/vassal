package VASSAL.counters;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import VASSAL.tools.NamedKeyStroke;


public class DeleteSerializeTest extends SerializeTest<Delete> {
  @Test
  public void serialize() throws Exception {
    Delete delete = new Delete();
    delete.commandName = "testCommandName";
    delete.key = NamedKeyStroke.of("A");
    super.serializeTest(Delete.class, delete);
  }

  @Override
  void assertSame(Delete delete1, Delete delete2) {
    assertEquals(delete1.commandName, delete2.commandName);
    assertEquals(delete1.key, delete2.key);
  }
}
