package VASSAL.counters;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import VASSAL.tools.NamedKeyStroke;


public class GlobalHotKeySerializeTest extends SerializeTest<GlobalHotKey> {
  @Test
  public void serialize() throws Exception {
    GlobalHotKey ghk = new GlobalHotKey();
    ghk.commandName = "testCommandName";
    ghk.commandKey = new NamedKeyStroke("A");
    ghk.globalHotKey = new NamedKeyStroke("B");
    ghk.description = "testDesc";

    super.serializeTest(GlobalHotKey.class, ghk);
  }

  @Override
  void assertSame(GlobalHotKey ghk1, GlobalHotKey ghk2) {
    assertEquals(ghk1.commandName, ghk2.commandName);
    assertEquals(ghk1.commandKey, ghk2.commandKey);
    assertEquals(ghk1.globalHotKey, ghk2.globalHotKey);
    assertEquals(ghk1.description, ghk2.description);
  }
}
