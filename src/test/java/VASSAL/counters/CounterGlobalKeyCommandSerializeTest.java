package VASSAL.counters;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import VASSAL.configure.PropertyExpression;
import VASSAL.tools.NamedKeyStroke;


public class CounterGlobalKeyCommandSerializeTest extends SerializeTest<CounterGlobalKeyCommand> {
  @Test
  public void serialize() throws Exception {
    CounterGlobalKeyCommand cgkc = new CounterGlobalKeyCommand();
    cgkc.commandName = "testCommandName";
    cgkc.key = new NamedKeyStroke("A");
    cgkc.globalKey = new NamedKeyStroke("B");
    cgkc.propertiesFilter = new PropertyExpression("foo=1");
    cgkc.restrictRange = true;
    cgkc.fixedRange = true;
    cgkc.range = 2;
    cgkc.rangeProperty = "testRangeProperty";
    cgkc.description = "testDesc";
      cgkc.globalCommand.setReportSingle(true);
      cgkc.globalCommand.setSelectFromDeck(3);
      cgkc.globalCommand.setKeyStroke(cgkc.globalKey);
    super.serializeTest(CounterGlobalKeyCommand.class, cgkc);
  }

  @Override
  void assertSame(CounterGlobalKeyCommand cgkc1, CounterGlobalKeyCommand cgkc2) {
    assertEquals(cgkc1.commandName, cgkc2.commandName);
    assertEquals(cgkc1.key, cgkc2.key);
    assertEquals(cgkc1.globalKey, cgkc2.globalKey);
    assertEquals(cgkc1.propertiesFilter, cgkc2.propertiesFilter);
    assertEquals(cgkc1.restrictRange, cgkc2.restrictRange);
    assertEquals(cgkc1.fixedRange, cgkc2.fixedRange);
    assertEquals(cgkc1.range, cgkc2.range);
    assertEquals(cgkc1.rangeProperty, cgkc2.rangeProperty);
    assertEquals(cgkc1.description, cgkc2.description);
    assertEquals(cgkc1.globalCommand, cgkc2.globalCommand);
  }
}
