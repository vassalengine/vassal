package VASSAL.counters;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import VASSAL.configure.ListConfigurerHelper;
import VASSAL.counters.DynamicProperty.DynamicKeyCommand;
import VASSAL.counters.DynamicProperty.DynamicKeyCommandConfigurer;

public class DynamicPropertySerializeTest extends
    SerializeTest<DynamicProperty> {
  @Test
  public void serialize() throws Exception {
    DynamicProperty dp = new DynamicProperty();
    dp.key = "testKey";
    dp.numeric = false;
    dp.minValue = 1;
    dp.maxValue = 2;
    dp.wrap = false;
    DynamicKeyCommandConfigurer dkcc1 = new DynamicKeyCommandConfigurer(dp);
    dkcc1.setName("name1");
    dkcc1.setFrozen(true);
    dkcc1.setValue("value1");
    dkcc1.updateValue();
    DynamicKeyCommandConfigurer dkcc2 = new DynamicKeyCommandConfigurer(dp);
    dkcc2.setName("name2");
    dkcc2.setFrozen(true);
    dkcc2.setValue("value2");
    dkcc2.updateValue();
    List<DynamicKeyCommand> keyCommands = new ArrayList<DynamicKeyCommand>();
    keyCommands.add(dkcc1.getKeyCommand());
    keyCommands.add(dkcc2.getKeyCommand());
    ListConfigurerHelper.getList(dp.keyCommandListConfig).add(dkcc1);
    ListConfigurerHelper.getList(dp.keyCommandListConfig).add(dkcc2);
    super.serializeTest(DynamicProperty.class, dp);
  }

  @Override
  void assertSame(DynamicProperty dp1, DynamicProperty dp2) {
    assertEquals(dp1.key, dp2.key);
    assertEquals(dp1.numeric, dp2.numeric);
    assertEquals(dp1.minValue, dp2.minValue);
    assertEquals(dp1.maxValue, dp2.maxValue);
    assertEquals(dp1.wrap, dp2.wrap);
    assertEquals(dp1.numeric, dp2.numeric);
    assertEquals(dp1.numeric, dp2.numeric);
    assertEquals(dp1.keyCommandListConfig, dp2.keyCommandListConfig);
  }
}
