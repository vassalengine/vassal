package VASSAL.counters;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import VASSAL.configure.ListConfigurerHelper;
import VASSAL.counters.DynamicProperty.DynamicKeyCommand;
import VASSAL.counters.DynamicProperty.DynamicKeyCommandConfigurer;

public class SetGlobalPropertySerializeTest extends
    SerializeTest<SetGlobalProperty> {
  @Test
  public void serialize() throws Exception {
    SetGlobalProperty sgp = new SetGlobalProperty();
    sgp.key = "testKey";
    sgp.numeric = false;
    sgp.minValue = 1;
    sgp.maxValue = 2;
    sgp.wrap = false;
    setKeyCommandListConfig(sgp);
    sgp.description = "testDesc";
    sgp.propertyLevel = "testPropertyLevel";
    sgp.searchName = "testSearchName";
    super.serializeTest(SetGlobalProperty.class, sgp);
  }

  private void setKeyCommandListConfig(SetGlobalProperty sgp) {
    DynamicKeyCommandConfigurer dkcc1 = new DynamicKeyCommandConfigurer(sgp);
    dkcc1.setName("name1");
    dkcc1.setFrozen(true);
    dkcc1.setValue("value1");
    dkcc1.updateValue();
    DynamicKeyCommandConfigurer dkcc2 = new DynamicKeyCommandConfigurer(sgp);
    dkcc2.setName("name2");
    dkcc2.setFrozen(true);
    dkcc2.setValue("value2");
    dkcc2.updateValue();
    List<DynamicKeyCommand> keyCommands = new ArrayList<DynamicKeyCommand>();
    keyCommands.add(dkcc1.getKeyCommand());
    keyCommands.add(dkcc2.getKeyCommand());
    ListConfigurerHelper.getList(sgp.keyCommandListConfig).add(dkcc1);
    ListConfigurerHelper.getList(sgp.keyCommandListConfig).add(dkcc2);
  }

  @Override
  void assertSame(SetGlobalProperty sgp1, SetGlobalProperty sgp2) {
    assertEquals(sgp1.key, sgp2.key);
    assertEquals(sgp1.numeric, sgp2.numeric);
    assertEquals(sgp1.minValue, sgp2.minValue);
    assertEquals(sgp1.maxValue, sgp2.maxValue);
    assertEquals(sgp1.wrap, sgp2.wrap);
    assertEquals(sgp1.numeric, sgp2.numeric);
    assertEquals(sgp1.numeric, sgp2.numeric);
    assertEquals(sgp1.keyCommandListConfig, sgp2.keyCommandListConfig);
    assertEquals(sgp1.description, sgp2.description);
    assertEquals(sgp1.propertyLevel, sgp2.propertyLevel);
    assertEquals(sgp1.searchName, sgp2.searchName);
  }
}
