package VASSAL.counters;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import VASSAL.script.expression.BeanShellExpression;

public class CalculatedPropertySerializeTest extends
    SerializeTest<CalculatedProperty> {
  @Test
  public void serialize() throws Exception {
    CalculatedProperty cp = new CalculatedProperty();
    cp.name = "testName";
    cp.expression = new BeanShellExpression("x+1");
    super.serializeTest(CalculatedProperty.class, cp);
  }

  @Override
  void assertSame(CalculatedProperty cp1, CalculatedProperty cp2) {
    assertEquals(cp1.name, cp2.name);
    assertEquals(cp1.expression, cp2.expression);
  }

}
