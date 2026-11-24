package VASSAL.configure;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;

public class ParameterTest {
  @Test
  public void equalParametersHaveEqualHashCodes() {
    final Parameter a = new Parameter("prop", "$value$");
    final Parameter b = new Parameter("prop", "$value$");
    assertEquals(a, b);
    assertEquals(a.hashCode(), b.hashCode());
  }

  @Test
  public void equalParametersCollapseInHashSet() {
    final Set<Parameter> set = new HashSet<>();
    set.add(new Parameter("prop", "1"));
    set.add(new Parameter("prop", "1"));
    set.add(new Parameter("prop", "2"));
    assertEquals(2, set.size());
  }

  @Test
  public void differentParametersAreNotEqual() {
    assertNotEquals(new Parameter("a", "1"), new Parameter("a", "2"));
    assertNotEquals(new Parameter("a", "1"), new Parameter("b", "1"));
    assertFalse(new Parameter("a", "1").equals("a"));
  }

  @Test
  public void nullFieldsDoNotThrow() {
    assertEquals(new Parameter(null, null), new Parameter(null, null));
    assertEquals(new Parameter(null, null).hashCode(), new Parameter(null, null).hashCode());
    assertNotEquals(new Parameter(null, "x"), new Parameter("a", "x"));
  }

  @Test
  public void encodedFormRoundTrips() {
    final Parameter a = new Parameter("prop", "a:b");
    final Parameter b = new Parameter(a.encode());
    assertEquals(a, b);
    assertEquals(a.hashCode(), b.hashCode());
  }
}
