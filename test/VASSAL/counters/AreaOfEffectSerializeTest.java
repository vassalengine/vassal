package VASSAL.counters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.awt.Color;

import org.junit.Test;

import VASSAL.tools.NamedKeyStroke;


public class AreaOfEffectSerializeTest extends SerializeTest<AreaOfEffect> {
  @Test
  public void serialize() throws Exception {
    AreaOfEffect aoe = new AreaOfEffect();
    aoe.transparencyColor = Color.black;
      aoe.transparencyLevel = (float)1.0;
      aoe.radius = 2;
      aoe.alwaysActive = true;
      aoe.activateCommand = "testActivateCommand";
      aoe.activateKey = new NamedKeyStroke("A");
      aoe.mapShaderName = "testMapShaderName";
      aoe.fixedRadius = true;
      aoe.radiusMarker = "testRadiusMarker";
    aoe.description = "testDesc";
    super.serializeTest(AreaOfEffect.class, aoe);
  }

  @Override
  void assertSame(AreaOfEffect aoe1, AreaOfEffect aoe2) {
    assertTrue(aoe1.transparencyColor.equals(aoe2.transparencyColor));
    assertEquals(aoe1.transparencyLevel, aoe2.transparencyLevel, 0.01);
    assertEquals(aoe1.radius, aoe2.radius);
    assertEquals(aoe1.alwaysActive, aoe2.alwaysActive);
    assertEquals(aoe1.activateCommand, aoe2.activateCommand);
    assertEquals(aoe1.activateKey, aoe2.activateKey);
    assertEquals(aoe1.mapShaderName, aoe2.mapShaderName);
    assertEquals(aoe1.fixedRadius, aoe2.fixedRadius);
    assertEquals(aoe1.radiusMarker, aoe2.radiusMarker);
    assertEquals(aoe1.description, aoe2.description);
  }
}
