package VASSAL.counters;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import VASSAL.tools.NamedKeyStroke;

public class FreeRotatorSerializeTest extends SerializeTest<FreeRotator> {
  @Test
  public void serializeLength1() throws Exception {
    FreeRotator fr = new FreeRotator();
    int length = 1;
    setAngles(fr, length);
    fr.setAngleKey = NamedKeyStroke.of("A");
    fr.setAngleText = "testAngleText";
    fr.rotateRNDKey = NamedKeyStroke.of("B");
    fr.rotateRNDText = "testRotateRNDText";
    fr.name = "testName";

    super.serializeTest(FreeRotator.class, fr);
  }

  @Test
  public void serializeLength5() throws Exception {
    FreeRotator fr = new FreeRotator();
    int length = 5;
    setAngles(fr, length);
    fr.rotateCWKey = NamedKeyStroke.of("B");
    fr.rotateCCWKey = NamedKeyStroke.of("B");
    fr.rotateCWText = "testRotateCWText";
    fr.rotateCCWText = "testRotateCCWText";
    fr.rotateRNDKey = NamedKeyStroke.of("B");
    fr.rotateRNDText = "testRotateRNDText";
    fr.name = "testName";

    super.serializeTest(FreeRotator.class, fr);
  }

  private void setAngles(FreeRotator fr, int length) {
    fr.validAngles = new double[length];
    for (int i = 0; i < length; ++i) {
      fr.validAngles[i] = -i * (360.0 / length);
    }
  }

  @Override
  void assertSame(FreeRotator fr1, FreeRotator fr2) {
    assertEquals(fr1.setAngleKey, fr2.setAngleKey);
    assertEquals(fr1.setAngleText, fr2.setAngleText);
    assertEquals(fr1.rotateRNDKey, fr2.rotateRNDKey);
    assertEquals(fr1.rotateRNDText, fr2.rotateRNDText);
    assertEquals(fr1.name, fr2.name);
    if (fr1.validAngles.length != 1) {
      assertEquals(fr1.rotateCWKey, fr2.rotateCWKey);
      assertEquals(fr1.rotateCCWKey, fr2.rotateCCWKey);
      assertEquals(fr1.rotateCWText, fr2.rotateCWText);
      assertEquals(fr1.rotateCCWText, fr2.rotateCCWText);
    }
  }
}
