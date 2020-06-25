package VASSAL.counters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;

import java.awt.event.InputEvent;

import org.junit.Test;

import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;


public class EmbellishmentSerializeTest extends SerializeTest<Embellishment> {
  @Test
  public void serialize() throws Exception {
    Embellishment emb = new Embellishment();
    emb.activateCommand = "testActivateCommand";
    emb.activateModifiers = InputEvent.CTRL_MASK;
    emb.activateKey = "testActivateKey";
    emb.upCommand = "testUpCommand";
    emb.upModifiers = InputEvent.CTRL_MASK;
    emb.upKey = "testUpKey";
    emb.downCommand = "testDownCommand";
    emb.downModifiers = InputEvent.CTRL_MASK;
    emb.downKey = "testDownKey";
    emb.resetKey = new NamedKeyStroke("A");
    emb.resetLevel = new FormattedString("resetLevel");
    emb.drawUnderneathWhenSelected = true;
    emb.xOff = 1;
    emb.yOff = 2;
    emb.imageName = new String[] {"imageName1", "imageName2"};
    emb.commonName = new String[] {"commonName1", "commonName2"};
    emb.loopLevels = true;
    emb.name = "testName";
    emb.rndKey = new NamedKeyStroke("B");
    emb.followProperty = true;
    emb.propertyName = "testPropertyName";
    emb.firstLevelValue = 3;
    emb.version = 4;
    emb.alwaysActive = true;
    emb.activateKeyStroke = new NamedKeyStroke("C");
    emb.increaseKeyStroke = new NamedKeyStroke("D");
    emb.decreaseKeyStroke = new NamedKeyStroke("E");

    super.serializeTest(Embellishment.class, emb);
  }

  @Override
  void assertSame(Embellishment emb1, Embellishment emb2) {
    assertEquals(emb1.activateCommand, emb2.activateCommand);
    assertEquals(emb1.activateModifiers, emb2.activateModifiers);
    assertEquals(emb1.activateKey, emb2.activateKey);
    assertEquals(emb1.upCommand, emb2.upCommand);
    assertEquals(emb1.upModifiers, emb2.upModifiers);
    assertEquals(emb1.upKey, emb2.upKey);
    assertEquals(emb1.downCommand, emb2.downCommand);
    assertEquals(emb1.downModifiers, emb2.downModifiers);
    assertEquals(emb1.downKey, emb2.downKey);
    assertEquals(emb1.resetKey, emb2.resetKey);
    assertEquals(emb1.resetLevel, emb2.resetLevel);
    assertEquals(emb1.drawUnderneathWhenSelected, emb2.drawUnderneathWhenSelected);
    assertEquals(emb1.xOff, emb2.xOff);
    assertEquals(emb1.yOff, emb2.yOff);
    assertArrayEquals(emb1.imageName, emb2.imageName);
    assertArrayEquals(emb1.commonName, emb2.commonName);
    assertEquals(emb1.loopLevels, emb2.loopLevels);
    assertEquals(emb1.name, emb2.name);
    assertEquals(emb1.rndKey, emb2.rndKey);
    assertEquals(emb1.followProperty, emb2.followProperty);
    assertEquals(emb1.propertyName, emb2.propertyName);
    assertEquals(emb1.firstLevelValue, emb2.firstLevelValue);
    assertEquals(emb1.version, emb2.version);
    assertEquals(emb1.alwaysActive, emb2.alwaysActive);
    assertEquals(emb1.activateKeyStroke, emb2.activateKeyStroke);
    assertEquals(emb1.increaseKeyStroke, emb2.increaseKeyStroke);
    assertEquals(emb1.decreaseKeyStroke, emb2.decreaseKeyStroke);
  }
}
