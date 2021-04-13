package VASSAL.counters;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

import java.awt.event.InputEvent;
import java.awt.image.BufferedImage;

import org.junit.jupiter.api.Test;

import VASSAL.build.GameModule;
import VASSAL.tools.ResourcePathFinder;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.ImageOp;

public class EmbellishmentSerializeTest extends SerializeTest<Embellishment> {
  class MockResourcePathFinder implements ResourcePathFinder {
    public String findImagePath(String name) {
      return "images/" + name;
    }
    public String findHelpFileName(String name) {
      return name;
    }
  }
  @Test
  public void serialize() throws Exception {
    Embellishment emb = new Embellishment();
    emb.activateCommand = "testActivateCommand";
    emb.activateModifiers = InputEvent.CTRL_DOWN_MASK;
    emb.activateKey = "testActivateKey";
    emb.upCommand = "testUpCommand";
    emb.upModifiers = InputEvent.CTRL_DOWN_MASK;
    emb.upKey = "testUpKey";
    emb.downCommand = "testDownCommand";
    emb.downModifiers = InputEvent.CTRL_DOWN_MASK;
    emb.downKey = "testDownKey";
    emb.resetKey = NamedKeyStroke.of("A");
    emb.resetLevel = new FormattedString("resetLevel");
    emb.drawUnderneathWhenSelected = true;
    emb.xOff = 1;
    emb.yOff = 2;
    emb.imageName = new String[] {"imageName1", "imageName2"};
    emb.commonName = new String[] {"commonName1", "commonName2"};
    emb.loopLevels = true;
    emb.name = "testName";
    emb.rndKey = NamedKeyStroke.of("B");
    emb.followProperty = true;
    emb.propertyName = "testPropertyName";
    emb.firstLevelValue = 3;
    emb.version = 4;
    emb.alwaysActive = true;
    emb.activateKeyStroke = NamedKeyStroke.of("C");
    emb.increaseKeyStroke = NamedKeyStroke.of("D");
    emb.decreaseKeyStroke = NamedKeyStroke.of("E");
    ImageOp im = Op.load(new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR));
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      try (MockedStatic<Op> staticOp = Mockito.mockStatic(Op.class)) {
        // Now return the dummy image when asked instead of looking in the archive
        staticOp.when(() -> Op.load(any(String.class))).thenReturn(im);
        final GameModule gm = mock(GameModule.class);
        final MockResourcePathFinder rpf = new MockResourcePathFinder();
        staticGm.when(GameModule::getGameModule).thenReturn(gm);
        when(gm.getResourcePathFinder()).thenReturn(rpf);
        super.serializeTest(Embellishment.class, emb);
      }
    }
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
