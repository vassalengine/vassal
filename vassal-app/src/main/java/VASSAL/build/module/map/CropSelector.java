package VASSAL.build.module.map;

import VASSAL.build.module.Map;
import java.awt.Rectangle;

public final class CropSelector {
  private CropSelector() {}

  public static Rectangle select(Map map, Rectangle initialSelection) {
    final CropOverlay overlay = new CropOverlay(map, initialSelection);
    return overlay.awaitSelection();
  }
}
