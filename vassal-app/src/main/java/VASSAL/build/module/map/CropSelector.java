package VASSAL.build.module.map;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.KeyBufferer;
import java.awt.Rectangle;

public final class CropSelector {
  private CropSelector() {}

  public static Rectangle select(Map map, Rectangle initialSelection) {
    final KeyBufferer kb = map.getKeyBufferer();
    if (kb != null) {
      kb.setEnabled(false);
    }
    final CropOverlay overlay = new CropOverlay(map, initialSelection);
    try {
      return overlay.awaitSelection();
    }
    finally {
      if (kb != null) {
        kb.setEnabled(true);
      }
    }
  }
}
