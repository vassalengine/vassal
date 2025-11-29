package VASSAL.build.module.map;

import VASSAL.build.module.Map;

import java.awt.Rectangle;

/**
 * High-level helper that manages the lifecycle of a rectangle selection session.
 * It temporarily disables regular map key buffering (lasso selection), installs
 * a {@link RectangularSelectionOverlay} for the interactive drag/resize UX, waits
 * for completion, and then restores the original state.
 */
public final class RectangularSelector {
  private RectangularSelector() {}

  public static Rectangle select(Map map, Rectangle initialSelection) {
    final KeyBufferer kb = map.getKeyBufferer();
    if (kb != null) {
      kb.setEnabled(false);
    }
    final RectangularSelectionOverlay overlay = new RectangularSelectionOverlay(map, initialSelection);
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
