package VASSAL.build.module.frontpolygon;

import java.awt.Color;
import java.util.List;

/**
 * Identifies which side a piece belongs to for front-line rendering.
 */
enum FrontPolygonSide {
  ALLIES(
    new Color(39, 110, 241, 70),
    new Color(39, 110, 241),
    List.of("Show All US")
  ),
  GERMANS(
    new Color(214, 68, 68, 70),
    new Color(214, 68, 68),
    List.of("Show All Ger", "Show All German")
  );

  private static final String LAYER_PREFIX = "Layer - ";

  final Color fillColor;
  final Color outlineColor;
  private final List<String> layerNames;

  FrontPolygonSide(Color fillColor, Color outlineColor, List<String> layerNames) {
    this.fillColor = fillColor;
    this.outlineColor = outlineColor;
    this.layerNames = layerNames;
  }

  boolean matchesLayer(String description) {
    if (description == null) {
      return false;
    }
    final String normalized = normalizeLayerDescription(description);
    for (final String candidate : layerNames) {
      if (candidate.equalsIgnoreCase(normalized)) {
        return true;
      }
    }
    return false;
  }

  private static String normalizeLayerDescription(String description) {
    final String trimmed = description.trim();
    if (trimmed.regionMatches(true, 0, LAYER_PREFIX, 0, LAYER_PREFIX.length())) {
      return trimmed.substring(LAYER_PREFIX.length()).trim();
    }
    return trimmed;
  }
}
