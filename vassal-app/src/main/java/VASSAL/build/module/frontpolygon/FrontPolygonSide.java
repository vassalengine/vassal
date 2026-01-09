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
    loadList("FrontPolygon.allies.layers", List.of("Show All US")),
    loadList("FrontPolygon.allies.maskOwners", List.of("Russian", "Allies"))
  ),
  GERMANS(
    new Color(214, 68, 68, 70),
    new Color(214, 68, 68),
    loadList("FrontPolygon.germans.layers", List.of("Show All Ger", "Show All German")),
    loadList("FrontPolygon.germans.maskOwners", List.of("German", "Axis"))
  );

  private static final String LAYER_PREFIX = "Layer - ";

  final Color fillColor;
  final Color outlineColor;
  private final List<String> layerNames;
  private final List<String> maskOwners;

  FrontPolygonSide(Color fillColor, Color outlineColor, List<String> layerNames, List<String> maskOwners) {
    this.fillColor = fillColor;
    this.outlineColor = outlineColor;
    this.layerNames = layerNames;
    this.maskOwners = maskOwners;
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

  boolean matchesMaskOwner(String owner) {
    if (owner == null) {
      return false;
    }
    for (final String candidate : maskOwners) {
      if (candidate.equalsIgnoreCase(owner.trim())) {
        return true;
      }
    }
    return false;
  }

  List<String> getLayerNames() {
    return layerNames;
  }

  List<String> getMaskOwners() {
    return maskOwners;
  }

  private static String normalizeLayerDescription(String description) {
    final String trimmed = description.trim();
    if (trimmed.regionMatches(true, 0, LAYER_PREFIX, 0, LAYER_PREFIX.length())) {
      return trimmed.substring(LAYER_PREFIX.length()).trim();
    }
    return trimmed;
  }

  private static List<String> loadList(String propertyKey, List<String> defaults) {
    final String raw = System.getProperty(propertyKey);
    if (raw == null || raw.isBlank()) {
      return defaults;
    }
    final String[] parts = raw.split(",");
    final List<String> names = new java.util.ArrayList<>();
    for (final String part : parts) {
      final String trimmed = part.trim();
      if (!trimmed.isEmpty()) {
        names.add(trimmed);
      }
    }
    return names.isEmpty() ? defaults : names;
  }

}
