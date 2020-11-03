package VASSAL.counters;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.build.module.properties.PropertySource;

/**
 * Interface for classes that can exporty the names and values of the properties they maintain
 */
public interface PropertyExporter extends PropertyNameSource, PropertySource {

  String LOCALIZED_NAME = "localizedName"; // NON-NLS

  /**
   * Return a Map of properties
   * @return Property Map
   */
  default Map<String, Object> getProperties() {
    final Map<String, Object> result = new HashMap<>();

    if (this instanceof GamePiece) {
      PieceAccess.GlobalAccess.hideAll(); // Force masked pieces to be hidden from me to generate correct masked name
      result.put(LOCALIZED_NAME, ((GamePiece) this).getLocalizedName());
      PieceAccess.GlobalAccess.revertAll();
    }

    return getProperties(result);
  }

  /**
   * Update an existing Map of Properties and return the Map.
   * Do not overwrite the value if a property with the same name already exists.
   *
   * @param result Map of properties
   * @return Update property Map
   */
  default Map<String, Object> getProperties(Map<String, Object> result) {
    final List<String> propertyNames = getPropertyNames();

    for (String propertyName : propertyNames) {
      if (! result.containsKey(propertyName)) {
        result.put(propertyName, getLocalizedProperty(propertyName));
      }
    }

    return result;
  }
}
