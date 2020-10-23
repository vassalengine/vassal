package VASSAL.counters;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.build.module.properties.PropertySource;

public interface PropertyExporter extends PropertyNameSource, PropertySource {

  String LOCALIZED_NAME = "localizedName"; // NON-NLS

  default Map<String, Object> getProperties() {
    final List<String> propertyNames = getPropertyNames();
    final Map<String, Object> result = new HashMap<>();
    for (String propertyName : propertyNames) {
      result.put(propertyName, getLocalizedProperty(propertyName));
    }

    if (this instanceof GamePiece) {
      result.put(LOCALIZED_NAME, ((GamePiece) this).getLocalizedName());
    }
    return result;
  }
}
