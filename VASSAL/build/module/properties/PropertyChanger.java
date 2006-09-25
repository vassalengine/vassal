package VASSAL.build.module.properties;

/**
 * Provides a new value for a global property
 * 
 * @author rkinney
 * 
 */
public interface PropertyChanger {
  String getNewValue(String oldValue);
}
