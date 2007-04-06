package VASSAL.build.module.properties;

import java.beans.PropertyChangeEvent;

/**
 * A component that can contain mutable (updateable) properties
 * 
 * @author rkinney
 * 
 */
public interface MutablePropertiesContainer {
  /**
   * Set the value of a mutable property Any {@link MutablePropertySource} returned {@link #getGlobalProperty(String)}
   * using the given key will fire a {@link PropertyChangeEvent} with the new value
   * 
   * @param key
   * @param value
   */
  void setProperty(String key, String value);

  /** Find a GlobalProperty object with the given name */
  MutablePropertySource getGlobalProperty(String propertyName);
}
