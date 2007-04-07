package VASSAL.build.module.properties;

import java.util.HashMap;
import java.util.Map;

/**
 * A component that can contain mutable (updateable) properties
 * 
 * @author rkinney
 * 
 */
public interface MutablePropertiesContainer {
  /**
   * Add a property under the given key
   * @param key
   * @param p
   */
  void addMutableProperty(String key, MutableProperty p);
  
  /**
   * Remove the property with the given key
   * @param key
   */
  MutableProperty removeMutableProperty(String key);

  /** Find a GlobalProperty object with the given name */
  MutableProperty getMutableProperty(String propertyName);
  
  /**
   * Simple implementation of {@link MutablePropertiesContainer}
   * @author rkinney
   *
   */
  public static class Impl implements MutablePropertiesContainer {
    private Map props = new HashMap();

    public void addMutableProperty(String key, MutableProperty p) {
      props.put(key,p);
    }

    public MutableProperty getMutableProperty(String propertyName) {
      return (MutableProperty) props.get(propertyName);
    }

    public MutableProperty removeMutableProperty(String key) {
      return (MutableProperty) props.remove(key);
    }

    public void setProperty2(String key, String value) {
      // TODO Delete this method
    }
  }
}
