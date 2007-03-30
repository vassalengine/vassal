package VASSAL.build.module.properties;

import java.beans.PropertyChangeListener;

/**
 * A component that can contain global properties
 * 
 * @author rkinney
 * 
 */
public interface GlobalPropertiesContainer {
  /**
   * Return a PropertyChangeListener that can be used to update the value of one of the GlobalProperties that this
   * object contains. Firing an event to this PropertyChangeListener will set the value of the GlobalProperty with the
   * corresponding name
   */
  PropertyChangeListener getPropertyListener();

  /** Find a GlobalProperty object with the given name */
  GlobalProperty getGlobalProperty(String propertyName);
}
