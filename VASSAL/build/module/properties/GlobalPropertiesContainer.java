package VASSAL.build.module.properties;

import java.beans.PropertyChangeListener;

/**
 * A component that can contain global properties
 * @author rkinney
 *
 */
public interface GlobalPropertiesContainer {
  PropertyChangeListener getPropertyListener();
}
