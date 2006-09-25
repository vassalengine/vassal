package VASSAL.build.module.properties;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Iterator;

import javax.swing.JToolBar;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.tools.TemporaryToolBar;
import VASSAL.tools.ToolBarComponent;

/**
 * Dummy component that acts as a simple container for GlobalProperty components
 * 
 * @author rkinney
 * 
 */
public class GlobalProperties extends AbstractConfigurable implements GlobalPropertiesContainer, ToolBarComponent, PropertySource {
  private TemporaryToolBar tempToolbar = new TemporaryToolBar();
  private PropertySource propertySource;
  private PropertyChangeListener forwardPropertyChange;
  private PropertyChangeSupport propertyChangeSupport;
  private HashMap initialValues = new HashMap();

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }
  
  public Configurer getConfigurer() {
    return null;
  }
  
  public static String getConfigureTypeName() {
    return "Global Properties";
  }

  public void setAttribute(String key, Object value) {
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void removeFrom(Buildable parent) {
    propertyChangeSupport.removePropertyChangeListener(((GlobalPropertiesContainer) parent).getPropertyListener());
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GlobalProperties.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {GlobalProperty.class};
  }

  public void addTo(Buildable parent) {
    propertyChangeSupport = new PropertyChangeSupport(this);
    propertyChangeSupport.addPropertyChangeListener(((GlobalPropertiesContainer) parent).getPropertyListener());
    for (Iterator it = initialValues.keySet().iterator(); it.hasNext();) {
      Object key = (Object) it.next();
      Object value = initialValues.get(key);
      propertyChangeSupport.firePropertyChange(key.toString(),null,value);
    }
    tempToolbar.setDelegate((ToolBarComponent) parent);
    propertySource = (PropertySource) parent;
  }

  public PropertyChangeListener getPropertyListener() {
    if (forwardPropertyChange == null) {
      forwardPropertyChange = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (propertyChangeSupport == null) {
            initialValues.put(evt.getPropertyName(), evt.getNewValue());
          }
          else {
            propertyChangeSupport.firePropertyChange(evt);
          }
        }
      };
    }
    return forwardPropertyChange;
  }

  public JToolBar getToolBar() {
    return tempToolbar.getToolBar();
  }

  public Object getProperty(Object key) {
    return propertySource == null ? null : propertySource.getProperty(key);
  }

}
