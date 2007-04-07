package VASSAL.build.module.properties;

import java.util.Enumeration;
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
public class GlobalProperties extends AbstractConfigurable implements MutablePropertiesContainer, ToolBarComponent, PropertySource {
  private TemporaryToolBar tempToolbar = new TemporaryToolBar();
  private PropertySource propertySource;
  private HashMap initialValues = new HashMap();
  private MutablePropertiesContainer parent;

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
    parent = null;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.htm");
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {GlobalProperty.class};
  }

  public void addTo(Buildable parent) {
    this.parent = (MutablePropertiesContainer) parent;
    for (Iterator it = initialValues.keySet().iterator(); it.hasNext();) {
      String key = (String) it.next();
      MutableProperty p = (MutableProperty) initialValues.get(key);
      this.parent.addMutableProperty(key, p);
    }
    tempToolbar.setDelegate((ToolBarComponent) parent);
    propertySource = (PropertySource) parent;
  }
  
  public void addMutableProperty(String key, MutableProperty p) {
    if (parent == null) {
      initialValues.put(key,p);
    }
    else {
      parent.addMutableProperty(key, p);
    }
  }

  public MutableProperty removeMutableProperty(String key) {
    if (parent == null) {
      return (MutableProperty) initialValues.remove(key);
    }
    else {
      return parent.removeMutableProperty(key);
    }
  }

  public JToolBar getToolBar() {
    return tempToolbar.getToolBar();
  }

  public Object getProperty(Object key) {
    return propertySource == null ? null : propertySource.getProperty(key);
  }

  public GlobalProperty getMutableProperty(String name) {
    GlobalProperty property = null;
    for (Enumeration e = getComponents(GlobalProperty.class);e.hasMoreElements() && property == null;) {
      GlobalProperty prop = (GlobalProperty) e.nextElement();
      if (prop.getConfigureName().equals(name)) {
        property = prop;
      }
    }
    return property;
  }
}
