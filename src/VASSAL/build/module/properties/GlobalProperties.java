/*
 * $Id$
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module.properties;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JToolBar;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
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
  private Map<String,MutableProperty> initialValues =
    new HashMap<String,MutableProperty>();
  private MutablePropertiesContainer parent;

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
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
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.htm");
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {GlobalProperty.class};
  }

  public void addTo(Buildable parent) {
    this.parent = (MutablePropertiesContainer) parent;

    for (Map.Entry<String,MutableProperty> e : initialValues.entrySet()) {
      this.parent.addMutableProperty(e.getKey(), e.getValue());
    }

    tempToolbar.setDelegate((ToolBarComponent) parent);
    propertySource = (PropertySource) parent;
    GameModule.getGameModule().addCommandEncoder(
      new ChangePropertyCommandEncoder(this));
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
      return initialValues.remove(key);
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

  public Object getLocalizedProperty(Object key) {
    return propertySource == null ? null : propertySource.getLocalizedProperty(key);
  }

   public GlobalProperty getMutableProperty(String name) {
   GlobalProperty property = null;
    for (GlobalProperty prop : getComponentsOf(GlobalProperty.class)) {
      if (prop.getConfigureName().equals(name)) {
        property = prop;
      }
    }
    return property;
  }

  /*
   * Null i18n key prefix for this component
   */
  public String getI18nPrefix() {
    return "";
  }

  public MutablePropertiesContainer getParent() {
    return parent;
  }

  /**
   * Use the identity of the owning container (i.e. Module, map or zone)
   */
  public String getMutablePropertiesContainerId() {
    return parent.getMutablePropertiesContainerId();
  }

  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<String>();
    for (GlobalProperty prop : getComponentsOf(GlobalProperty.class)) {
      l.add(prop.getConfigureName());
    }
    return l;
  }
}
