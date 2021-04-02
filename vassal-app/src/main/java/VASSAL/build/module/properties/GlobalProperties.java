/*
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
import VASSAL.i18n.Resources;
import VASSAL.tools.TemporaryToolBar;
import VASSAL.tools.ToolBarComponent;

/**
 * Dummy component that acts as a simple container for GlobalProperty components
 *
 * @author rkinney
 *
 */
public class GlobalProperties extends AbstractConfigurable implements MutablePropertiesContainer, ToolBarComponent, PropertySource {
  private final TemporaryToolBar tempToolbar = new TemporaryToolBar();
  private PropertySource propertySource;
  private final Map<String, MutableProperty> initialValues = new HashMap<>();
  private MutablePropertiesContainer parent;

  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public Configurer getConfigurer() {
    return null;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GlobalProperties.component_type");
  }

  @Override
  public void setAttribute(String key, Object value) {
  }

  @Override
  public String getAttributeValueString(String key) {
    return null;
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.html"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {GlobalProperty.class};
  }

  @Override
  public void addTo(Buildable parent) {
    this.parent = (MutablePropertiesContainer) parent;

    for (final Map.Entry<String, MutableProperty> e : initialValues.entrySet()) {
      this.parent.addMutableProperty(e.getKey(), e.getValue());
    }

    tempToolbar.setDelegate((ToolBarComponent) parent);
    propertySource = (PropertySource) parent;
    GameModule.getGameModule().addCommandEncoder(
      new ChangePropertyCommandEncoder(this));
  }

  @Override
  public void addMutableProperty(String key, MutableProperty p) {
    if (parent == null) {
      initialValues.put(key, p);
    }
    else {
      parent.addMutableProperty(key, p);
    }
  }

  @Override
  public MutableProperty removeMutableProperty(String key) {
    if (parent == null) {
      return initialValues.remove(key);
    }
    else {
      return parent.removeMutableProperty(key);
    }
  }

  @Override
  public JToolBar getToolBar() {
    return tempToolbar.getToolBar();
  }

  @Override
  public Object getProperty(Object key) {
    return propertySource == null ? null : propertySource.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    return propertySource == null ? null : propertySource.getLocalizedProperty(key);
  }

  @Override
  public GlobalProperty getMutableProperty(String name) {
    GlobalProperty property = null;
    for (final GlobalProperty prop : getComponentsOf(GlobalProperty.class)) {
      if (name.equals(prop.getConfigureName())) {
        property = prop;
      }
    }
    return property;
  }

  /*
   * Null i18n key prefix for this component
   */
  @Override
  public String getI18nPrefix() {
    return "";
  }

  public MutablePropertiesContainer getParent() {
    return parent;
  }

  /**
   * Use the identity of the owning container (i.e. Module, map or zone)
   */
  @Override
  public String getMutablePropertiesContainerId() {
    return parent.getMutablePropertiesContainerId();
  }

  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    for (final GlobalProperty prop : getComponentsOf(GlobalProperty.class)) {
      l.add(prop.getConfigureName());
    }
    return l;
  }
}
