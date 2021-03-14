/*
 *
 * Copyright (c) 2000-2021 by Rodney Kinney, Brent Easton, Brian Reynolds
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
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.TemporaryToolBar;
import VASSAL.tools.ToolBarComponent;

/**
 * Dummy component that acts as a simple container for GlobalProperty components
 */
public class GlobalTranslatableMessages extends AbstractConfigurable implements TranslatableStringContainer, ToolBarComponent, PropertySource {
  private final TemporaryToolBar tempToolbar = new TemporaryToolBar();
  private PropertySource propertySource;
  private final Map<String, TranslatableString> initialValues = new HashMap<>();
  private TranslatableStringContainer parent;

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
    return Resources.getString("Editor.GlobalTranslatableMessages.component_type");
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
    return HelpFile.getReferenceManualPage("GlobalTranslatableMessages.html"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { GlobalTranslatableMessage.class };
  }

  @Override
  public void addTo(Buildable parent) {
    this.parent = (TranslatableStringContainer) parent;

    for (final Map.Entry<String, TranslatableString> e : initialValues.entrySet()) {
      this.parent.addTranslatableString(e.getKey(), e.getValue());
    }

    tempToolbar.setDelegate((ToolBarComponent) parent);
    propertySource = (PropertySource) parent;
  }

  @Override
  public void addTranslatableString(String key, TranslatableString p) {
    if (parent == null) {
      initialValues.put(key, p);
    }
    else {
      parent.addTranslatableString(key, p);
    }
  }

  @Override
  public TranslatableString removeTranslatableString(String key) {
    if (parent == null) {
      return initialValues.remove(key);
    }
    else {
      return parent.removeTranslatableString(key);
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
  public GlobalTranslatableMessage getTranslatableString(String name) {
    for (final GlobalTranslatableMessage prop : getComponentsOf(GlobalTranslatableMessage.class)) {
      if (prop.getConfigureName().equals(name)) {
        return prop;
      }
    }
    return null;
  }

  /*
   * Null i18n key prefix for this component
   */
  @Override
  public String getI18nPrefix() {
    return "";
  }

  public TranslatableStringContainer getParent() {
    return parent;
  }

  /**
   * Use the identity of the owning container (i.e. Module, map or zone)
   */
  @Override
  public String getTranslatableStringContainerId() {
    return parent.getTranslatableStringContainerId();
  }

  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    for (final GlobalTranslatableMessage prop : getComponentsOf(GlobalTranslatableMessage.class)) {
      l.add(prop.getConfigureName());
    }
    return l;
  }
}
