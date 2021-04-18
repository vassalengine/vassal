/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.build.module;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;


import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.folder.PrototypeFolder;
import VASSAL.configure.Configurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;

/**
 * Container for definitions of Game Piece prototypes.
 * Actual definition is in inner class
 * {@link VASSAL.build.module.PrototypeDefinition}
 */
public class PrototypesContainer extends AbstractConfigurable {
  private static PrototypesContainer instance;
  private final Map<String, PrototypeDefinition> definitions = new HashMap<>();

  /**
   * Return an unmodifiable Collection of the current Prototype Definitions
   * @return PrototypeDefinition Collection
   */
  public Collection<PrototypeDefinition> getDefinitions() {
    return Collections.unmodifiableCollection(definitions.values());
  }

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
  public String getAttributeValueString(String key) {
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
  }

  @Override
  public Configurer getConfigurer() {
    return null;
  }

  @Override
  public void addTo(Buildable parent) {
    validator = new SingleChildInstance(GameModule.getGameModule(), getClass());
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{ PrototypeFolder.class, PrototypeDefinition.class };
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PrototypesContainer.component_type"); //$NON-NLS-1$
  }

  public void addDefinition(PrototypeDefinition def) {
    definitions.put(def.getConfigureName(), def);
    def.addPropertyChangeListener(evt -> {
      if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
        definitions.remove(evt.getOldValue());
        definitions.put((String) evt.getNewValue(),
          (PrototypeDefinition) evt.getSource());
      }
    });
  }

  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof PrototypeDefinition) {
      addDefinition((PrototypeDefinition) b);
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Prototypes.html"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  public static PrototypeDefinition getPrototype(String name) {
    if (instance == null) {
      final Iterator<PrototypesContainer> i =
        GameModule.getGameModule()
                  .getComponentsOf(PrototypesContainer.class)
                  .iterator();
      if (i.hasNext()) {
        instance = i.next();
      }
      else {
        return null;
      }
    }
    return instance.definitions.get(name);
  }

  @Override
  public ComponentI18nData getI18nData() {
    final ComponentI18nData data = super.getI18nData();
    data.setPrefix(""); //$NON-NLS-1$
    return data;
  }
}
