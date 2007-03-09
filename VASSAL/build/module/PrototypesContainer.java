package VASSAL.build.module;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.HashMap;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.SingleChildInstance;

/*
 * $Id$
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

/**
 * Container for definitions of Game Piece prototypes.
 * Actual definition is in inner class {@link VASSAL.build.module.PrototypeDefinition}
 */
public class PrototypesContainer extends AbstractConfigurable {
  private static PrototypesContainer instance;
  private HashMap definitions = new HashMap();

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  public Configurer getConfigurer() {
    return null;
  }

  public void addTo(Buildable parent) {
    validator = new SingleChildInstance(GameModule.getGameModule(),getClass());
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{PrototypeDefinition.class};
  }

  public static String getConfigureTypeName() {
    return "Game Piece Prototype Definitions";
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof PrototypeDefinition) {
      PrototypeDefinition def = (PrototypeDefinition) b;
      definitions.put(def.getConfigureName(), def);
      def.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
            definitions.remove(evt.getOldValue());
            definitions.put(evt.getNewValue(), evt.getSource());
          }
        }
      });
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Prototypes.htm"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
  }

  public static PrototypeDefinition getPrototype(String name) {
    if (instance == null) {
      Enumeration e = GameModule.getGameModule().getComponents(PrototypesContainer.class);
      if (e.hasMoreElements()) {
        instance = (PrototypesContainer) e.nextElement();
      }
      else {
        return null;
      }
    }
    return (PrototypeDefinition) instance.definitions.get(name);
  }
}
