/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module.gamepieceimage;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.SingleChildInstance;

/**
 * Container for definitions of Generic Counter Definitions.
 * Actual definition is in inner class
 * {@link VASSAL.build.module.gamepieceimage.GamePieceLayout}.
 */
public class GamePieceLayoutsContainer extends AbstractConfigurable {
  protected Map<String,GamePieceLayout> definitions = new HashMap<>();

  protected GamePieceLayout getDefinition(String name) {
    return definitions.get(name);
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
    validator = new SingleChildInstance(GameModule.getGameModule(),getClass());
    setAllAttributesUntranslatable();
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{GamePieceLayout.class};
  }

  public static String getConfigureTypeName() {
    return "Game Piece Layouts";
  }

  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof GamePieceLayout) {
      GamePieceLayout def = (GamePieceLayout) b;
      definitions.put(def.getConfigureName(), def);
      def.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
            definitions.remove(evt.getOldValue());
            definitions.put((String) evt.getNewValue(),
                            (GamePieceLayout) evt.getSource());
          }
        }
      });
    }
  }

  @Override
  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof GamePieceLayout) {
      definitions.remove(((GamePieceLayout) b).getConfigureName());
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceLayouts.htm"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  public GamePieceImage getGenericDefn(String defnName) {
    for (GamePieceLayout d : definitions.values()) {
      if (d != null) {
        return d.getGenericDefn(defnName);
      }
    }
    return null;
  }
}

