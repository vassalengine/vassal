package VASSAL.build.module.gamepieceimage;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Iterator;

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

/**
 * Container for definitions of Generic Counter Definitions.
 * Actual definition is in inner class {@link VASSAL.build.module.gamepieceimage.GamePieceLayout}
 */
public class GamePieceLayoutsContainer extends AbstractConfigurable {
  
  protected HashMap definitions = new HashMap();
  
  protected GamePieceLayout getDefinition(String name) {
    return (GamePieceLayout) definitions.get(name);
  }
  
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
    return new Class[]{GamePieceLayout.class};
  }

  public static String getConfigureTypeName() {
    return "Game Piece Layouts";
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof GamePieceLayout) {
      GamePieceLayout def = (GamePieceLayout) b;
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

  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof GamePieceLayout) {
      definitions.remove(((GamePieceLayout) b).getConfigureName());
    }
  }
  
  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GamePieceLayouts.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void removeFrom(Buildable parent) {
  }

  public GamePieceImage getGenericDefn(String defnName) {

    GamePieceImage defn = null;
    
    Iterator i = definitions.values().iterator();
    while (i.hasNext() && defn == null) {
      defn = ((GamePieceLayout) i.next()).getGenericDefn(defnName);
    }
    
    return defn;
  }
}

