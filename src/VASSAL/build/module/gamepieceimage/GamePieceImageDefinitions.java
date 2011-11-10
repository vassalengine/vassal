package VASSAL.build.module.gamepieceimage;

import java.awt.Color;

import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
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
 * Actual definition is in inner class {@link GamePieceLayout}
 */
public class GamePieceImageDefinitions extends AbstractConfigurable {

  protected static GamePieceImageDefinitions instance;
  protected GamePieceLayoutsContainer definitions;
  protected ColorManager colors;
  protected FontManager fonts;

  protected static final Color DEFAULT_COLOR  = Color.WHITE;

  public GamePieceImageDefinitions() {
    instance = this;
  }

  public static GamePieceImageDefinitions getInstance() {
    return instance;
  }

  public void build(Element e) {
    super.build(e);

    if (colors == null) {
      addChild(new ColorManager());
      colors.build(null);
    }
    if (fonts == null) {
      addChild(new FontManager());
      fonts.build(null);
    }
    if (definitions == null) addChild(new GamePieceLayoutsContainer());

  }

  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
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
    setAllAttributesUntranslatable();
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {
        GamePieceLayoutsContainer.class,
        ColorManager.class,
        FontManager.class};
  }

  public static String getConfigureTypeName() {
    return "Game Piece Image Definitions"; //$NON-NLS-1$
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof GamePieceLayoutsContainer) {
      definitions = (GamePieceLayoutsContainer) b;
    }
    else if (b instanceof ColorManager) {
      colors = (ColorManager) b;
    }
    else if (b instanceof FontManager) {
      fonts = (FontManager) b;
    }
  }

  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof GamePieceLayoutsContainer) {
      definitions = null;
    }
    else if (b instanceof ColorManager) {
      colors = null;
    }
    else if (b instanceof FontManager) {
      fonts = null;
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceImageDefinitions.htm"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
  }

  public GamePieceImage getGenericDefn(String defnName) {

    return definitions.getGenericDefn(defnName);

  }

}
