/*
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

import java.awt.Color;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
 * Container for definitions of Generic Color Definitions
 */
public class ColorManager extends AbstractConfigurable {

  /**
   * Statics - Standard Colors
   */
  protected static ColorManager instance;

  public static ColorManager getColorManager() {
    return instance;
  }

  public static final Color DEFAULT_COLOR = Color.WHITE;
  public static final String SELECT_COLOR = "Select...";

  protected static Color[] standardColors = new Color[] {
    Color.WHITE,
    Color.GRAY,
    Color.BLACK,
    null,
    Color.RED,
    Color.GREEN,
    Color.BLUE,
    Color.ORANGE,
    Color.PINK,
    Color.CYAN,
    Color.MAGENTA,
    Color.YELLOW,
    Color.LIGHT_GRAY,
    Color.DARK_GRAY,
  };

  protected static String[] standardColorNames = new String[] {
    "WHITE",
    "GRAY",
    "BLACK",
    "CLEAR",
    "RED",
    "GREEN",
    "BLUE",
    "ORANGE",
    "PINK",
    "CYAN",
    "MAGENTA",
    "YELLOW",
    "LIGHT GRAY",
    "DARK GRAY"
  };

  protected static String getStandardColorName(Color c) {
    for (int i = 0; i < standardColors.length; i++) {
      if (standardColors[i].equals(c)) {
        return standardColorNames[i];
      }
    }
    return null;
  }

  protected static Color getStandardColor(String name) {
    for (int i = 0; i < standardColors.length; i++) {
      if (standardColorNames[i].equals(name)) {
        return standardColors[i];
      }
    }
    return null;
  }

  /**
   * User defined Colors
   */
  protected Map<String,ColorSwatch> userColors =
    new HashMap<>();

  public ColorManager() {
    instance = this;
  }

  public ColorSwatch getColorSwatch(String name) {
    ColorSwatch c = userColors.get(name);
    if (c == null) {
      c = new ColorSwatch(name, getStandardColor(name));
    }
    return c;
  }

  public ColorSwatch getColorSwatch(Color color) {
    if (color == null) {
      return new ColorSwatch("CLEAR", null);
    }

    ColorSwatch swatch = null;
    for (ColorSwatch cs : userColors.values()) {
      if (color.equals(cs.getColor())) {
        swatch = cs;
        break;
      }
    }

    if (swatch == null) {
      for (int j = 0; j < standardColors.length && swatch == null; j++) {
        if (standardColors[j] != null && standardColors[j].equals(color)) {
          swatch = new ColorSwatch(standardColorNames[j], standardColors[j]);
        }
      }
    }

    if (swatch == null) {
      swatch = new ColorSwatch(SELECT_COLOR, color);
    }

    return swatch;
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
    return new Class<?>[] { ColorSwatch.class };
  }

  public static String getConfigureTypeName() {
    return "Named Colors";
  }

  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof ColorSwatch) {
      ColorSwatch def = (ColorSwatch) b;
      userColors.put(def.getConfigureName(), def);
      def.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
            userColors.remove(evt.getOldValue());
            userColors.put((String) evt.getNewValue(),
                           (ColorSwatch) evt.getSource());
          }
        }
      });
    }
  }

  @Override
  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof ColorSwatch) {
      userColors.remove(((ColorSwatch) b).getConfigureName());
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceImageDefinitions.htm","NamedColors"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  public Color getColorByName(String colorName) {

    ColorSwatch gcolor = getColorSwatch(colorName);
    if (gcolor != null) {
      Color color = gcolor.getColor();
      //if (color != null) {
      return color;
      //}
    }
    return DEFAULT_COLOR;
  }

  public String[] getColorNames() {
    ArrayList<ColorSwatch> a = new ArrayList<>(userColors.values());
    Collections.sort(a);

    ArrayList<String> names =
      new ArrayList<>(a.size() + standardColors.length);

    for (ColorSwatch cs : a) {
      names.add(cs.getConfigureName());
    }

    names.addAll(Arrays.asList(standardColorNames));
    return names.toArray(new String[0]);
  }
}
