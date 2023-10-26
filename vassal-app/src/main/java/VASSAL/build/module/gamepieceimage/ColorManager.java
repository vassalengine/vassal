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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

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

  protected static Color[] standardColors = {
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

  protected static String[] standardColorNames = {
    "WHITE",      // NON-NLS
    "GRAY",       // NON-NLS
    "BLACK",      // NON-NLS
    "CLEAR",      // NON-NLS
    "RED",        // NON-NLS
    "GREEN",      // NON-NLS
    "BLUE",       // NON-NLS
    "ORANGE",     // NON-NLS
    "PINK",       // NON-NLS
    "CYAN",       // NON-NLS
    "MAGENTA",    // NON-NLS
    "YELLOW",     // NON-NLS
    "LIGHT GRAY", // NON-NLS
    "DARK GRAY"   // NON-NLS
  };

  protected static String[] standardColorKeys = {
    "Editor.ColorManager.white",
    "Editor.ColorManager.gray",
    "Editor.ColorManager.black",
    "Editor.ColorManager.clear",
    "Editor.ColorManager.red",
    "Editor.ColorManager.green",
    "Editor.ColorManager.blue",
    "Editor.ColorManager.orange",
    "Editor.ColorManager.pink",
    "Editor.ColorManager.cyan",
    "Editor.ColorManager.magenta",
    "Editor.ColorManager.yellow",
    "Editor.ColorManager.light_gray",
    "Editor.ColorManager.dark_gray"
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
  protected Map<String, ColorSwatch> userColors =
    new HashMap<>();

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
    for (final ColorSwatch cs : userColors.values()) {
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
    instance = this;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { ColorSwatch.class };
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ColorManager.component_type");
  }

  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof ColorSwatch) {
      final ColorSwatch def = (ColorSwatch) b;
      userColors.put(def.getConfigureName(), def);
      def.addPropertyChangeListener(evt -> {
        if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
          userColors.remove(evt.getOldValue());
          userColors.put((String) evt.getNewValue(),
                         (ColorSwatch) evt.getSource());
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
    return HelpFile.getReferenceManualPage("GamePieceImageDefinitions.html", "NamedColors"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  public Color getColorByName(String colorName) {

    final ColorSwatch gcolor = getColorSwatch(colorName);
    if (gcolor != null) {
      return gcolor.getColor();
    }
    return DEFAULT_COLOR;
  }

  public String[] getColorNames() {
    final ArrayList<ColorSwatch> a = new ArrayList<>(userColors.values());
    Collections.sort(a);

    final ArrayList<String> names =
      new ArrayList<>(a.size() + standardColors.length);

    for (final ColorSwatch cs : a) {
      names.add(cs.getConfigureName());
    }

    names.addAll(Arrays.asList(standardColorNames));
    return names.toArray(new String[0]);
  }

  public String[] getColorDisplayNames() {
    final ArrayList<ColorSwatch> a = new ArrayList<>(userColors.values());
    Collections.sort(a);

    final ArrayList<String> names =
      new ArrayList<>(a.size() + standardColors.length);

    for (final ColorSwatch cs : a) {
      names.add(cs.getConfigureName());
    }

    for (final String key : standardColorKeys) {
      names.add(Resources.getString(key));
    }
    return names.toArray(new String[0]);
  }

  @Override
  public boolean isUnique() {
    return true;
  }

  @Override
  public boolean isMandatory() {
    return true;
  }
}
