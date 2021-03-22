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

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Element;

/**
 * Container for definitions of Generic Color Definitions
 */
public class FontManager extends AbstractConfigurable {

  protected static FontManager instance;

  public static FontManager getFontManager() {
    return instance;
  }

  protected Map<String, FontStyle> fontStyles = new HashMap<>();

  public static final String DIALOG = "Dialog"; //$NON-NLS-1$
  public static final String SERIF = "Serif"; //$NON-NLS-1$
  public static final String SANS_SERIF = "SanSerif"; //$NON-NLS-1$
  public static final String DIALOG_INPUT = "DialogInput"; //$NON-NLS-1$
  public static final String MONOSPACED = "Monospaced"; //$NON-NLS-1$

  public static final String DEFAULT = "Default"; //$NON-NLS-1$
  public static final OutlineFont DEFAULT_FONT = new OutlineFont(DIALOG, Font.PLAIN, 12, false);
  public static final FontStyle DEFAULT_STYLE = new FontStyle();

//  public static final String[] ALLOWABLE_FONTS = new String[] { DIALOG, DIALOG_INPUT, MONOSPACED, SANS_SERIF, SERIF };
  public static final String[] ALLOWABLE_FONTS = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();

  @Override
  public void build(Element e) {
    super.build(e);

    if (fontStyles.get(DEFAULT) == null) {
      addChild(new FontStyle(DEFAULT, DEFAULT_FONT));
    }
  }

  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
  }

  protected FontStyle getFontStyle(String name) {
    final FontStyle fs = fontStyles.get(name);
    return fs == null ? DEFAULT_STYLE : fs;
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
    return new Class<?>[] { FontStyle.class };
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.FontManager.component_type");
  }

  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof FontStyle) {
      final FontStyle def = (FontStyle) b;
      fontStyles.put(def.getConfigureName(), def);
      def.addPropertyChangeListener(evt -> {
        if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
          fontStyles.remove(evt.getOldValue());
          fontStyles.put((String) evt.getNewValue(),
                         (FontStyle) evt.getSource());
        }
      });
    }
  }

  @Override
  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof ColorSwatch) {
      fontStyles.remove(((ColorSwatch) b).getConfigureName());
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceImageDefinitions.html", "FontStyles"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  public String[] getFontNames() {
    final ArrayList<String> names = new ArrayList<>(fontStyles.size());
    for (final FontStyle fs : fontStyles.values()) {
      names.add(fs.getConfigureName());
    }
    return names.toArray(new String[0]);
  }
}
