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
 * Class tht implements a names Font Swatch
 */

package VASSAL.build.module.gamepieceimage;

import java.awt.Font;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;

public class FontStyle extends AbstractConfigurable {

  protected static final String NAME = "name"; //$NON-NLS-1$
  protected static final String STYLE = "style"; //$NON-NLS-1$

  protected OutlineFont font;

  public FontStyle() {
    super();
    setConfigureName(FontManager.DEFAULT);
    font = new OutlineFont(FontManager.DIALOG, Font.PLAIN, 12, false);
  }

  public FontStyle(String name, OutlineFont f) {
    super();
    setConfigureName(name);
    font = f;
  }

  public OutlineFont getFont() {
    return font;
  }

  public boolean isOutline() {
    return font.isOutline();
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
      "Style Name:  ",
      "Font Style:  "
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      FontStyleConfig.class
    };
  }

  public static class FontStyleConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FontConfigurer(key, name, ((FontStyle) c).font);
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, STYLE };
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (STYLE.equals(key)) {
      if (o instanceof String) {
        o = FontConfigurer.decode((String) o);
      }
      font = (OutlineFont) o;
    }

  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (STYLE.equals(key)) {
      return FontConfigurer.encode(font);
    }
    else
      return null;
  }

  public void removeFrom(Buildable parent) {

  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceLayout.htm","NamedColors"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public static String getConfigureTypeName() {
    return "Font Style";
  }

  public void addTo(Buildable parent) {
    setAllAttributesUntranslatable();
  }
}
