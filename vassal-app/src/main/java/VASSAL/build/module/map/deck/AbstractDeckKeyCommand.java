/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AbstractFolder;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedExpressionConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;

import java.util.List;

/**
 * Base class for additional Menu items (with Hotkeys) that can be added to a DrawPile/Deck
 */
public abstract class AbstractDeckKeyCommand extends AbstractConfigurable implements DeckKeyCommand {
  public static final String DESCRIPTION = "description";
  public static final String MENU_TEXT = "menuText";
  public static final String REPORT_FORMAT = "reportFormat";
  public static final String HOTKEY = "hotkey";
  public static final String COMMAND_NAME = "commandName";
  public static final String DECK_NAME = "deckName";

  protected String description = "";
  protected NamedKeyStroke keyStroke = NamedKeyStroke.NULL_KEYSTROKE;
  protected FormattedString reportFormat = new FormattedString();

  public String getDescription() {
    return description;
  }

  public NamedKeyStroke getKeyStroke() {
    return keyStroke;
  }

  public FormattedString getReportFormat() {
    return reportFormat;
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {DESCRIPTION, MENU_TEXT, HOTKEY, REPORT_FORMAT};
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
    else if (HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      keyStroke = (NamedKeyStroke) value;
    }
    else if (MENU_TEXT.equals(key)) {
      setConfigureName((String) value);
    }
    else if (REPORT_FORMAT.equals(key)) {
      if (value instanceof String) {
        reportFormat.setFormat((String) value);
      }
      else {
        reportFormat = (FormattedString) value;
      }
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (DESCRIPTION.equals(key)) {
      return description;
    }
    else if (HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(keyStroke);
    }
    else if (MENU_TEXT.equals(key)) {
      return getConfigureName();
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    return null;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.description_label"),
      Resources.getString("Editor.menu_command"),
      Resources.getString("Editor.hotkey_label"),
      Resources.getString("Editor.report_format")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] {String.class, String.class, NamedKeyStroke.class, DeckReportFormatConfig.class};
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder)parent).getNonFolderAncestor();
    }
  }

  @Override
  public void removeFrom(Buildable parent) {
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder)parent).getNonFolderAncestor();
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return List.of(keyStroke);
  }


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(getComponentName());
  }


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(reportFormat.getFormat());
  }

  public static class DeckReportFormatConfig implements TranslatableConfigurerFactory {

    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedExpressionConfigurer(key, name, ((DeckKeyCommand) c).getAdditionalReportProperties());
    }
  }

  @Override
  public String[] getAdditionalReportProperties() {
    return new String[]{DECK_NAME, COMMAND_NAME};
  }
}
