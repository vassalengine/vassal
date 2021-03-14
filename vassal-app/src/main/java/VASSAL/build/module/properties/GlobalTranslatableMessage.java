/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module.properties;

import java.util.List;

import javax.swing.JToolBar;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;
import VASSAL.tools.TemporaryToolBar;
import VASSAL.tools.ToolBarComponent;

/**
 * Adds a global property to a Map or Module
 *
 * @author rkinney
 *
 */
public class GlobalTranslatableMessage extends AbstractConfigurable implements ToolBarComponent, GameComponent, PropertySource, TranslatableString {
  public static final String NAME = "name"; //NON-NLS
  public static final String INITIAL_VALUE = "initialValue"; //NON-NLS
  public static final String DESCRIPTION = "description"; //NON-NLS
  protected static final String COMMAND_PREFIX = "GlobalTranslatable\t"; //NON-NLS
  protected TemporaryToolBar tempToolbar = new TemporaryToolBar();
  protected String description = "";
  protected String initialValue = "";
  protected FormattedString format = new FormattedString();
  protected PropertySource propertySource;
  protected TranslatableString.Impl property = new TranslatableString.Impl("", this);
  protected TranslatableStringContainer parentContainer;

  public GlobalTranslatableMessage() {

  }

  public GlobalTranslatableMessage(GlobalTranslatableMessage p) {
    this();
    setConfigureName(p.getConfigureName());
    description = p.description;
    initialValue = p.initialValue;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.GlobalTranslatableMessage.message"),
      Resources.getString("Editor.description_label"),
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class,
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      INITIAL_VALUE,
      DESCRIPTION,
    };
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      property.setPropertyName(getConfigureName());
    }
    else if (INITIAL_VALUE.equals(key)) {
      initialValue = (String) value;
      if (initialValue == null) {
        initialValue = "";
      }
      property.setPropertyValue(initialValue);
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (INITIAL_VALUE.equals(key)) {
      return initialValue;
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    return null;
  }

  @Override
  public void removeFrom(Buildable parent) {
    property.removeFromContainer();
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalTranslatableMessages.html"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void addTo(Buildable parent) {
    parentContainer = (TranslatableStringContainer) parent;
    property.addTo(parentContainer);
    tempToolbar.setDelegate((ToolBarComponent) parent);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    propertySource = (PropertySource) parent;

    setAllAttributesUntranslatable();
    setAttributeTranslatable(INITIAL_VALUE, true);
  }

  @Override
  public JToolBar getToolBar() {
    return tempToolbar.getToolBar();
  }

  @Override
  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      property.setPropertyValue(initialValue); // Set value to default for next New Game
    }
  }

  @Override
  public Command getRestoreCommand() {
    return new NullCommand();
  }


  /**
   * A String that identifies this property in an encoded Command
   * @return propertyId
   */
  protected String getPropertyId() {
    return getConfigureName();
  }

  protected String getContainerId() {
    return parentContainer == null ? "" : parentContainer.getTranslatableStringContainerId();
  }

  public String getDescription() {
    return description;
  }

  @Override
  public Object getProperty(Object key) {
    return propertySource == null ? null : propertySource.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    return propertySource == null ? null : propertySource.getLocalizedProperty(key);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GlobalTranslatableMessage.component_type");
  }

  public void setPropertyName(String name) {
    property.setPropertyName(name);
  }

  @Override
  public void setPropertyValue(String newValue) {
    property.setPropertyValue(newValue);
  }

  @Override
  public String getPropertyValue() {
    return property.getPropertyValue();
  }

  @Override
  public TranslatableStringContainer getParent() {
    return parentContainer;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(initialValue);
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(property.getName());
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(description);
  }
}
