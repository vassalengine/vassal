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

import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JToolBar;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.TemporaryToolBar;
import VASSAL.tools.ToolBarComponent;

/**
 * Adds a global property to a Map or Module
 *
 * @author rkinney
 *
 */
public class GlobalProperty extends AbstractConfigurable implements ToolBarComponent, GameComponent, CommandEncoder, PropertySource, MutableProperty {
  public static final String NAME = "name"; //NON-NLS
  public static final String INITIAL_VALUE = "initialValue"; //NON-NLS
  public static final String DESCRIPTION = "description"; //NON-NLS
  public static final String NUMERIC = "isNumeric"; //NON-NLS
  public static final String MIN_VALUE = "min"; //NON-NLS
  public static final String MAX_VALUE = "max"; //NON-NLS
  public static final String WRAP = "wrap"; //NON-NLS
  protected static final String COMMAND_PREFIX = "GlobalProperty\t"; //NON-NLS
  protected TemporaryToolBar tempToolbar = new TemporaryToolBar();
  protected String description = "";
  protected String initialValue = "";
  protected boolean numeric;
  protected String minValue;
  protected String maxValue;
  protected boolean wrap;
  protected VisibilityCondition numericVisibility;
  protected FormattedString format = new FormattedString();
  protected PropertySource propertySource;
  protected MutableProperty.Impl property = new MutableProperty.Impl("", this);
  protected MutablePropertiesContainer parentContainer;

  public GlobalProperty() {
    numericVisibility = this::isNumeric;
  }

  public GlobalProperty(GlobalProperty p) {
    this();
    setConfigureName(p.getConfigureName());
    description = p.description;
    initialValue = p.initialValue;
    numeric = p.numeric;
    minValue = p.minValue;
    maxValue = p.maxValue;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.GlobalProperty.initial_value"),
      Resources.getString("Editor.description_label"),
      Resources.getString("Editor.GlobalProperty.is_numeric"),
      Resources.getString("Editor.GlobalProperty.minimum_value"),
      Resources.getString("Editor.GlobalProperty.maximum_value"),
      Resources.getString("Editor.GlobalProperty.wrap_around")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class,
      Boolean.class,
      String.class,
      String.class,
      Boolean.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      INITIAL_VALUE,
      DESCRIPTION,
      NUMERIC,
      MIN_VALUE,
      MAX_VALUE,
      WRAP
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
    else if (NUMERIC.equals(key)) {
      numeric = Boolean.TRUE.equals(value) || "true".equals(value); //NON-NLS
    }
    else if (MIN_VALUE.equals(key)) {
      minValue = (String) value;
    }
    else if (MAX_VALUE.equals(key)) {
      maxValue = (String) value;
    }
    else if (WRAP.equals(key)) {
      wrap = Boolean.TRUE.equals(value) || "true".equals(value); //NON-NLS
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
    else if (NUMERIC.equals(key)) {
      return String.valueOf(numeric);
    }
    else if (MIN_VALUE.equals(key)) {
      return String.valueOf(minValue);
    }
    else if (MAX_VALUE.equals(key)) {
      return String.valueOf(maxValue);
    }
    else if (WRAP.equals(key)) {
      return String.valueOf(wrap);
    }
    return null;
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(MIN_VALUE, MAX_VALUE, WRAP).contains(name)) {
      return numericVisibility;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public void removeFrom(Buildable parent) {
    property.removeFromContainer();
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.html"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{ChangePropertyButton.class};
  }

  @Override
  public void addTo(Buildable parent) {
    parentContainer = (MutablePropertiesContainer) parent;
    property.addTo(parentContainer);
    tempToolbar.setDelegate((ToolBarComponent) parent);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    propertySource = (PropertySource) parent;
    setAllAttributesUntranslatable();
  }

  @Override
  public JToolBar getToolBar() {
    return tempToolbar.getToolBar();
  }

  @Override
  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      property.setPropertyValue(initialValue); // Set value back to default for next New Game
    }
  }

  @Override
  public Command getRestoreCommand() {
    return new SetGlobalProperty(this, "", property.getPropertyValue());
  }

  @Override
  public Command decode(String command) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, ';');
    final String prefix = sd.nextToken("");
    if (!prefix.equals(COMMAND_PREFIX)) {
      return null;
    }
    final String propertyId = sd.nextToken("");
    if (!propertyId.equals(getPropertyId())) {
      return null;
    }

    final String newValue = sd.nextToken("");
    final String containerId = sd.nextToken("");
    if (containerId.length() != 0 && !containerId.equals(parentContainer.getMutablePropertiesContainerId())) {
      return null;
    }

    return new SetGlobalProperty(this, property.getPropertyValue(), newValue);
  }

  /**
   * A String that identifies this property in an encoded Command
   * @return propertyId
   */
  protected String getPropertyId() {
    return getConfigureName();
  }

  protected String getContainerId() {
    return parentContainer == null ? "" : parentContainer.getMutablePropertiesContainerId();
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof SetGlobalProperty)) {
      return null;
    }

    final SetGlobalProperty sgp = (SetGlobalProperty) c;
    if (!sgp.getTargetName().equals(getPropertyId())) {
      return null;
    }

    if (!getContainerId().equals(sgp.getProperty().getContainerId())) {
      return null;
    }

    final SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, ';');
    se
      .append(getPropertyId())
      .append(sgp.newValue)
      .append(getContainerId());
    return se.getValue();
  }
  /**
   * Command to pass a new Global property value to other players or into the logfile.
   */
  public static class SetGlobalProperty extends Command {
    protected String newValue;
    protected String oldValue;
    protected GlobalProperty target;
    protected String targetName;

    public SetGlobalProperty(GlobalProperty target, String oldV, String newV) {
      oldValue = oldV;
      newValue = newV;
      this.target = target;
    }

    public String getTargetName() {
      if (target == null) {
        return "";
      }
      else {
        final String targetName = target.getPropertyId();
        return targetName == null ? "" : targetName;
      }
    }

    public GlobalProperty getProperty() {
      return target;
    }

    @Override
    protected void executeCommand() {
      target.property.setPropertyValue(newValue);
    }

    @Override
    protected Command myUndoCommand() {
      return new SetGlobalProperty(target, newValue, oldValue);
    }
  }

  public int getMaxValue() {
    int max = 100;
    if (maxValue != null) {
      format.setFormat(maxValue);
      try {
        max = Integer.parseInt(format.getText(this));
      }
      catch (final NumberFormatException e) {
        // Use default value if formatted string is not a number
      }
    }
    return max;
  }

  public int getMinValue() {
    int min = 0;
    if (minValue != null) {
      format.setFormat(minValue);
      try {
        min = Integer.parseInt(format.getText(this));
      }
      catch (final NumberFormatException e) {
        // Use default value if not a number
      }
    }
    return min;
  }

  public boolean isNumeric() {
    return numeric;
  }

  public boolean isWrap() {
    return wrap;
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
    return Resources.getString("Editor.GlobalProperty.component_type");
  }

  @Override
  public void addMutablePropertyChangeListener(PropertyChangeListener l) {
    property.addMutablePropertyChangeListener(l);
  }

  @Override
  public void removeMutablePropertyChangeListener(PropertyChangeListener l) {
    property.removeMutablePropertyChangeListener(l);
  }

  public void setPropertyName(String name) {
    property.setPropertyName(name);
  }

  @Override
  public Command setPropertyValue(String newValue) {
    return property.setPropertyValue(newValue);
  }

  @Override
  public String getPropertyValue() {
    return property.getPropertyValue();
  }

  @Override
  public MutablePropertiesContainer getParent() {
    return parentContainer;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return List.of(initialValue, minValue, maxValue);
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
