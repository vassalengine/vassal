/*
 * $Id$
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

import javax.swing.JToolBar;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.VisibilityCondition;
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
  public static final String NAME = "name";
  public static final String INITIAL_VALUE = "initialValue";
  public static final String DESCRIPTION = "description";
  public static final String NUMERIC = "isNumeric";
  public static final String MIN_VALUE = "min";
  public static final String MAX_VALUE = "max";
  public static final String WRAP = "wrap";
  protected static final String COMMAND_PREFIX = "GlobalProperty\t";
  protected TemporaryToolBar tempToolbar = new TemporaryToolBar();
  protected String description;
  protected String initialValue;
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
    numericVisibility = new VisibilityCondition() {
      public boolean shouldBeVisible() {
        return isNumeric();
      }
    };
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

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Name",
      "Initial value:  ",
      "Description:  ",
      "Is Numeric",
      "Minimum value:  ",
      "Maximum value:  ",
      "Wrap around"
    };
  }

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
      numeric = Boolean.TRUE.equals(value) || "true".equals(value);
    }
    else if (MIN_VALUE.equals(key)) {
      minValue = (String) value;
    }
    else if (MAX_VALUE.equals(key)) {
      maxValue = (String) value;
    }
    else if (WRAP.equals(key)) {
      wrap = Boolean.TRUE.equals(value) || "true".equals(value);
    }
  }

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

  public VisibilityCondition getAttributeVisibility(String name) {
    if (MIN_VALUE.equals(name) || MAX_VALUE.equals(name) || WRAP.equals(name)) {
      return numericVisibility;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public void removeFrom(Buildable parent) {
    property.removeFromContainer();
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.htm");
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{ChangePropertyButton.class};
  }

  public void addTo(Buildable parent) {
    parentContainer = (MutablePropertiesContainer) parent;
    property.addTo(parentContainer);
    tempToolbar.setDelegate((ToolBarComponent) parent);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    propertySource = (PropertySource) parent;
    setAllAttributesUntranslatable();
  }

  public JToolBar getToolBar() {
    return tempToolbar.getToolBar();
  }

  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      property.setPropertyValue(initialValue); // Set value back to default for next New Game
    }
    return;
  }

  public Command getRestoreCommand() {
    return new SetGlobalProperty(this, "", property.getPropertyValue());
  }

  public Command decode(String command) {
    Command comm = null;
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, ';');
    String prefix = sd.nextToken("");
    if (prefix.equals(COMMAND_PREFIX)) {
      String propertyId = sd.nextToken("");
      String newValue = sd.nextToken("");
      String containerId = sd.nextToken("");
      if (propertyId.equals(getPropertyId())) {
        if (containerId.length() == 0 || containerId.equals(parentContainer.getMutablePropertiesContainerId())) {
          comm = new SetGlobalProperty(this, property.getPropertyValue(), newValue);
        }
      }
    }
    return comm;
  }

  /**
   * A String that identifies this property in an encoded Command
   * @return
   */
  protected String getPropertyId() {
    return getConfigureName();
  }

  protected String getContainerId() {
    return parentContainer == null ? "" : parentContainer.getMutablePropertiesContainerId();
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetGlobalProperty) {
      final SetGlobalProperty sgp = (SetGlobalProperty) c;
      if (sgp.getTargetName().equals(getPropertyId())) {
        if (getContainerId().equals(sgp.getProperty().getContainerId())) {
          SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, ';');
          se.append(getPropertyId());
          se.append(sgp.newValue);
          se.append(getContainerId());
          s = se.getValue();
        }
      }
    }
    return s;
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

    protected void executeCommand() {
      target.property.setPropertyValue(newValue);
    }

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
      catch (NumberFormatException e) {
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
      catch (NumberFormatException e) {
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

  public Object getProperty(Object key) {
    return propertySource == null ? null : propertySource.getProperty(key);
  }

  public Object getLocalizedProperty(Object key) {
    return propertySource == null ? null : propertySource.getLocalizedProperty(key);
  }

  public static String getConfigureTypeName() {
    return "Global Property";
  }

  public void addMutablePropertyChangeListener(PropertyChangeListener l) {
    property.addMutablePropertyChangeListener(l);
  }

  public void removeMutablePropertyChangeListener(PropertyChangeListener l) {
    property.removeMutablePropertyChangeListener(l);
  }

  public void setPropertyName(String name) {
    property.setPropertyName(name);
  }

  public Command setPropertyValue(String newValue) {
    return property.setPropertyValue(newValue);
  }

  public String getPropertyValue() {
    return property.getPropertyValue();
  }

  public MutablePropertiesContainer getParent() {
    return parentContainer;
  }
}
