/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney
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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;

/**
 * Adds a toolbar button that changes the value of a global property
 *
 * @author rkinney
 *
 */
public class ChangePropertyButton extends AbstractConfigurable implements PropertyChangerConfigurer.Constraints {
  public static final String BUTTON_TEXT = "text";
  public static final String BUTTON_TOOLTIP = "tooltip";
  public static final String BUTTON_ICON = "icon";
  public static final String HOTKEY = "hotkey";
  public static final String PROPERTY_CHANGER = "propChanger";

  public static final String REPORT_FORMAT = "reportFormat";
  public static final String OLD_VALUE_FORMAT = "oldValue";
  public static final String NEW_VALUE_FORMAT = "newValue";
  public static final String DESCRIPTION_FORMAT = "description";

  protected LaunchButton launch;
  protected FormattedString report = new FormattedString();
  protected GlobalProperty property;
  protected PropertyChangerConfigurer propChangeConfig = new PropertyChangerConfigurer(null,null,this);
  protected FormattedString format = new FormattedString();

  public ChangePropertyButton() {
    launch = new LaunchButton("Change", BUTTON_TOOLTIP, BUTTON_TEXT, HOTKEY, BUTTON_ICON, new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
  }

  public void launch() {
    String oldValue = property.getPropertyValue();
    String newValue = getNewValue();
    if (newValue != null && !newValue.equals(oldValue)) {
      Command c = property.setPropertyValue(newValue);
      if (report.getFormat().length() > 0) {
        report.setProperty(OLD_VALUE_FORMAT, oldValue);
        report.setProperty(NEW_VALUE_FORMAT, property.getPropertyValue());
        report.setProperty(DESCRIPTION_FORMAT, property.getDescription());
        Chatter.DisplayText chatCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* "+report.getLocalizedText());
        chatCommand.execute();
        c.append(chatCommand);
      }
      GameModule.getGameModule().sendAndLog(c);
    }
  }

  protected String getNewValue() {
    String newValue = getPropertyChanger().getNewValue(property.getPropertyValue());
    format.setFormat(newValue);
    newValue = format.getText(property);
    return newValue;
  }

  public PropertyChanger getPropertyChanger() {
    return propChangeConfig.getPropertyChanger();
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      "Button text:  ",
      "Tooltip Text:  ",
      "Button icon:  ",
      "Hotkey:  ",
      "Report format:  ",
      "Options:  "
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      Icon.class,
      NamedKeyStroke.class,
      ReportFormatConfig.class,
      PropChangerOptions.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      BUTTON_TEXT,
      BUTTON_TOOLTIP,
      BUTTON_ICON,
      HOTKEY,
      REPORT_FORMAT,
      PROPERTY_CHANGER
    };
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] {OLD_VALUE_FORMAT, NEW_VALUE_FORMAT, DESCRIPTION_FORMAT});
    }
  }

  public static class PropChangerOptions implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return ((ChangePropertyButton)c).propChangeConfig;
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (PROPERTY_CHANGER.equals(key)) {
      if (value instanceof String) {
        propChangeConfig.setValue((String)value);
      }
      else {
        propChangeConfig.setValue(value);
      }
    }
    else if (REPORT_FORMAT.equals(key)) {
      report.setFormat((String) value);
    }
    else {
      if (BUTTON_TEXT.equals(key)) {
        setConfigureName((String)value);
      }
      launch.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (PROPERTY_CHANGER.equals(key)) {
      return propChangeConfig.getValueString();
    }
    else if (REPORT_FORMAT.equals(key)) {
      return report.getFormat();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  @Override
  public void removeFrom(Buildable parent) {
    property.getToolBar().remove(launch);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.htm","ChangePropertyToolbarButton");
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  @Override
  public void addTo(Buildable parent) {
    property = (GlobalProperty) parent;
    property.getToolBar().add(launch);
    propChangeConfig.setName(property.getConfigureName());
  }

  public static String getConfigureTypeName() {
    return "Change-property Toolbar Button";
  }

  @Override
  public Component getComponent() {
    return launch.getTopLevelAncestor();
  }

  @Override
  public int getMaximumValue() {
    return property.getMaxValue();
  }

  @Override
  public int getMinimumValue() {
    return property.getMinValue();
  }

  @Override
  public boolean isNumeric() {
    return property.isNumeric();
  }

  @Override
  public boolean isWrap() {
    return property.isWrap();
  }

  @Override
  public Object getProperty(Object key) {
    return property.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    return property.getLocalizedProperty(key);
  }

  @Override
  public PropertySource getPropertySource() {
    return property;
  }
}
