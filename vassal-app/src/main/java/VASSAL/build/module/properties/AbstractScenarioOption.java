/*
 * Copyright (c) 2023 by The VASSAL Development Team
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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.PlayerIdFormattedExpressionConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;

public abstract class AbstractScenarioOption extends AbstractConfigurable {

  public static final String TAB = "tab"; //NON-NLS
  public static final String DESCRIPTION = "description"; //NON-NLS
  public static final String REPORT = "report";
  public static final String HOTKEY = "hotkey";

  public static final String REPORT_PROP_NAME = "propertyName";
  public static final String REPORT_OLD_VALUE = "oldValue";
  public static final String REPORT_NEW_VALUE = "newValue";
  public static final String REPORT_PROMPT = "prompt";
  public static final String REPORT_TAB = "tab";

  protected String tab = "";
  protected String prompt = "";
  protected GlobalProperty property;
  protected Configurer configurer;
  protected String oldValue;
  protected FormattedString reportFormat = new FormattedString("");
  protected NamedKeyStroke hotkey = NamedKeyStroke.NULL_KEYSTROKE;

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ScenarioOption.component_type");
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{TAB, DESCRIPTION, REPORT, HOTKEY};
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (TAB.equals(key)) {
      tab = (String) value;
    }
    else if (DESCRIPTION.equals(key)) {
      prompt = (String) value;
      setConfigureName(prompt);
    }
    else if (REPORT.equals(key)) {
      reportFormat.setFormat((String) value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (TAB.equals(key)) {
      return tab;
    }
    else if (DESCRIPTION.equals(key)) {
      return prompt;
    }
    else if (REPORT.equals(key)) {
      return reportFormat.getFormat();
    }

    return null;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.ScenarioOption.option_tab"),
      Resources.getString("Editor.ScenarioOption.option_text"),
      Resources.getString("Editor.ScenarioOption.report_format"),
      Resources.getString("Editor.ScenarioOption.hotkey")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, String.class, ChangeOptionConfig.class, NamedKeyStroke.class};
  }

  @Override
  public void addTo(Buildable parent) {
    // Scenario Options can only be a child of a Global Property and should have the same name
    if (parent instanceof GlobalProperty) {
      property = (GlobalProperty) parent;
      ScenarioOptions.getInstance().addOption(this);
    }
  }

  @Override
  public void removeFrom(Buildable parent) {
    ScenarioOptions.getInstance().removeOption(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String getTab() {
    return tab;
  }

  public String getPrompt() {
    return prompt;
  }

  public String getPropertyValue() {
    return property == null ? "" : property.getPropertyValue();
  }

  public void saveOldValue() {
    oldValue = getPropertyValue();
  }

  public Configurer getCachedConfigurer() {
    if (configurer == null) {
      configurer = getOptionConfigurer();
      configurer.addPropertyChangeListener(e -> processOptionChange((String) e.getNewValue()));
    }
    return configurer;
  }

  public void processOptionChange(String newValue) {
    final GameModule gm = GameModule.getGameModule();

    saveOldValue();
    if (!newValue.equals(oldValue)) {
      Command c = new GlobalProperty.SetGlobalProperty(property, oldValue, newValue);
      if (!reportFormat.getFormat().isBlank()) {
        reportFormat.clearProperties();
        reportFormat.setProperty(REPORT_PROP_NAME, property.getConfigureName());
        reportFormat.setProperty(REPORT_PROMPT, prompt);
        reportFormat.setProperty(REPORT_TAB, tab);
        reportFormat.setProperty(REPORT_OLD_VALUE, oldValue);
        reportFormat.setProperty(REPORT_NEW_VALUE, newValue);
        final String report = reportFormat.getLocalizedText(this, "Editor.ScenarioOption.report_format");
        c = c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + report));
      }

      // Perform the property change and report on our client and save the command
      c.execute();

      // Firing Hotkeys is different, they get executed directly and the Commands sent directly to the hosts.
      // So pause logging so we can capture those commands, and append them to our Command stream so far
      if (!hotkey.isNull()) {
        gm.pauseLogging();
        gm.fireKeyStroke(hotkey);
        c = c.append(gm.resumeLogging());
      }

      // Send the collated Commands as a single un-doable sequence to the log and other clients
      GameModule.getGameModule().sendAndLog(c);
    }
  }

  public abstract Configurer getOptionConfigurer();

  public static class ChangeOptionConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedExpressionConfigurer(key, name, new String[]{
        REPORT_PROP_NAME,
        REPORT_PROMPT,
        REPORT_OLD_VALUE,
        REPORT_NEW_VALUE,
        REPORT_TAB});
    }
  }
}
