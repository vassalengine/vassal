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

import VASSAL.build.AbstractFolder;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameState;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.NotNullConfigureName;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ToolBarComponent;

public abstract class AbstractScenarioProperty extends GlobalProperty {

  public static final String HOTKEY = "hotkey"; // NON-NLS

  /** The Tab to which this Scenario Property belongs **/
  protected ScenarioPropertiesOptionTab tab;

  protected NamedKeyStroke hotkey = NamedKeyStroke.NULL_KEYSTROKE;

  @Override
  public void addTo(Buildable parent) {
    validator = new NotNullConfigureName(this);
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder) parent).getNonFolderAncestor();
    }

    // Real parent is the Global Properties component one level up
    if (parent instanceof ScenarioPropertiesOptionTab) {
      tab = (ScenarioPropertiesOptionTab) parent;
      parent = tab.getNonFolderAncestor();
    }

    if (parent != null) {
      parentContainer = (MutablePropertiesContainer) parent;
      property.addTo(parentContainer);
      tempToolbar.setDelegate((ToolBarComponent) parent);
      propertySource = (PropertySource) parent;
    }

    // For copy-pasting purposes this method may end up getting called twice on the same component. Only do this part once.
    final GameModule gm = GameModule.getGameModule();
    final GameState gs = gm.getGameState();
    if (!gs.getGameComponents().contains(this)) {
      gs.addGameComponent(this);
      gm.addCommandEncoder(this);
      setAllAttributesUntranslatable();
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.GlobalProperty.initial_value"),
      Resources.getString("Editor.ScenarioProperties.prompt"),
      Resources.getString("Editor.ScenarioProperties.hotkey_on_change"),
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      getInitialValueClass(),
      String.class,
      NamedKeyStroke.class,
    };
  }

  public abstract Class getInitialValueClass();

  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, INITIAL_VALUE, DESCRIPTION, HOTKEY};
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      super.setAttribute(key, value);
      property.setPropertyValue(initialValue);
    }
    else if (HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      hotkey = (NamedKeyStroke) value;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(hotkey);
    }
    return super.getAttributeValueString(key);
  }

  // Return a Configurer to add to the Scenario options tab
  public abstract Configurer getOptionConfigurer();

  public void processOptionChange(Object newValue) {

    final String oldValue = getPropertyValue();

    if (!oldValue.equals(newValue.toString())) {
      final GameModule gm = GameModule.getGameModule();
      final FormattedString reportFormat = tab.getReportFormat();
      Command c = new SetGlobalProperty(this, oldValue, newValue.toString());
      if (!reportFormat.getFormat().isBlank()) {
        reportFormat.clearProperties();
        reportFormat.setProperty(ScenarioPropertiesOptionTab.REPORT_PROP_NAME, getConfigureName());
        reportFormat.setProperty(ScenarioPropertiesOptionTab.REPORT_PROMPT, getDescription());
        reportFormat.setProperty(ScenarioPropertiesOptionTab.REPORT_TAB, tab.getConfigureName());
        reportFormat.setProperty(ScenarioPropertiesOptionTab.REPORT_OLD_VALUE, oldValue);
        reportFormat.setProperty(ScenarioPropertiesOptionTab.REPORT_NEW_VALUE, newValue.toString());
        final String report = reportFormat.getLocalizedText(this, "Editor.ScenarioOption.report_format");
        c = c.append(new Chatter.DisplayText(gm.getChatter(), "* " + report));
      }

      // Perform the property change and report on our client and save the command
      c.execute();

      // Firing Hotkeys is different, they get executed directly and the Commands sent directly to the hosts.
      // So pause logging so we can capture those commands, and append them to our Command stream so far
      if (!hotkey.isNull()) {
        final boolean loggingPaused = gm.pauseLogging();
        try {
          gm.fireKeyStroke(hotkey);
        }
        finally {
          if (loggingPaused) {
            c.append(gm.resumeLogging());
          }
        }
      }

      // Send the collated Commands as a single un-doable sequence to the log and other clients
      GameModule.getGameModule().sendAndLog(c);
    }
  }

  public NamedKeyStroke getHotkey() {
    return hotkey;
  }

  public void setHotkey(NamedKeyStroke hotkey) {
    this.hotkey = hotkey;
  }
}
