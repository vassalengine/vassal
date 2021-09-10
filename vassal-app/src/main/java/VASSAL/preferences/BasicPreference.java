/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney, Brent Easton
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

package VASSAL.preferences;

import java.awt.Component;

import javax.swing.JLabel;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.i18n.Resources;

/**
 * Base class for a Module Preference. Module preferences are defined within
 * a module and create additional Preferences. Module Preferences can be place
 * on a new preference tab, or on existing preference tabs (tabName). The value
 * of the preference is exposed to counters and components via a Global
 * Property (variableName). A default value (defaultValue) can be specified for
 * when the preference is first accessed by a user.
 */
public abstract class BasicPreference extends AbstractConfigurable {

  public static final String NAME = "name"; //NON-NLS
  public static final String TAB = "tab"; //NON-NLS
  public static final String DESC = "desc"; //NON-NLS
  public static final String DEFAULT = "default"; //NON-NLS

  protected String tabName;
  protected String variableName = "";
  protected MutableProperty.Impl property = new MutableProperty.Impl("", this);

  public BasicPreference() {
    tabName = GameModule.getGameModule().getConfigureName();
    setAttributeTranslatable(NAME, false);
    setAttributeTranslatable(DEFAULT, false);
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {"note", TAB, DESC, NAME, DEFAULT}; //NON-NLS
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {"", Resources.getString("Editor.BasicPreference.tab"), Resources.getString("Editor.BasicPreference.desc"), Resources.getString("Editor.BasicPreference.global"), Resources.getString("Editor.BasicPreference.default")};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      NoteConfig.class,
      String.class,
      String.class,
      String.class,
      getDefaultClass()
    };
  }

  public static class NoteConfig implements ConfigurerFactory {

    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new Configurer(null, "note") { //NON-NLS
        @Override
        public String getValueString() {
          return null;
        }
        @Override
        public void setValue(String s) {
        }
        @Override
        public Component getControls() {
          return new JLabel(Resources.getString("Editor.BasicPreference.note"));
        }
      };
    }

  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      variableName = (String) value;
      property.setPropertyName(variableName);
    }
    else if (TAB.equals(key)) {
      tabName = (String) value;
    }
    else if (DESC.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DEFAULT.equals(key)) {
      setDefaultValue(value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getVariableName();
    }
    else if (TAB.equals(key)) {
      return tabName;
    }
    else if (DESC.equals(key)) {
      return getConfigureName();
    }
    else if (DEFAULT.equals(key)) {
      return getDefaultValue();
    }
    else
      return null;
  }

  public abstract Class<?> getDefaultClass();
  public abstract String getDefaultValue();
  public abstract void setDefaultValue(Object value);
  public abstract Configurer getPreferenceConfigurer();

  @Override
  public void addTo(Buildable b) {
    final GameModule g = GameModule.getGameModule();

    property.addTo(g);

    if (tabName != null && tabName.length() > 0) {
      g.getPrefs().addOption(tabName, getPreferenceConfigurer());
    }

  }

  protected void updateGlobalProperty(String newValue) {
    property.setPropertyValue(newValue);
  }

  @Override
  public void removeFrom(Buildable b) {
    property.removeFromContainer();
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalOptions.html"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public String getDescription() {
    return getConfigureName();
  }

  public String getVariableName() {
    return variableName;
  }

}
