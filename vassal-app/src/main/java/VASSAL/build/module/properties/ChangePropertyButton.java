/*
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
import java.util.ArrayList;
import java.util.List;


import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.script.expression.Expression;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import org.apache.commons.lang3.ArrayUtils;

/**
 * Adds a toolbar button that changes the value of a global property
 *
 * @author rkinney
 *
 */
public class ChangePropertyButton extends AbstractToolbarItem implements PropertyChangerConfigurer.Constraints {
  public static final String BUTTON_TEXT = "text"; //NON-NLS
  public static final String BUTTON_TOOLTIP = "tooltip"; //NON-NLS
  public static final String BUTTON_ICON = "icon"; //NON-NLS
  public static final String HOTKEY = "hotkey"; //NON-NLS

  public static final String PROPERTY_CHANGER = "propChanger"; //NON-NLS

  public static final String REPORT_FORMAT = "reportFormat"; //NON-NLS
  public static final String OLD_VALUE_FORMAT = "oldValue"; //NON-NLS
  public static final String NEW_VALUE_FORMAT = "newValue"; //NON-NLS
  public static final String DESCRIPTION_FORMAT = "description"; //NON-NLS

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected FormattedString report = new FormattedString();
  protected GlobalProperty property;
  protected PropertyChangerConfigurer propChangeConfig = new PropertyChangerConfigurer(null, null, this);
  protected FormattedString format = new FormattedString();

  public ChangePropertyButton() {
    setNameKey("");
    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.ChangePropertyButton.change"),
      Resources.getString("Editor.ChangePropertyButton.change"),
      "",
      e -> launch()
    ));

    launch = getLaunchButton(); // for compatibility
  }

  public void launch() {
    final String oldValue = property.getPropertyValue();
    final String newValue = getNewValue();
    if (newValue != null && !newValue.equals(oldValue)) {
      final Command c = property.setPropertyValue(newValue);
      if (report.getFormat().length() > 0) {
        report.setProperty(OLD_VALUE_FORMAT, oldValue);
        report.setProperty(NEW_VALUE_FORMAT, property.getPropertyValue());
        report.setProperty(DESCRIPTION_FORMAT, property.getDescription());
        final Chatter.DisplayText chatCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + report.getLocalizedText());
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
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.report_format"),
      Resources.getString("Editor.ChangePropertyButton.options")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      ReportFormatConfig.class,
      PropChangerOptions.class
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      REPORT_FORMAT,
      PROPERTY_CHANGER
    );
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
      super.setAttribute(key, value);
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
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public void removeFrom(Buildable parent) {
    property.getToolBar().remove(getLaunchButton());
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.html", "ChangePropertyToolbarButton"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void addTo(Buildable parent) {
    property = (GlobalProperty) parent;
    property.getToolBar().add(getLaunchButton());
    propChangeConfig.setName(property.getConfigureName());
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ChangePropertyButton.component_type");
  }

  @Override
  public Component getComponent() {
    return getLaunchButton().getTopLevelAncestor();
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


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(report.getFormat());
  }


  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> l = new ArrayList<>();

    final PropertyChanger propChanger = getPropertyChanger();
    if (propChanger != null) {
      if (propChanger instanceof IncrementProperty) {
        l.add(((IncrementProperty) propChanger).getIncrement());
      }
      else if (propChanger instanceof PropertySetter) {
        l.add(((PropertySetter) propChanger).getRawValue());
      }
      else if (propChanger instanceof PropertyPrompt) {
        final PropertyPrompt pp = (PropertyPrompt) propChanger;
        l.add(pp.getPrompt());
        if (pp instanceof EnumeratedPropertyPrompt) {
          final Expression[] ve = ((EnumeratedPropertyPrompt) pp).getValueExpressions();
          for (final Expression e : ve) {
            if (e == null) {
              continue;
            }
            l.add(e.getExpression());
          }
        }
      }
    }

    return l;
  }
}
