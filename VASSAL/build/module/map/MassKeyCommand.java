/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Sep 25, 2002
 * Time: 10:43:11 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.counters.BooleanAndPieceFilter;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;

/** Adds a button to a map window toolbar.  Hitting the button applies a particular key command to all pieces
 * on that map with a given name.
 */
public class MassKeyCommand extends AbstractConfigurable {
  public static final String DEPRECATED_NAME = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String TOOLTIP = "tooltip";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String HOTKEY = "buttonHotkey";
  public static final String KEY_COMMAND = "hotkey";
  public static final String AFFECTED_PIECE_NAMES = "names";
  public static final String PROPERTIES_FILTER = "filter";
  public static final String REPORT_SINGLE = "reportSingle";
  public static final String REPORT_FORMAT = "reportFormat";
  public static final String CONDITION = "condition";
  private static final String IF_ACTIVE = "If layer is active";
  private static final String IF_INACTIVE = "If layer is inactive";
  private static final String ALWAYS = "Always";
  public static final String CHECK_PROPERTY = "property";
  public static final String CHECK_VALUE = "propValue";

  private LaunchButton launch;
  private KeyStroke stroke = KeyStroke.getKeyStroke(0, 0);
  private String[] names = new String[0];
  private String condition;
  protected String checkProperty;
  protected String checkValue;
  protected String propertiesFilter;
  protected PieceFilter filter;
  private Map map;
  protected GlobalCommand globalCommand = new GlobalCommand();
  protected FormattedString reportFormat = new FormattedString();

  public MassKeyCommand() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        apply();
      }
    };
    launch = new LaunchButton("CTRL", TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, al);
  }

  public void addTo(Buildable parent) {
    map = (Map) parent;
    map.getToolBar().add(launch);
  }

  public void apply() {
    apply(map);
  }

  public void apply(Map m) {
    GameModule.getGameModule().sendAndLog(globalCommand.apply(m,filter));
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    if (condition == null) {
      return new String[]{"Description", "Key Command", "Matching properties", "Tooltip text", "Button text", "Button Icon", "Hotkey",
                          "Suppress individual reports", "Report Format"};
    }
    else {
      return new String[]{"Description", "Key Command", "Matching properties", "Tooltip text", "Button text", "Button Icon", "Hotkey",
                          "Suppress individual reports", "Report Format", "Apply Command"};

    }
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, KEY_COMMAND, PROPERTIES_FILTER, TOOLTIP, BUTTON_TEXT, ICON, HOTKEY,
                        REPORT_SINGLE, REPORT_FORMAT, CONDITION, CHECK_VALUE, CHECK_PROPERTY, AFFECTED_PIECE_NAMES};
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, IF_ACTIVE, IF_INACTIVE};
    }
  }

  public Class[] getAttributeTypes() {
    if (condition == null) {
      return new Class[]{String.class, KeyStroke.class, String.class, String.class, String.class, IconConfig.class, KeyStroke.class,
                         Boolean.class, ReportFormatConfig.class};
    }
    else {
      return new Class[]{String.class, KeyStroke.class, String.class, String.class, String.class, IconConfig.class, KeyStroke.class,
                         Boolean.class, ReportFormatConfig.class, Prompt.class};
    }
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/keyCommand.gif");
    }
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[0]);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (KEY_COMMAND.equals(key)) {
      return HotKeyConfigurer.encode(stroke);
    }
    else if (AFFECTED_PIECE_NAMES.equals(key)) {
      return names == null || names.length == 0 ? null : StringArrayConfigurer.arrayToString(names);
    }
    else if (CHECK_PROPERTY.equals(key)) {
      return propertiesFilter != null ? null : checkProperty;
    }
    else if (CHECK_VALUE.equals(key)) {
      return propertiesFilter != null ? null : checkValue;
    }
    else if (PROPERTIES_FILTER.equals(key)) {
      return propertiesFilter;
    }
    else if (CONDITION.equals(key)) {
      return ALWAYS.equals(condition) ? null : condition;
    }
    else if (REPORT_SINGLE.equals(key)) {
      return String.valueOf(globalCommand.isReportSingle());
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public static String getConfigureTypeName() {
    return "Global Key Command";
  }

  protected LaunchButton getLaunchButton() {
    return launch;
  }

  protected void setLaunchButton(LaunchButton launch) {
    this.launch = launch;
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#GlobalKeyCommand");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(launch);
  }

  private void buildFilter() {
    if (checkValue != null) {
      propertiesFilter = checkProperty + "=" + checkValue;
    }
    if (propertiesFilter != null) {
      filter = PropertiesPieceFilter.parse(propertiesFilter);
    }
    if (filter != null
        && condition != null) {
      filter = new BooleanAndPieceFilter(filter, new PieceFilter() {
        public boolean accept(GamePiece piece) {
          boolean valid = false;
          if (ALWAYS.equals(condition)) {
            valid = true;
          }
          else if (IF_ACTIVE.equals(condition)) {
            valid = Embellishment.getLayerWithMatchingActivateCommand(piece, stroke, true) != null;
          }
          else if (IF_INACTIVE.equals(condition)) {
            valid = Embellishment.getLayerWithMatchingActivateCommand(piece, stroke, false) != null;
          }
          return valid;
        }
      });
    }
  }

  public void setAttribute(String key, Object value) {
    if (DEPRECATED_NAME.equals(key)) {
      setAttribute(NAME, value);
      setAttribute(BUTTON_TEXT, value);
    }
    else if (NAME.equals(key)) {
      setConfigureName((String) value);
      if (launch.getAttributeValueString(TOOLTIP) == null) {
        launch.setAttribute(TOOLTIP, (String) value);
      }
    }
    else if (KEY_COMMAND.equals(key)) {
      if (value instanceof String) {
        value = HotKeyConfigurer.decode((String) value);
      }
      stroke = (KeyStroke) value;
      globalCommand.setKeyStroke(stroke);
    }
    else if (AFFECTED_PIECE_NAMES.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      names = (String[]) value;
      if (names.length == 0) {
        names = null;
      }
      else {
        filter = new PieceFilter() {
          public boolean accept(GamePiece piece) {
            for (int j = 0; j < names.length; ++j) {
              if (Decorator.getInnermost(piece).getName().equals(names[j])) {
                return true;
              }
            }
            return false;
          }
        };
      }
    }
    else if (CHECK_PROPERTY.equals(key)) {
      checkProperty = (String) value;
      buildFilter();
    }
    else if (CHECK_VALUE.equals(key)) {
      checkValue = (String) value;
      buildFilter();
    }
    else if (PROPERTIES_FILTER.equals(key)) {
      propertiesFilter = (String) value;
      buildFilter();
    }
    else if (CONDITION.equals(key)) {
      condition = (String) value;
      buildFilter();
    }
    else if (REPORT_SINGLE.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      globalCommand.setReportSingle(((Boolean) value).booleanValue());
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) value);
      globalCommand.setReportFormat((String) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }
}
