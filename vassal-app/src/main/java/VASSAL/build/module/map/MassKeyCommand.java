/*
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

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.gamepieceimage.StringEnumConfigurer;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.counters.BooleanAndPieceFilter;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.ToolBarComponent;

/**
 * Adds a button to a map window toolbar. Hitting the button applies a particular key command to all pieces on that map
 * with a given name.
 */
public class MassKeyCommand extends AbstractConfigurable
                            implements RecursionLimiter.Loopable {
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
  public static final String DECK_COUNT = "deckCount";
  private static final String IF_ACTIVE = "If layer is active";
  private static final String IF_INACTIVE = "If layer is inactive";
  private static final String ALWAYS = "Always";
  public static final String CHECK_PROPERTY = "property";
  public static final String CHECK_VALUE = "propValue";
  public static final String SINGLE_MAP = "singleMap";
  protected LaunchButton launch;
  protected NamedKeyStroke stroke = new NamedKeyStroke();
  protected String[] names = new String[0];
  protected String condition;
  protected String checkProperty;
  protected String checkValue;
  protected PropertyExpression propertiesFilter = new PropertyExpression();
  protected PropertySource propertySource;
  protected PieceFilter filter;
  protected Map map;
  protected GlobalCommand globalCommand = new GlobalCommand(this);
  protected FormattedString reportFormat = new FormattedString();
  protected boolean singleMap = true;

  public MassKeyCommand() {
    ActionListener al = new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        apply();
      }
    };
    launch = new LaunchButton("CTRL", TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, al);
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof Map) {
      map = (Map) parent;
    }
    if (parent instanceof ToolBarComponent) {
      ((ToolBarComponent)parent).getToolBar().add(launch);
    }
    if (parent instanceof PropertySource) {
      propertySource = (PropertySource) parent;
    }
    setAttributeTranslatable(NAME, false);
    globalCommand.setPropertySource(propertySource);
  }

  public void apply() {
    buildFilter();
    if (singleMap) {
      GameModule.getGameModule().sendAndLog(globalCommand.apply(map, getFilter()));
    }
    else {
      final List<Map> l = Map.getMapList();
      GameModule.getGameModule().sendAndLog(
          globalCommand.apply(l.toArray(new Map[0]), getFilter()));
    }
  }

  public void setPropertySource(PropertySource source) {
    propertySource = source;
    globalCommand.setPropertySource(source);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeDescriptions() {
    if (condition == null) {
      return new String[]{
        Resources.getString(Resources.DESCRIPTION),
        Resources.getString("Editor.MassKey.key"), //$NON-NLS-1$
        Resources.getString("Editor.MassKey.match"), //$NON-NLS-1$
        Resources.getString("Editor.MassKey.counters"), //$NON-NLS-1$
        Resources.getString("Editor.MassKey.deck_content"), //$NON-NLS-1$
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
        Resources.getString("Editor.MassKey.suppress"), //$NON-NLS-1$
        Resources.getString("Editor.report_format"), //$NON-NLS-1$
      };
    }
    else {
      // Backward compatibility
      return new String[]{
        Resources.getString(Resources.DESCRIPTION),
        Resources.getString("Editor.MassKey.key"), //$NON-NLS-1$
        Resources.getString("Editor.MassKey.match"), //$NON-NLS-1$
        Resources.getString("Editor.MassKey.counters"), //$NON-NLS-1$
        Resources.getString("Editor.MassKey.deck_content"), //$NON-NLS-1$
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
        Resources.getString("Editor.MassKey.suppress"), //$NON-NLS-1$
        Resources.getString("Editor.report_format"), //$NON-NLS-1$
        Resources.getString("Editor.MassKey.apply"), //$NON-NLS-1$
      };
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      KEY_COMMAND,
      PROPERTIES_FILTER,
      SINGLE_MAP,
      DECK_COUNT,
      BUTTON_TEXT,
      TOOLTIP,
      ICON,
      HOTKEY,
      REPORT_SINGLE,
      REPORT_FORMAT,
      CONDITION,
      CHECK_VALUE,
      CHECK_PROPERTY,
      AFFECTED_PIECE_NAMES
    };
  }

  public static class Prompt extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, IF_ACTIVE, IF_INACTIVE};
    }
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    if (condition == null) {
      return new Class<?>[]{
        String.class,
        NamedKeyStroke.class,
        PropertyExpression.class,
        Boolean.class,
        DeckPolicyConfig.class,
        String.class,
        String.class,
        IconConfig.class,
        NamedKeyStroke.class,
        Boolean.class,
        ReportFormatConfig.class
      };
    }
    else {
      // Backward compatibility
      return new Class<?>[]{
        String.class,
        NamedKeyStroke.class,
        String.class,
        Boolean.class,
        DeckPolicyConfig.class,
        String.class,
        String.class,
        IconConfig.class,
        NamedKeyStroke.class,
        Boolean.class,
        ReportFormatConfig.class,
        Prompt.class
      };
    }
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/keyCommand.gif");
    }
  }
  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[0]);
    }
  }
  public static class DeckPolicyConfig extends Configurer implements ConfigurerFactory {
    protected static final String FIXED = "Fixed number of pieces";
    protected static final String NONE = "No pieces";
    protected static final String ALL = "All pieces";
    protected IntConfigurer intConfig;
    protected StringEnumConfigurer typeConfig;
    protected JLabel prompt;
    protected Box controls;

    public DeckPolicyConfig() {
      super(null, "");
      typeConfig = new StringEnumConfigurer(null, "", new String[]{ALL, NONE, FIXED});
      intConfig = new IntConfigurer(null, "");
      controls = Box.createHorizontalBox();
      prompt = new JLabel("Within a Deck, apply to:  ");
      controls.add(prompt);
      controls.add(typeConfig.getControls());
      controls.add(intConfig.getControls());
      PropertyChangeListener l = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          intConfig.getControls().setVisible(FIXED.equals(typeConfig.getValueString()));
          Window w = SwingUtilities.getWindowAncestor(intConfig.getControls());
          if (w != null) {
            w.pack();
          }
        }
      };
      PropertyChangeListener l2 = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          setValue(getIntValue());
        }
      };
      typeConfig.addPropertyChangeListener(l);
      typeConfig.addPropertyChangeListener(l2);
      intConfig.addPropertyChangeListener(l2);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getValueString() {
      return String.valueOf(getIntValue());
    }

    public int getIntValue() {
      String type = typeConfig.getValueString();
      if (ALL.equals(type)) {
        return -1;
      }
      else if (NONE.equals(type)) {
        return 0;
      }
      else {
        return intConfig.getIntValue(1);
      }
    }

    @Override
    public void setValue(Object o) {
      if (typeConfig != null) {
        typeConfig.setFrozen(true);
        intConfig.setFrozen(true);
        if (o instanceof Integer) {
          Integer i = (Integer) o;
          switch (i) {
          case 0:
            typeConfig.setValue(NONE);
            intConfig.setValue(1);
            break;
          case -1:
            typeConfig.setValue(ALL);
            intConfig.setValue(1);
            break;
          default:
            typeConfig.setValue(FIXED);
            intConfig.setValue(i);
          }
          intConfig.getControls().setVisible(FIXED.equals(typeConfig.getValueString()));
        }
      }
      super.setValue(o);
      if (typeConfig != null) {
        typeConfig.setFrozen(false);
        intConfig.setFrozen(false);
      }
    }

    @Override
    public void setValue(String s) {
      if (s != null) {
        setValue(Integer.valueOf(s));
      }
    }

    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      setName(name);
      this.key = key;
      return this;
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (KEY_COMMAND.equals(key)) {
      return NamedHotKeyConfigurer.encode(stroke);
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
      return propertiesFilter.getExpression();
    }
    else if (CONDITION.equals(key)) {
      return ALWAYS.equals(condition) ? null : condition;
    }
    else if (REPORT_SINGLE.equals(key)) {
      return String.valueOf(globalCommand.isReportSingle());
    }
    else if (DECK_COUNT.equals(key)) {
      return String.valueOf(globalCommand.getSelectFromDeck());
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (SINGLE_MAP.equals(key)) {
      return String.valueOf(singleMap);
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

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "GlobalKeyCommand");
  }

  @Override
  public void removeFrom(Buildable parent) {
    if (parent instanceof ToolBarComponent) {
      ((ToolBarComponent)parent).getToolBar().remove(launch);
    }
  }

  public PieceFilter getFilter() {
    buildFilter();
    return filter;
  }

  private void buildFilter() {
    if (checkValue != null) {
      propertiesFilter.setExpression(checkProperty + "=" + checkValue);
    }
    if (propertiesFilter != null) {
      filter = propertiesFilter.getFilter(propertySource);
    }
    if (filter != null && condition != null) {
      filter = new BooleanAndPieceFilter(filter, new PieceFilter() {
        @Override
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

  @Override
  public void setAttribute(String key, Object value) {
    if (DEPRECATED_NAME.equals(key)) {
      setAttribute(NAME, value);
      setAttribute(BUTTON_TEXT, value);
    }
    else if (NAME.equals(key)) {
      setConfigureName((String) value);
      if (launch.getAttributeValueString(TOOLTIP) == null) {
        launch.setAttribute(TOOLTIP, value);
      }
    }
    else if (KEY_COMMAND.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      stroke = (NamedKeyStroke) value;
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
          @Override
          public boolean accept(GamePiece piece) {
            for (String s : names) {
              if (Decorator.getInnermost(piece).getName().equals(s)) {
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
    }
    else if (CHECK_VALUE.equals(key)) {
      checkValue = (String) value;
    }
    else if (PROPERTIES_FILTER.equals(key)) {
      propertiesFilter.setExpression((String) value);
    }
    else if (CONDITION.equals(key)) {
      condition = (String) value;
    }
    else if (REPORT_SINGLE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      globalCommand.setReportSingle((Boolean) value);
    }
    else if (DECK_COUNT.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      globalCommand.setSelectFromDeck((Integer) value);
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) value);
      globalCommand.setReportFormat((String) value);
    }
    else if (SINGLE_MAP.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      singleMap = ((Boolean) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  // Implement Loopable
  @Override
  public String getComponentName() {
    return getConfigureName();
  }

  @Override
  public String getComponentTypeName() {
    return getConfigureTypeName();
  }

}
