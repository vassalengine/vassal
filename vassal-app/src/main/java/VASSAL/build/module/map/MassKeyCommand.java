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

import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.counters.GlobalCommandTarget;
import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.List;

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BooleanAndPieceFilter;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.ToolBarComponent;
import net.miginfocom.swing.MigLayout;

/**
 * Adds a button to a map window toolbar. Hitting the button applies a particular key command to all pieces on that map
 * with a given name.
 */
public class MassKeyCommand extends AbstractConfigurable
                            implements RecursionLimiter.Loopable {
  public static final String DEPRECATED_NAME = "text"; // NON-NLS
  public static final String NAME = "name"; // NON-NLS
  public static final String ICON = "icon"; // NON-NLS
  public static final String TOOLTIP = "tooltip"; // NON-NLS
  public static final String BUTTON_TEXT = "buttonText"; // NON-NLS
  public static final String HOTKEY = "buttonHotkey"; // NON-NLS
  public static final String KEY_COMMAND = "hotkey"; // NON-NLS
  public static final String AFFECTED_PIECE_NAMES = "names"; // NON-NLS
  public static final String PROPERTIES_FILTER = "filter"; // NON-NLS
  public static final String REPORT_SINGLE = "reportSingle"; // NON-NLS
  public static final String REPORT_FORMAT = "reportFormat"; // NON-NLS
  public static final String CONDITION = "condition"; // NON-NLS
  public static final String DECK_COUNT = "deckCount"; // NON-NLS
  private static final String IF_ACTIVE = "If layer is active"; // NON-NLS
  private static final String IF_INACTIVE = "If layer is inactive"; // NON-NLS
  private static final String ALWAYS = "Always"; // NON-NLS
  public static final String CHECK_PROPERTY = "property"; // NON-NLS
  public static final String CHECK_VALUE = "propValue"; // NON-NLS
  public static final String SINGLE_MAP = "singleMap"; // NON-NLS
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

  public static final String TARGET   = "target"; //NON-NLS
//  public static final String TARGET_TYPE   = "targetType"; //NON-NLS
//  public static final String TARGET_MAP    = "targetMap"; //NON-NLS
//  public static final String TARGET_BOARD  = "targetBoard"; //NON-NLS
//  public static final String TARGET_ZONE   = "targetZone"; //NON-NLS
//  public static final String TARGET_REGION = "targetRegion"; //NON-NLS
//  public static final String TARGET_X      = "targetX"; //NON-NLS
//  public static final String TARGET_Y      = "targetY"; //NON-NLS
//  public static final String TARGET_EXACT_MATCH = "targetExactMatch"; //NON-NLS
//  public static final String TARGET_PROPERTY    = "targetProperty"; //NON-NLS
//  public static final String TARGET_VALUE       = "targetValue"; //NON-NLS

//  protected GlobalCommand.GlobalCommandTarget targetType = GlobalCommand.GlobalCommandTarget.GAME;
//  protected Expression targetMap = Expression.createExpression("");
//  protected Expression targetBoard = Expression.createExpression("");
//  protected Expression targetZone = Expression.createExpression("");
//  protected Expression targetRegion = Expression.createExpression("");
//  protected int targetX = 0;
//  protected int targetY = 0;
//  protected boolean targetExactMatch = false;
//  protected Expression targetProperty = Expression.createExpression("");
//  protected Expression targetValue = Expression.createExpression("");

  protected GlobalCommandTarget target = new GlobalCommandTarget (false);

  public MassKeyCommand() {
    ActionListener al = e -> apply();
    launch = new LaunchButton(Resources.getString("Editor.GlobalKeyCommand.button_name"), TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, al);
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

    // FIXME Pass the GlobalCommandTarget to GlobalCommand and let it do all the work.
    //
//    globalCommand.setTargetExactMatch(targetExactMatch);
//    if (targetExactMatch) {
//      globalCommand.setTargetProperty(targetProperty.tryEvaluate(propertySource));
//      globalCommand.setTargetValue(targetValue.tryEvaluate(propertySource));
//    }
//
//    globalCommand.setTargetType(targetType);
//
//    if (targetType != GlobalCommand.GlobalCommandTarget.GAME) {
//      globalCommand.setTargetMap(targetMap.tryEvaluate(propertySource));
//    }
//
//    switch (targetType) {
//    case ZONE:
//      globalCommand.setTargetZone(targetZone.tryEvaluate(propertySource));
//      break;
//
//    case REGION:
//      globalCommand.setTargetRegion(targetRegion.tryEvaluate(propertySource));
//      break;
//
//    case XY:
//      globalCommand.setTargetBoard(targetBoard.tryEvaluate(propertySource));
//      globalCommand.setTargetX(targetX);
//      globalCommand.setTargetY(targetY);
//      break;
//    }

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
        Resources.getString(Resources.DESCRIPTION),                             // Description
        Resources.getString("Editor.keyboard_command"), //$NON-NLS-1$             // Key Command
        Resources.getString("Editor.MassKey.counters"), //$NON-NLS-1$        // Apply to counters on this map only

        Resources.getString ("Editor.GlobalKeyCommand.pre_select"),
//        Resources.getString("Editor.GlobalKeyCommand.restrict_matches_to"),     // Restrict by location? (fast match)
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_map"),         // Restrict to map
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_board"),       // Restrict to board
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_zone"),        // Restrict to zone
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_region"),      // Restrict to region
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_x_position"),  // Restrict to X position
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_y_position"),  // Restrict to Y position
//        Resources.getString("Editor.GlobalKeyCommand.exact_match"),             // Exact property match (fast match)
//        Resources.getString("Editor.GlobalKeyCommand.exact_property"),          // Property name for fast match
//        Resources.getString("Editor.GlobalKeyCommand.exact_value"),             // Property value for fast match

        Resources.getString("Editor.MassKey.match"), //$NON-NLS-1$           // Match properties
        Resources.getString("Editor.MassKey.deck_content"), //$NON-NLS-1$    // Apply to pieces in deck
        Resources.getString(Resources.BUTTON_TEXT),                             // Button text
        Resources.getString(Resources.TOOLTIP_TEXT),                            // Button tooltip
        Resources.getString(Resources.BUTTON_ICON),                             // Button icon
        Resources.getString(Resources.HOTKEY_LABEL),                            // Hotkey
        Resources.getString("Editor.MassKey.suppress"), //$NON-NLS-1$       // Suppress individual reports?
        Resources.getString("Editor.report_format"), //$NON-NLS-1$          // Report format
      };
    }
    else {
      // Backward compatibility
      return new String[]{
        Resources.getString(Resources.DESCRIPTION),                             // Description
        Resources.getString("Editor.MassKey.key"), //$NON-NLS-1$             // Key Command
        Resources.getString("Editor.MassKey.counters"), //$NON-NLS-1$        // Apply to counters on this map only

        Resources.getString ("Editor.GlobalKeyCommand.pre_select"),
//        Resources.getString("Editor.GlobalKeyCommand.restrict_matches_to"),     // Restrict by location? (fast match)
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_map"),         // Restrict to map
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_board"),       // Restrict to board
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_zone"),        // Restrict to zone
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_region"),      // Restrict to region
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_x_position"),  // Restrict to X position
//        Resources.getString("Editor.GlobalKeyCommand.restrict_to_y_position"),  // Restrict to Y position
//        Resources.getString("Editor.GlobalKeyCommand.exact_match"),             // Exact property match (fast match)
//        Resources.getString("Editor.GlobalKeyCommand.exact_property"),          // Property name for fast match
//        Resources.getString("Editor.GlobalKeyCommand.exact_value"),             // Property value for fast match

        Resources.getString("Editor.MassKey.match"), //$NON-NLS-1$           // Match properties
        Resources.getString("Editor.MassKey.deck_content"), //$NON-NLS-1$    // Apply to pieces in deck
        Resources.getString(Resources.BUTTON_TEXT),                             // Button text
        Resources.getString(Resources.TOOLTIP_TEXT),                            // Button tooltip
        Resources.getString(Resources.BUTTON_ICON),                             // Button icon
        Resources.getString(Resources.HOTKEY_LABEL),                            // Hotkey
        Resources.getString("Editor.MassKey.suppress"), //$NON-NLS-1$       // Suppress individual reports?
        Resources.getString("Editor.report_format"), //$NON-NLS-1$          // Report format
        Resources.getString("Editor.MassKey.apply"), //$NON-NLS-1$          // Legacy condition
      };
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,                                 // Description
      KEY_COMMAND,                          // Key Command
      SINGLE_MAP,                           // Apply to counters on this map only

      TARGET,
//      TARGET_TYPE,        // Restrict by location? (fast match)
//      TARGET_MAP,         // Restrict to map
//      TARGET_BOARD,       // Restrict to board
//      TARGET_ZONE,        // Restrict to zone
//      TARGET_REGION,      // Restrict to region
//      TARGET_X,           // Restrict to X position
//      TARGET_Y,           // Restrict to Y position
//      TARGET_EXACT_MATCH, // Exact property match (fast match)
//      TARGET_PROPERTY,    // Property name for fast match
//      TARGET_VALUE,       // Property value for fast match

      PROPERTIES_FILTER,                    // Match properties
      DECK_COUNT,                           // Apply to pieces in deck
      BUTTON_TEXT,                          // Button text
      TOOLTIP,                              // Button tooltip
      ICON,                                 // Button icon
      HOTKEY,                               // Hotkey
      REPORT_SINGLE,                        // Suppress individual reports?
      REPORT_FORMAT,                        // Report format
      CONDITION,                            // Legacy condition
      CHECK_VALUE,                          // NOT DISPLAYED
      CHECK_PROPERTY,                       // NOT DISPLAYED
      AFFECTED_PIECE_NAMES                  // NOT DISPLAYED
    };
  }

  public static class Prompt extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, IF_ACTIVE, IF_INACTIVE};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.GlobalKeyCommand.if_layer_is_active",
        "Editor.GlobalKeyCommand.if_layer_is_inactive",
        "Editor.GlobalKeyCommand.always"
      };
    }
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    if (condition == null) {
      return new Class<?>[]{
        String.class,                       // Description
        NamedKeyStroke.class,               // Key Command
        Boolean.class,                      // Apply to counters on this map only

        GlobalCommandTarget.class,
//        GlobalCommand.GlobalCommandTargetConfigurer.class,  // Restrict by location? (fast match)
//        PropertyExpression.class,                 // Restrict to map
//        PropertyExpression.class,                 // Restrict to board
//        PropertyExpression.class,                 // Restrict to zone
//        PropertyExpression.class,                 // Restrict to region
//        Integer.class,                            // Restrict to X position
//        Integer.class,                            // Restrict to Y position
//        Boolean.class,                            // Exact property match (fast match)
//        PropertyExpression.class,                 // Property name for fast match
//        PropertyExpression.class,                 // Property value for fast match

        PropertyExpression.class,           // Match properties
        DeckPolicyConfig.class,             // Apply to pieces in deck
        String.class,                       // Button text
        String.class,                       // Button tooltip
        IconConfig.class,                   // Button icon
        NamedKeyStroke.class,               // Hotkey
        Boolean.class,                      // Suppress individual reports?
        ReportFormatConfig.class            // Report format
      };
    }
    else {
      // Backward compatibility
      return new Class<?>[]{
        String.class,
        NamedKeyStroke.class,
        Boolean.class,

        GlobalCommandTarget.class,
//        GlobalCommand.GlobalCommandTargetConfigurer.class,  // Restrict by location? (fast match)
//        PropertyExpression.class,                 // Restrict to map
//        PropertyExpression.class,                 // Restrict to board
//        PropertyExpression.class,                 // Restrict to zone
//        PropertyExpression.class,                 // Restrict to region
//        Integer.class,                            // Restrict to X position
//        Integer.class,                            // Restrict to Y position
//        Boolean.class,                            // Exact property match (fast match)
//        PropertyExpression.class,                 // Property name for fast match
//        PropertyExpression.class,                 // Property value for fast match

        String.class,
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
      return new IconConfigurer(key, name, "/images/keyCommand.gif"); // NON-NLS
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[0]);
    }
  }

  public static class DeckPolicyConfig extends Configurer implements ConfigurerFactory {
    protected static final String FIXED = "Fixed number of pieces"; //NON-NLS (really)
    protected static final String NONE = "No pieces"; //NON-NLS (really)
    protected static final String ALL = "All pieces"; //NON-NLS (really)
    protected IntConfigurer intConfig;
    protected TranslatingStringEnumConfigurer typeConfig;
    protected JLabel prompt;
    protected Box controls;
    protected JPanel controls2;


    public DeckPolicyConfig() {
      this(true);
    }

    public DeckPolicyConfig(boolean showPrompt) {
      super(null, "");

      typeConfig = new TranslatingStringEnumConfigurer(
        new String[]{ALL, NONE, FIXED},
        new String[]{
          "Editor.GlobalKeyCommand.all_pieces",
          "Editor.GlobalKeyCommand.no_pieces",
          "Editor.GlobalKeyCommand.fixed_number_of_pieces"
        }
      );
      intConfig = new IntConfigurer(null, "");
      if (showPrompt) {
        controls2 = new JPanel(new MigLayout("ins 0", "[]rel[]rel[]")); // NON-NLS
        prompt = new JLabel(Resources.getString("Editor.GlobalKeyCommand.deck_policy"));
        controls2.add(prompt);
      }
      else {
        controls2 = new JPanel(new MigLayout("ins 0", "[]rel[]")); // NON-NLS
      }
      controls2.add(typeConfig.getControls());
      controls2.add(intConfig.getControls());

      PropertyChangeListener l = evt -> {
        intConfig.getControls().setVisible(FIXED.equals(typeConfig.getValueString()));
        Window w = SwingUtilities.getWindowAncestor(intConfig.getControls());
        if (w != null) {
          w.pack();
        }
      };
      PropertyChangeListener l2 = evt -> setValue(getIntValue());
      typeConfig.addPropertyChangeListener(l);
      typeConfig.addPropertyChangeListener(l2);
      intConfig.addPropertyChangeListener(l2);
    }

    @Override
    public Component getControls() {
      return controls2;
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
    else if (TARGET.equals(key)) {
      return target.encode ();
    }
//    else if (TARGET_TYPE.equals(key)) {
//      return targetType.name();
//    }
//    else if (TARGET_MAP.equals(key)) {
//      return targetMap.getExpression();
//    }
//    else if (TARGET_BOARD.equals(key)) {
//      return targetBoard.getExpression();
//    }
//    else if (TARGET_ZONE.equals(key)) {
//      return targetZone.getExpression();
//    }
//    else if (TARGET_REGION.equals(key)) {
//      return targetRegion.getExpression();
//    }
//    else if (TARGET_PROPERTY.equals(key)) {
//      return targetProperty.getExpression();
//    }
//    else if (TARGET_VALUE.equals(key)) {
//      return targetValue.getExpression();
//    }
//    else if (TARGET_EXACT_MATCH.equals(key)) {
//      return String.valueOf(targetExactMatch);
//    }
//    else if (TARGET_X.equals(key)) {
//      return String.valueOf(targetX);
//    }
//    else if (TARGET_Y.equals(key)) {
//      return String.valueOf(targetY);
//    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public static String getConfigureTypeName() {
    return Resources.getString ("Editor.GlobalkeyCommand.global_key_command");
  }

  protected LaunchButton getLaunchButton() {
    return launch;
  }

  protected void setLaunchButton(LaunchButton launch) {
    this.launch = launch;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "GlobalKeyCommand"); // NON-NLS
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
      filter = new BooleanAndPieceFilter(filter, piece -> {
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
      });
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
//    if (TARGET_MAP.equals(key)) {
//      return () -> (targetType != GlobalCommand.GlobalCommandTarget.GAME) && (condition == null);
//    }
//    else if (TARGET_ZONE.equals(key)) {
//      return () -> (targetType == GlobalCommand.GlobalCommandTarget.ZONE) && (condition == null);
//    }
//    else if (TARGET_REGION.equals(key)) {
//      return () -> (targetType == GlobalCommand.GlobalCommandTarget.REGION) && (condition == null);
//    }
//    else if (TARGET_X.equals(key) || TARGET_Y.equals(key) || TARGET_BOARD.equals(key)) {
//      return () -> (targetType == GlobalCommand.GlobalCommandTarget.XY) && (condition == null);
//    }
//    else if (TARGET_PROPERTY.equals(key) || TARGET_VALUE.equals(key)) {
//      return () -> targetExactMatch && (condition == null);
//    }
//    else if (TARGET_TYPE.equals(key) || TARGET_EXACT_MATCH.equals(key)) {
//      return () -> (condition == null);
//    }

    return () -> true;
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
        filter = piece -> {
          for (String s : names) {
            if (Decorator.getInnermost(piece).getName().equals(s)) {
              return true;
            }
          }
          return false;
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
    else if (TARGET.equals(key)) {
      if (value instanceof String) {
        value = new GlobalCommandTarget ((String) value);
      }
      target = (GlobalCommandTarget) value;
      target.setCounterGkc (false);
    }
//    else if (TARGET_TYPE.equals(key)) {
//      if (value instanceof String) {
//        value = GlobalCommand.GlobalCommandTarget.valueOf((String)value);
//      }
//      targetType = (GlobalCommand.GlobalCommandTarget)value;
//    }
//    else if (TARGET_MAP.equals(key)) {
//      if (value instanceof String) {
//        targetMap.setExpression((String) value);
//      }
//    }
//    else if (TARGET_BOARD.equals(key)) {
//      if (value instanceof String) {
//        targetBoard.setExpression((String) value);
//      }
//    }
//    else if (TARGET_ZONE.equals(key)) {
//      if (value instanceof String) {
//        targetZone.setExpression((String) value);
//      }
//    }
//    else if (TARGET_REGION.equals(key)) {
//      if (value instanceof String) {
//        targetRegion.setExpression((String) value);
//      }
//    }
//    else if (TARGET_PROPERTY.equals(key)) {
//      if (value instanceof String) {
//        targetMap.setExpression((String) value);
//      }
//    }
//    else if (TARGET_VALUE.equals(key)) {
//      if (value instanceof String) {
//        targetValue.setExpression((String) value);
//      }
//    }
//    else if (TARGET_X.equals(key)) {
//      if (value instanceof String) {
//        value = Integer.valueOf((String)value);
//      }
//      targetX = (Integer)value;
//    }
//    else if (TARGET_Y.equals(key)) {
//      if (value instanceof String) {
//        value = Integer.valueOf((String)value);
//      }
//      targetY = (Integer)value;
//    }
//    else if (TARGET_EXACT_MATCH.equals(key)) {
//      if (value instanceof String) {
//        value = Boolean.valueOf((String)value);
//      }
//      targetExactMatch = (Boolean)value;
//    }
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

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurable's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return List.of(propertiesFilter.getExpression());
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(reportFormat.getFormat());
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(getAttributeValueString(BUTTON_TEXT), getAttributeValueString(TOOLTIP));
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(HOTKEY)), NamedHotKeyConfigurer.decode(getAttributeValueString(KEY_COMMAND)));
  }
}
