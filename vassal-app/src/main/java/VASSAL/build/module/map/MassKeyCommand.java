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
package VASSAL.build.module.map;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BooleanAndPieceFilter;
import VASSAL.counters.CounterGlobalKeyCommand;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.GlobalCommandTarget;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.ToolBarComponent;
import VASSAL.tools.swing.SwingUtils;

import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.List;

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.ArrayUtils;

/**
 * Adds a button to a map window toolbar. Hitting the button applies a particular key command to all pieces on that map
 * with a given name.
 *
 * The "Global Key Command" functionality, as the term is used in Vassal Modules, is spread out over several classes internally:
 * {@link GlobalCommand} - primary functionality for sending commands to multiple pieces based on matching parameters
 * {@link VASSAL.build.module.GlobalKeyCommand}         - Global Key Commands from a Module window
 * {@link VASSAL.build.module.StartupGlobalKeyCommand}  - Global Key Commands from a Module "At Startup"
 * {@link VASSAL.build.module.map.MassKeyCommand}       - Global Key Commands from a specific Map window
 * {@link VASSAL.build.module.map.DeckGlobalKeyCommand} - Global Key Commands from a Deck
 * {@link CounterGlobalKeyCommand}                      - Global Key Commands from a Game Piece
 *
 * Other important classes:
 * {@link GlobalCommandTarget}           - "Fast Match" parameters
 * {@link GlobalCommandTargetConfigurer} - configurer for "Fast Match" parameters
 */
public class MassKeyCommand extends AbstractToolbarItem
                            implements RecursionLimiter.Loopable {
  public static final String DEPRECATED_NAME = "text"; // NON-NLS
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

  // TODO: When these are removed, look for all of the "removal" warning
  // suppressions we added for them, and remove those.
  // These 3 identical to AbstractToolbarItem and here for clirr purposes only
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String NAME = "name"; // NON-NLS
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; // NON-NLS
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; // NON-NLS

  /** @deprecated use launch from the superclass */
  @Deprecated (since = "2020-10-21", forRemoval = true)
  protected LaunchButton launch; // Exists for clirr - but use getLaunchButton()

  protected NamedKeyStroke stroke = NamedKeyStroke.NULL_KEYSTROKE;
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

  protected GlobalCommandTarget target = new GlobalCommandTarget(getGKCtype());

  /**
   * @return Our GKC type -- this method is overridden by {@link VASSAL.build.module.GlobalKeyCommand} for module-level GKC's
   * and by {@link DeckGlobalKeyCommand} for Deck GKC's. This value affects what configurer options are shown.
   */
  public GlobalCommandTarget.GKCtype getGKCtype() {
    return GlobalCommandTarget.GKCtype.MAP;
  }

  public MassKeyCommand() {
    setButtonTextKey(BUTTON_TEXT);
    setHotKeyKey(HOTKEY);

    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.GlobalKeyCommand.button_name"),
      Resources.getString("Editor.GlobalKeyCommand.button_name"),
      "", //Default art exists, but is a little weird, and wasn't actually being defaulted to before --> "/images/keyCommand.gif", //NON-NLS
      e -> apply()
    ));

    launch = getLaunchButton(); // for compatibility
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof Map) {
      map = (Map) parent;
    }
    if (parent instanceof ToolBarComponent) {
      ((ToolBarComponent)parent).getToolBar().add(getLaunchButton());
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
      GameModule.getGameModule().sendAndLog(globalCommand.apply(map, getFilter(), target));
    }
    else {
      final List<Map> l = Map.getMapList();
      GameModule.getGameModule().sendAndLog(
          globalCommand.apply(l.toArray(new Map[0]), getFilter(), target));
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
      return ArrayUtils.addAll(
        super.getAttributeDescriptions(),
        Resources.getString("Editor.GlobalKeyCommand.global_key_command"), //$NON-NLS-1$       // Key Command
        Resources.getString("Editor.MassKey.counters"), //$NON-NLS-1$       // Apply to counters on this map only

        Resources.getString("Editor.GlobalKeyCommand.pre_select"),          // Fast match target info

        Resources.getString("Editor.MassKey.match"), //$NON-NLS-1$          // Match properties        
        Resources.getString("Editor.GlobalKeyCommand.deck_policy"), //$NON-NLS-1$   // Apply to pieces in deck
        Resources.getString("Editor.MassKey.suppress"), //$NON-NLS-1$       // Suppress individual reports?
        Resources.getString("Editor.report_format") //$NON-NLS-1$           // Report format
      );
    }
    else {
      // Backward compatibility
      return ArrayUtils.addAll(
        super.getAttributeDescriptions(),
        Resources.getString("Editor.MassKey.key"), //$NON-NLS-1$             // Key Command
        Resources.getString("Editor.MassKey.counters"), //$NON-NLS-1$        // Apply to counters on this map only

        Resources.getString("Editor.GlobalKeyCommand.pre_select"),           // Fast match target info

        Resources.getString("Editor.MassKey.match"), //$NON-NLS-1$           // Match properties
        Resources.getString("Editor.MassKey.deck_content"), //$NON-NLS-1$    // Apply to pieces in deck
        Resources.getString("Editor.MassKey.suppress"), //$NON-NLS-1$       // Suppress individual reports?
        Resources.getString("Editor.report_format"), //$NON-NLS-1$          // Report format
        Resources.getString("Editor.MassKey.apply") //$NON-NLS-1$           // Legacy condition
      );
    }
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      KEY_COMMAND,                          // Key Command
      SINGLE_MAP,                           // Apply to counters on this map only

      TARGET,                               // Fast match target info

      PROPERTIES_FILTER,                    // Match properties
      DECK_COUNT,                           // Apply to pieces in deck
      REPORT_SINGLE,                        // Suppress individual reports?
      REPORT_FORMAT,                        // Report format
      CONDITION,                            // Legacy condition
      CHECK_VALUE,                          // NOT DISPLAYED
      CHECK_PROPERTY,                       // NOT DISPLAYED
      AFFECTED_PIECE_NAMES                  // NOT DISPLAYED
    );
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
      return ArrayUtils.addAll(
        super.getAttributeTypes(),
        NamedKeyStroke.class,               // Key Command
        Boolean.class,                      // Apply to counters on this map only

        GlobalCommandTarget.class,          // Fast Match target info

        PropertyExpression.class,           // Match properties
        DeckPolicyConfig.class,             // Apply to pieces in deck
        Boolean.class,                      // Suppress individual reports?
        ReportFormatConfig.class            // Report format
      );
    }
    else {
      // Backward compatibility
      return ArrayUtils.addAll(
        super.getAttributeTypes(),
        NamedKeyStroke.class,
        Boolean.class,

        GlobalCommandTarget.class,

        String.class,
        DeckPolicyConfig.class,
        Boolean.class,
        ReportFormatConfig.class,
        Prompt.class
      );
    }
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/keyCommand.gif"); //NON-NLS
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
      this(false);
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

      final PropertyChangeListener l = evt -> {
        intConfig.getControls().setVisible(FIXED.equals(typeConfig.getValueString()));
        SwingUtils.repack(intConfig.getControls());
      };
      final PropertyChangeListener l2 = evt -> setValue(getIntValue());
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
      final String type = typeConfig.getValueString();
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
          final Integer i = (Integer) o;
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
    if (KEY_COMMAND.equals(key)) {
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
      return target.encode();
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public GlobalCommandTarget getTarget() {
    return target;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GlobalkeyCommand.global_key_command");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "GlobalKeyCommand"); // NON-NLS
  }

  @Override
  public void removeFrom(Buildable parent) {
    if (parent instanceof ToolBarComponent) {
      ((ToolBarComponent)parent).getToolBar().remove(getLaunchButton());
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
      if (getLaunchButton().getAttributeValueString(TOOLTIP) == null) {
        getLaunchButton().setAttribute(TOOLTIP, value);
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
          for (final String s : names) {
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
        value = new GlobalCommandTarget((String) value);
      }
      target = (GlobalCommandTarget) value;
      target.setGKCtype(getGKCtype());

      // Fast match currently disabled for DGKC's - they already search only their own deck
      if (getGKCtype() == GlobalCommandTarget.GKCtype.DECK) {
        target.setFastMatchLocation(false);
        target.setFastMatchProperty(false);
      }
    }
    else {
      super.setAttribute(key, value);
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
   * @return a list of the Configurables property fields (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return target.getPropertyList();
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> expList = target.getExpressionList();
    expList.add(propertiesFilter.getExpression());
    return expList;
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
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(HOTKEY)), NamedHotKeyConfigurer.decode(getAttributeValueString(KEY_COMMAND)));
  }
}
