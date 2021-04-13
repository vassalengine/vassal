/*
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.turn;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.IconButton;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.SwingConstants;

/**
 * Generic Turn Counter
 */
public class TurnTracker extends TurnComponent implements CommandEncoder, GameComponent, ActionListener, UniqueIdManager.Identifyable, RecursionLimiter.Loopable {

  protected static final UniqueIdManager idMgr = new UniqueIdManager("TurnTracker"); //$NON-NLS-1$

  protected static final String COMMAND_PREFIX = "TURN"; //$NON-NLS-1$

  public static final String NAME = "name"; // NON-NLS
  public static final String HOT_KEY = "hotkey"; //$NON-NLS-1$
  public static final String NEXT_HOT_KEY = "nexthotkey"; //$NON-NLS-1$
  public static final String PREV_HOT_KEY = "prevhotkey"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$
  public static final String BUTTON_TOOLTIP = "buttonTooltip"; //$NON-NLS-1$
  public static final String TURN_FORMAT = "turnFormat"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String LENGTH = "length"; //$NON-NLS-1$
  public static final String LENGTH_STYLE = "lengthStyle"; //$NON-NLS-1$

  protected static final String FONT_SIZE = "turnFontSize"; //$NON-NLS-1$
  protected static final String FONT_BOLD = "turnFontBold"; //$NON-NLS-1$
  protected static final String DOCKED = "turnDocked"; //$NON-NLS-1$

  /** Variable name for reporting format */
  protected static final String OLD_TURN = "oldTurn"; //$NON-NLS-1$
  protected static final String NEW_TURN = "newTurn"; //$NON-NLS-1$
  protected static final String LEVEL = "level"; //$NON-NLS-1$

  protected static final String TURN_FONT = "Dialog"; //NON-NLS
  protected static String SET_COMMAND;
  protected static String DOCK_COMMAND;
  protected static String UNDOCK_COMMAND;

  protected static final String NEXT = "Next"; //NON-NLS (really)
  protected static final String PREV = "Prev"; //NON-NLS (really)
  protected static final String SET = "Set"; //NON-NLS (really)

  protected static final String PROP_VALUE = "_value"; //$NON-NLS-1$
  protected static final String PROP_COMMAND = "_command"; //$NON-NLS-1$

  protected static final String LENGTH_VARIABLE = "Variable"; //NON-NLS (really)
  protected static final String LENGTH_MAXIMUM = "Maximum"; //NON-NLS (really)
  protected static final String LENGTH_FIXED = "Fixed"; //NON-NLS (really)

  protected FormattedString turnFormat = new FormattedString(String.join(LEVEL, "$", "1$ $", "2$ $", "3$ $", "4$")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

  protected FormattedString reportFormat = new FormattedString(Resources.getString("Editor.TurnTracker.report_default"));

  protected TurnWindow turnWindow;
  protected TurnWidget turnWidget;
  protected JPanel launchWidget;
  protected SetDialog setDialog;
  protected LaunchButton launch;
  protected NamedKeyStrokeListener nextListener;
  protected NamedKeyStrokeListener prevListener;

  protected String savedState = ""; //$NON-NLS-1$
  protected String savedSetState = ""; //$NON-NLS-1$
  protected String savedTurn = ""; //$NON-NLS-1$
  protected JPopupMenu popup;

  protected int currentLevel = 0;
  protected String id;
  protected int width = -1;
  protected String lengthStyle = LENGTH_MAXIMUM;

  protected MutableProperty.Impl lastCommand = new MutableProperty.Impl(SET, this);
  protected MutableProperty.Impl lastTurn = new MutableProperty.Impl("", this);

  public TurnTracker() {

    final ActionListener al = e -> {
      if (!isDocked()) {
        turnWindow.setControls();
        turnWindow.setVisible(!turnWindow.isShowing());
        turnWindow.setFocusable(true);
      }
    };
    setConfigureName(Resources.getString("TurnTracker.turn")); //$NON-NLS-1$
    launch = new LaunchButton(Resources.getString("TurnTracker.turn"), "", BUTTON_TEXT, HOT_KEY, ICON, al); //$NON-NLS-1$
    launch.setToolTipText(Resources.getString("TurnTracker.turn_tracker")); //$NON-NLS-1$

    SET_COMMAND = Resources.getString("TurnTracker.set_turn"); //$NON-NLS-1$
    DOCK_COMMAND = Resources.getString("General.dock"); //$NON-NLS-1$
    UNDOCK_COMMAND = Resources.getString("General.undock"); //$NON-NLS-1$

    // Create preferences
    final IntConfigurer size = new IntConfigurer(FONT_SIZE, Resources.getString("TurnTracker.size_pref"), 14); //$NON-NLS-1$
    final BooleanConfigurer bold = new BooleanConfigurer(FONT_BOLD,  Resources.getString("TurnTracker.bold_pref"), Boolean.FALSE); //$NON-NLS-1$
    final BooleanConfigurer docked = new BooleanConfigurer(DOCKED,  Resources.getString("TurnTracker.docked_pref"), Boolean.FALSE); //$NON-NLS-1$

    final String prefTab = Resources.getString("TurnTracker.turn_counter"); //$NON-NLS-1$
    GameModule.getGameModule().getPrefs().addOption(prefTab, size);
    GameModule.getGameModule().getPrefs().addOption(prefTab, bold);
    GameModule.getGameModule().getPrefs().addOption(prefTab, docked);

    size.addPropertyChangeListener(e -> setDisplayFont());
    bold.addPropertyChangeListener(e -> setDisplayFont());
    docked.addPropertyChangeListener(e -> setDocked(isDocked()));

    // Set up listeners for prev/next hotkeys
    nextListener = new NamedKeyStrokeListener(e -> turnWidget.doNext());
    GameModule.getGameModule().addKeyStrokeListener(nextListener);

    prevListener = new NamedKeyStrokeListener(e -> turnWidget.doPrev());
    GameModule.getGameModule().addKeyStrokeListener(prevListener);

    // Create the displayable widget
    turnWidget = new TurnWidget();
  }


  public String getState() {
    final SequenceEncoder se = new SequenceEncoder('|');
    se.append(currentLevel);
    final Iterator<TurnLevel> i = getTurnLevels();
    while (i.hasNext()) {
      final TurnLevel level = i.next();
      se.append(level.getState());
    }
    return se.getValue();
  }

  public void setState(String newState) {
    final SequenceEncoder.Decoder sd =
      new SequenceEncoder.Decoder(newState, '|');
    currentLevel = sd.nextInt(0);
    final Iterator<TurnLevel> i = getTurnLevels();
    while (i.hasNext()) {
      final TurnLevel level = i.next();
      level.setState(sd.nextToken("")); //$NON-NLS-1$
    }

    setLaunchToolTip();
    updateTurnDisplay(SET);
  }

  protected void setLaunchToolTip() {
    launch.setToolTipText(getTurnString());
  }

  /*
   * Module level Configuration stuff
   */
  @Override
  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOT_KEY, NEXT_HOT_KEY, PREV_HOT_KEY, TURN_FORMAT, REPORT_FORMAT, TOOLTIP, LENGTH_STYLE, LENGTH };
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      clearGlobalProperties();
      setConfigureName((String) value);
      lastCommand.setPropertyName(getConfigureName() + PROP_COMMAND);
      lastTurn.setPropertyName(getConfigureName() + PROP_VALUE);
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) value);
    }
    else if (TURN_FORMAT.equals(key)) {
      turnFormat.setFormat((String) value);
    }
    else if (TOOLTIP.equals(key)) {
      turnWidget.setLabelToolTipText((String) value);
    }
    else if (LENGTH.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      width = (Integer) value;
    }
    else if (LENGTH_STYLE.equals(key)) {
      lengthStyle = (String) value;
      if (LENGTH_VARIABLE.equals(lengthStyle)) {
        width = 0;
      }
      else if (LENGTH_MAXIMUM.equals(lengthStyle)) {
        width = -1;
      }
    }
    else if (NEXT_HOT_KEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      nextListener.setKeyStroke((NamedKeyStroke) value);
      turnWidget.setNextStroke((NamedKeyStroke) value);
    }
    else if (PREV_HOT_KEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      prevListener.setKeyStroke((NamedKeyStroke) value);
      turnWidget.setPrevStroke((NamedKeyStroke) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  protected void setWidgetWidth() {
    if (LENGTH_FIXED.equals(lengthStyle)) {
      turnWidget.setWidth(width);
    }
    else if (LENGTH_MAXIMUM.equals(lengthStyle)) {
      turnWidget.setWidth(getMaximumWidth());
    }
    else {
      turnWidget.setWidth(0);
    }
  }

  /**
   * Calculate the maximum width for the turnWidget to display
   * any item.
   *
   * First calculate the maximum string that can be displayed, then
   * convert this to a width based on the display font.
   *
   * @return Maximum Width
   */
  protected int getMaximumWidth() {
    final String maxString = getMaximumTurnString();
    final int max = turnWidget.getWidth(maxString);
    return max + 2;
  }

  protected void setDisplayFont() {
    turnWidget.setLabelFont(getDisplayFont());
    if (!isDocked() && turnWindow != null) {
      turnWindow.pack();
    }
  }

  protected Font getDisplayFont() {
    return new Font(TURN_FONT, getFontStyle(), getFontSize());
  }

  protected void setFontSize() {
    setDisplayFont();
  }

  protected int getFontSize() {
    return (Integer) GameModule.getGameModule().getPrefs().getValue(FONT_SIZE);
  }

  protected int getFontStyle() {
    return ((Boolean) GameModule.getGameModule().getPrefs().getValue(FONT_BOLD)) ? Font.BOLD : Font.PLAIN;
  }

  protected boolean isDocked() {
    return (Boolean) GameModule.getGameModule().getPrefs().getValue(DOCKED);
  }

  protected void setDocked(boolean dock) {
    final GameModule g = GameModule.getGameModule();
    g.getPrefs().setValue(DOCKED, dock);
    launch.setVisible(
      !dock && (
        getAttributeValueString(BUTTON_TEXT).length() > 0 ||
        getAttributeValueString(ICON).length() > 0
      )
    );
    if (dock) {
      turnWindow.setWidget(null);
      turnWindow.setVisible(false);
      launchWidget.add(turnWidget, BorderLayout.CENTER);
      launchWidget.setVisible(g.getGameState().isGameStarted());
    }
    else {
      launchWidget.setVisible(false);
      launchWidget.remove(turnWidget);
      turnWindow.setWidget(turnWidget);
      turnWindow.setVisible(g.getGameState().isGameStarted());
      turnWindow.setFocusable(true);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (TURN_FORMAT.equals(key)) {
      return turnFormat.getFormat();
    }
    else if (TOOLTIP.equals(key)) {
      return turnWidget.getLabelToolTipText();
    }
    else if (LENGTH.equals(key)) {
      return String.valueOf(width);
    }
    else if (LENGTH_STYLE.equals(key)) {
      return lengthStyle;
    }
    else if (NEXT_HOT_KEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(nextListener.getNamedKeyStroke());
    }
    else if (PREV_HOT_KEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(prevListener.getNamedKeyStroke());
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.button_text_label"),
      Resources.getString("Editor.button_icon_label"),
      Resources.getString("Editor.TurnTracker.show_hotkey"),
      Resources.getString("Editor.TurnTracker.next_hotkey"),
      Resources.getString("Editor.TurnTracker.prev_hotkey"),
      Resources.getString("Editor.TurnTracker.turn_name_format"),
      Resources.getString("Editor.report_format"),
      Resources.getString("Editor.TurnTracker.turn_tooltip"),
      Resources.getString("Editor.TurnTracker.turn_length"),
      Resources.getString("Editor.TurnTracker.turn_display")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class,
      NamedKeyStroke.class,
      NamedKeyStroke.class,
      TurnFormatConfig.class,
      ReportFormatConfig.class,
      String.class,
      LengthStyleConfig.class,
      Integer.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((TurnTracker) c).launch.getAttributeValueString(ICON));
    }
  }

  public static class TurnFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      final TurnTracker t = (TurnTracker) c;
      final String[] s = new String[t.getLevelCount()];
      for (int i = 0; i < s.length; i++) {
        s[i] = LEVEL + (i + 1);
      }
      return new FormattedStringConfigurer(key, name, s);
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] {OLD_TURN, NEW_TURN });
    }
  }

  public static class LengthStyleConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{LENGTH_VARIABLE, LENGTH_FIXED, LENGTH_MAXIMUM};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[]{
        Resources.getString("Editor.TurnTracker.length_variable"),
        Resources.getString("Editor.TurnTracker.length_fixed"),
        Resources.getString("Editor.TurnTracker.length_maximum"),
      };
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (LENGTH.equals(name)) {
      return () -> LENGTH_FIXED.equals(lengthStyle);
    }
    else {
      return null;
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {
      CounterTurnLevel.class, ListTurnLevel.class, TurnGlobalHotkey.class
    };
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.TurnTracker.component_type");
  }

  @Override
  public void addTo(Buildable b) {
    //Create the turn window
    turnWindow = new TurnWindow();
    turnWindow.pack();
    turnWindow.setVisible(false);

    launchWidget = new JPanel();
    launchWidget.setLayout(new BorderLayout());
    launchWidget.setBorder(BorderFactory.createEtchedBorder());
    GameModule.getGameModule().getToolBar().add(launchWidget);
    launchWidget.setAlignmentY(0.0F);
    launchWidget.setVisible(false);

    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    launch.setEnabled(false);

    setDocked(isDocked());

    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    idMgr.add(this);

    //Global Property support
    lastCommand.addTo((MutablePropertiesContainer) b);
    lastTurn.addTo((MutablePropertiesContainer) b);

  }

  @Override
  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().getToolBar().remove(launchWidget);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    lastCommand.removeFromContainer();
    lastTurn.removeFromContainer();
    clearGlobalProperties();
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("TurnTracker.html"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public String getId() {
    return id;
  }

  protected void captureState() {
    savedState = getState();
    savedTurn = getTurnString();
  }

  protected void save() {

    if (!savedState.equals(getState())) {

      reportFormat.setProperty(OLD_TURN, savedTurn);
      reportFormat.setProperty(NEW_TURN, getTurnString());

      final String s = updateString(reportFormat.getText(), new String[] { "\\n", "\\t" }, new String[] { " - ", " " }); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      final Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + s);
      c.execute();
      c.append(new SetTurn(this, savedState));

      GameModule.getGameModule().sendAndLog(c);

      setLaunchToolTip();
    }

    captureState();
  }

  /**
   * Calculate the maximum sized turn string that can be generated
   * by any turn combination.
   *
   * @return maximum turn string
   */
  protected String getMaximumTurnString() {
    final List<String> levels = new ArrayList<>();
    for (final Buildable b : getBuildables()) {
      if (b instanceof TurnLevel) {
        ((TurnLevel) b).findMaximumStrings(levels, 0);
      }
    }
    turnFormat.clearProperties();
    for (int i = 0; i < levels.size(); i++) {
      turnFormat.setProperty(LEVEL + (i + 1), levels.get(i));
    }
    return turnFormat.getText(GameModule.getGameModule());
  }

  /**
   * Build the turn string to be displayed from the currently
   * active Child TurnLevel's
   * @return Turn String
   */
  protected String getTurnString() {
    turnFormat.clearProperties();
    final List<TurnLevel> turnDesc = getActiveChildLevels();
    for (int i = 0; i < 15; i++) {
      turnFormat.setProperty(LEVEL + (i + 1), i < turnDesc.size() ? turnDesc.get(i).getTurnString() : "");
    }
    return turnFormat.getText(GameModule.getGameModule());
  }

  /**
   * A list of all active TurnLevels within the TurnTracker
   * @return List of Turn levels
   */
  protected List<TurnLevel> getActiveChildLevels() {
    final ArrayList<TurnLevel> levels = new ArrayList<>();
    final TurnLevel level = getTurnLevel(currentLevel);
    if (level != null) {
      levels.add(level);
      levels.addAll(level.getActiveChildLevels());
    }
    return levels;
  }

  protected int getLevelCount() {
    return getActiveChildLevels().size();
  }

  protected void next() {

    if (getTurnLevelCount() == 0) {
      return;
    }

    final TurnLevel level = getTurnLevel(currentLevel);
    level.advance();
    if (level.hasRolledOver()) {
      currentLevel++;
      if (currentLevel >= getTurnLevelCount()) {
        currentLevel = 0;
      }
      getTurnLevel(currentLevel).setLow();
    }

    updateTurnDisplay(NEXT);
    doGlobalkeys();
  }

  protected void prev() {

    if (getTurnLevelCount() == 0) {
      return;
    }

    final TurnLevel level = getTurnLevel(currentLevel);
    level.retreat();
    if (level.hasRolledOver()) {
      currentLevel--;
      if (currentLevel < 0) {
        currentLevel = getTurnLevelCount() - 1;
      }
      getTurnLevel(currentLevel).setHigh();
    }

    updateTurnDisplay(PREV);
    doGlobalkeys();
  }

  protected void doGlobalkeys() {
    for (final TurnGlobalHotkey key : getComponentsOf(TurnGlobalHotkey.class)) {
      try {
        RecursionLimiter.startExecution(this);
        key.apply();
      }
      catch (RecursionLimitException e) {
        RecursionLimiter.infiniteLoop(e);
      }
      finally {
        RecursionLimiter.endExecution();
      }
    }
  }

  // Implement Loopable
  @Override
  public String getComponentTypeName() {
    return getConfigureTypeName();
  }

  @Override
  public String getComponentName() {
    return getConfigureName();
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    final String command = e.getActionCommand();
    if (command.equals(SET_COMMAND)) {
      set();
    }
    else if (command.equals(DOCK_COMMAND)) {
      setDocked(true);
    }
    else if (command.equals(UNDOCK_COMMAND)) {
      setDocked(false);
    }
  }

  protected void set() {
    savedSetState = getState();
    if (setDialog == null) {
      setDialog = new SetDialog();
      setDialog.setTitle(Resources.getString("TurnTracker.set_turn2", getConfigureName())); //$NON-NLS-1$
    }
    setDialog.setControls(this);
    setDialog.setVisible(true);
  }

  protected void updateTurnDisplay(String command) {
    lastCommand.setPropertyValue(command);
    lastTurn.setPropertyValue(getTurnString());
    turnWidget.setControls();
    turnWidget.repaint();
    turnWindow.pack();
    turnWindow.setFocusable(true);
    turnWindow.requestFocus();
  }

  protected void clearGlobalProperties() {
    lastCommand.setPropertyValue(null);
    lastTurn.setPropertyValue(null);
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(COMMAND_PREFIX + getId())) {
      return null;
    }
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, '\t');
    sd.nextToken(""); //$NON-NLS-1$
    return new SetTurn(sd.nextToken(""), this); //$NON-NLS-1$
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof SetTurn)) {
      return null;
    }
    final SetTurn com = (SetTurn) c;
    final SequenceEncoder se = new SequenceEncoder('\t');
    se.append(COMMAND_PREFIX + com.getTurn().getId());
    se.append(com.newState);
    return se.getValue();
  }

  @Override
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);

    turnWindow.setVisible(false);

    launchWidget.setVisible(isDocked() && gameStarting);
    if (gameStarting) {
      lastCommand.setPropertyValue(SET);
      lastTurn.setPropertyValue("");
      turnWidget.setControls();
      setWidgetWidth();
    }
    else {
      reset();
    }
  }

  protected void reset() {
    for (int i = 0; i < getTurnLevelCount(); i++) {
      (getTurnLevel(i)).reset();
    }
    currentLevel = 0;
    setLaunchToolTip();
    clearGlobalProperties();
  }

  public String updateString(String str, String[] from, String[] to) {
    final StringBuilder s = new StringBuilder(str);

    for (int i = 0; i < from.length; i++) {
      replace(s, from[i], to[i]);
    }

    return s.toString();
  }

  public void replace(StringBuilder s, String from, String to) {
    int i = s.indexOf(from);
    while (i >= 0) {
      s = s.replace(i, i + 2, to);
      i = s.indexOf(from);
    }
  }

  /** @deprecated Use {@link #replace(StringBuilder,String,String)} instead. */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void replace(StringBuffer s, String from, String to) {
    ProblemDialog.showDeprecated("2020-08-06");
    int i = s.indexOf(from);
    while (i >= 0) {
      s = s.replace(i, i + 2, to);
      i = s.indexOf(from);
    }
  }

  @Override
  public Command getRestoreCommand() {
    return new SetTurn(getState(), this);
  }

  protected class TurnWindow extends JDialog {
    private static final long serialVersionUID = 1L;
    protected TurnWidget widget;

    protected TurnWindow() {
      super(GameModule.getGameModule().getPlayerWindow());
      setTitle(getConfigureName());
      pack();
      setLocation(100, 100);
      setFocusable(true);
    }

    protected void setWidget(TurnWidget t) {
      if (t == null) {
        if (widget != null) {
          remove(widget);
        }
      }
      else {
        add(t);
      }
      pack();
    }

    protected void setControls() {
      if (widget != null) {
        widget.setControls();
      }
      pack();
    }

  }

  protected class TurnWidget extends JPanel implements MouseListener {

    private static final long serialVersionUID = 1L;
    private IconButton nextButton;
    private IconButton prevButton;

    protected final int BUTTON_SIZE = 22; //NOPMD

    protected JLabel turnLabel = new JLabel();

    protected TurnWidget() {
      super();
      initComponents();
    }

    public void setLabelFont(Font displayFont) {
      turnLabel.setFont(displayFont);
    }

    public void setWidth(int length) {
      if (length > 0) {
        turnLabel.setMinimumSize(new Dimension(length, BUTTON_SIZE));
        turnLabel.setPreferredSize(new Dimension(length, BUTTON_SIZE));
      }
      else {
        turnLabel.setMinimumSize(null);
        turnLabel.setPreferredSize(null);
      }
    }

    public void setLabelToolTipText(String tooltip) {
      turnLabel.setToolTipText(tooltip);
    }

    public String getLabelToolTipText() {
      return turnLabel.getToolTipText();
    }

    public Color getColor() {
      return turnLabel.getBackground();
    }

    public int getWidth(String text) {
      return turnLabel.getGraphics().getFontMetrics().stringWidth(text);
    }

    protected void doNext() {
      captureState();
      next();
      save();
    }

    protected void doPrev() {
      captureState();
      prev();
      save();
    }

    protected void initComponents() {

      setLayout(new BorderLayout(5, 5));

      nextButton = new IconButton(IconButton.PLUS_ICON, BUTTON_SIZE);
      setNextStroke(nextListener.getNamedKeyStroke());
      nextButton.setAlignmentY(Component.TOP_ALIGNMENT);
      nextButton.addActionListener(e -> doNext());

      prevButton = new IconButton(IconButton.MINUS_ICON, BUTTON_SIZE);
      setPrevStroke(prevListener.getNamedKeyStroke());
      prevButton.setAlignmentY(Component.TOP_ALIGNMENT);
      prevButton.addActionListener(e -> doPrev());

      // Next, the Label containing the Turn Text
      turnLabel.setFont(getDisplayFont());
      turnLabel.setFocusable(false);
      turnLabel.setHorizontalTextPosition(JLabel.CENTER);
      turnLabel.setHorizontalAlignment(SwingConstants.CENTER);
      turnLabel.addMouseListener(this);
      turnLabel.setBackground(Color.WHITE);
      turnLabel.setToolTipText(Resources.getString("TurnTracker.click_to_configure")); //$NON-NLS-1$


      add(prevButton, BorderLayout.LINE_START);
      add(turnLabel, BorderLayout.CENTER);
      add(nextButton, BorderLayout.LINE_END);

      addMouseListener(this);
    }

    public void setNextStroke(NamedKeyStroke key) {
      final String tooltip = Resources.getString("TurnTracker.next_turn") + (key == null ? "" : " " + NamedHotKeyConfigurer.getFancyString(key)); //NON-NLS
      nextButton.setToolTipText(tooltip);
    }

    public void setPrevStroke(NamedKeyStroke key) {
      final String tooltip = Resources.getString("TurnTracker.prev_turn") +  //$NON-NLS-1$
        (key == null ? "" : " " + NamedHotKeyConfigurer.getFancyString(key)); //$NON-NLS-1$ //$NON-NLS-2$
      prevButton.setToolTipText(tooltip);
    }



    public void setControls() {
      final String s = updateString(getTurnString(), new String[] { "\\n", "\\t" }, new String[] { "\n", "    " }); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      turnLabel.setText(s);
    }

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (e.isPopupTrigger()) {
        doPopup(e.getPoint());
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (e.isPopupTrigger()) {
        doPopup(e.getPoint());
      }
    }

    public void doPopup(Point p) {
      buildPopup();
      if (isShowing()) {
        popup.show(this, p.x, p.y);
      }
    }
  }

  protected void buildPopup() {
    popup = new JPopupMenu();
    popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
      @Override
      public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
        turnWidget.repaint();
      }

      @Override
      public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
        turnWidget.repaint();
      }

      @Override
      public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
      }
    });

    JMenuItem item;

    // Dock/Undock
    if (isDocked()) {
      item = new JMenuItem(UNDOCK_COMMAND);
    }
    else {
      item = new JMenuItem(DOCK_COMMAND);
    }
    item.addActionListener(this);
    popup.add(item);

    // Set Current Turn directly
    item = new JMenuItem(SET_COMMAND);
    item.addActionListener(this);
    popup.add(item);

    // Configure List Items
    final JMenu config = new JMenu(Resources.getString("TurnTracker.configure")); //$NON-NLS-1$

    for (int i = 0; i < getTurnLevelCount(); i++) {
      getTurnLevel(i).buildConfigMenu(config);
    }

    if (config.getItemCount() > 0) {
      popup.add(config);
    }
  }

  protected void addItem(JMenu menu, String command) {
    final JMenuItem item = new JMenuItem(command);
    item.addActionListener(this);
    menu.add(item);
  }

  private static final Dimension FILLER = new Dimension(0, 3);

  protected class SetDialog extends JDialog {

    private static final long serialVersionUID = 1L;

    protected JPanel panel;
    protected JPanel controls = null;
    protected JPanel levelControls = null;
    protected Component childControls = null;
    protected TurnTracker turn;
    protected JDialog me;

    protected SetDialog() {
      super(GameModule.getGameModule().getPlayerWindow());
      initComponents();
      setLocation(100, 100);
      me = this;
    }

    protected void initComponents() {
      setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          cancelSet();
          setVisible(false);
        }
      });

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      add(panel);

      final JPanel p = new JPanel();

      final JButton saveButton = new JButton(Resources.getString(Resources.SAVE));
      saveButton.setToolTipText(Resources.getString("TurnTracker.save_changes")); //$NON-NLS-1$
      p.add(saveButton);
      saveButton.addActionListener(e -> {
        saveSet();
        setVisible(false);
      });

      final JButton cancelButton = new JButton(Resources.getString(Resources.CANCEL));
      cancelButton.setToolTipText(Resources.getString("TurnTracker.discard_changes")); //$NON-NLS-1$
      cancelButton.addActionListener(e -> {
        cancelSet();
        setVisible(false);
      });
      p.add(cancelButton);

      add(p);
    }

    public void setControls(TurnTracker turn) {

      this.turn = turn;

      if (controls != null) {
        panel.remove(controls);
      }

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      levelControls = new JPanel();
      levelControls.setLayout(new BoxLayout(levelControls, BoxLayout.Y_AXIS));

      if (getTurnLevelCount() > 1) {
        final JPanel p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
        p.setBorder(BorderFactory.createLineBorder(Color.black));

        final String[] s = new String[getTurnLevelCount()];
        for (int i = 0; i < s.length; i++) {
          s[i] = getTurnLevel(i).getConfigureName();
        }
        final StringEnumConfigurer e = new StringEnumConfigurer(null, Resources.getString("TurnTracker.select"), s); //$NON-NLS-1$
        e.setValue(getTurnLevel(currentLevel).getConfigureName());
        e.addPropertyChangeListener(e1 -> {
          final String option = ((StringEnumConfigurer) e1.getSource()).getValueString();
          for (int i = 0; i < getTurnLevelCount(); i++) {
            if (option.equals(getTurnLevel(i).getConfigureName())) {
              currentLevel = i;
              updateTurnDisplay(SET);
              addChildControls();
            }
          }
        });

        p.add(Box.createRigidArea(FILLER));
        p.add(e.getControls());
        p.add(Box.createRigidArea(FILLER));
        levelControls.add(p);
        levelControls.add(Box.createRigidArea(FILLER));

      }

      addChildControls();

      controls.add(levelControls);

      panel.add(controls);
      pack();
    }

    protected void addChildControls() {
      if (childControls != null) {
        levelControls.remove(childControls);
      }
      childControls = getTurnLevel(currentLevel).getSetControls(me, turn);
      levelControls.add(childControls);
      pack();
    }

  }

  protected void cancelSet() {
    setState(savedSetState);
    turnWindow.setVisible(true);
    turnWindow.setFocusable(true);
  }

  protected void saveSet() {
    save();
    updateTurnDisplay(SET);
    doGlobalkeys();
  }

  public static class SetTurn extends Command {
    private final String oldState;
    private final String newState;
    private final TurnTracker turn;

    public SetTurn(String newState, TurnTracker t) {
      this.newState = newState;
      oldState = t.getState();
      turn = t;
    }

    public SetTurn(TurnTracker t, String oldState) {
      newState = t.getState();
      this.oldState = oldState;
      turn = t;
    }

    public TurnTracker getTurn() {
      return turn;
    }

    @Override
    protected void executeCommand() {
      turn.setState(newState);
    }

    @Override
    protected Command myUndoCommand() {
      return new SetTurn(oldState, turn);
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(turnFormat.getFormat(), reportFormat.getFormat());
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
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(HOT_KEY)),
                         NamedHotKeyConfigurer.decode(getAttributeValueString(NEXT_HOT_KEY)),
                         NamedHotKeyConfigurer.decode(getAttributeValueString(PREV_HOT_KEY))
      );
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    final String fileName = launch.getAttributeValueString(ICON);
    if (fileName != null) {
      s.add(fileName);
    }
  }
}
