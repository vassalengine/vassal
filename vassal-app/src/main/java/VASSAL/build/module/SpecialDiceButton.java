/*
 *
 * Copyright (c) 2004-2012 by Michael Blumohr, Rodney Kinney, Brent Easton
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
package VASSAL.build.module;

import VASSAL.build.AbstractFolder;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.MutableProperty.Impl;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.ComponentDescription;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PlayerIdFormattedExpressionConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.FormattedString;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.LoopControl;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OwningOpMultiResolutionImage;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * ...
 */
public class SpecialDiceButton extends DoActionButton implements CommandEncoder, UniqueIdManager.Identifyable, ComponentDescription {
  private static final Logger logger = LoggerFactory.getLogger(SpecialDiceButton.class);

  protected static final UniqueIdManager idMgr = new UniqueIdManager("SpecialDiceButton"); //$NON-NLS-1$
  public static final String SHOW_RESULTS_COMMAND = "SHOW_RESULTS\t"; //$NON-NLS-1$
  protected List<SpecialDie> dice = new ArrayList<>();
  protected java.util.Random ran;
  protected boolean reportResultAsText = true;
  protected boolean reportResultInWindow = false;
  protected boolean reportResultInButton = false;
  protected String id;
  protected String sMapName;
  protected JDialog dialog; // Dialog to show results graphical
  protected JLabel dialogLabel;
  protected Color bgColor;
  protected ResultsIcon resultsIcon = new ResultsIcon();
  protected FormattedString format = new FormattedString();
  protected String chatResultFormat = "** $" + NAME + "$ = [$result1$] *** &lt;$" + GlobalOptions.PLAYER_NAME + "$&gt;"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  protected String windowTitleResultFormat = "$" + NAME + "$"; //$NON-NLS-1$ //$NON-NLS-2$
  protected String tooltip = ""; //$NON-NLS-1$
  protected final MutableProperty.Impl property = new Impl("", this); //$NON-NLS-1$
  protected String description;
  // FIX: New field to store the previous roll results for Undo
  private int[] lastRollResults = EMPTY;


  public static final String RESULT_CHATTER = "resultChatter"; //$NON-NLS-1$
  public static final String CHAT_RESULT_FORMAT = "format"; //$NON-NLS-1$
  public static final String RESULT_N = "result#"; //$NON-NLS-1$
  public static final String RESULT_TOTAL = "numericalTotal"; //$NON-NLS-1$
  public static final String RESULT_WINDOW = "resultWindow"; //$NON-NLS-1$
  public static final String WINDOW_TITLE_RESULT_FORMAT = "windowTitleResultFormat"; //$NON-NLS-1$
  public static final String RESULT_BUTTON = "resultButton"; //$NON-NLS-1$
  public static final String WINDOW_X = "windowX"; //$NON-NLS-1$
  public static final String WINDOW_Y = "windowY"; //$NON-NLS-1$
  public static final String BACKGROUND_COLOR = "backgroundColor"; //$NON-NLS-1$
  public static final String DICE_SET = "diceSet"; //$NON-NLS-1$
  public static final String NONE = "&lt;none&gt;"; //$NON-NLS-1$
  public static final String DESCRIPTION = "description"; //NON-NLS
  private static final int[] EMPTY = new int[0];

  // These five identical to AbstractToolbarItem, and are only here for "clirr purposes"
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String NAME = "name"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String HOTKEY = "hotkey"; //$NON-NLS-1$

  public SpecialDiceButton() {
    super(false); // Make a DoActionButton, but don't call its normal constructor

    dialog = new JDialog(GameModule.getGameModule().getPlayerWindow());
    dialog.setLayout(new MigLayout("ins 0")); //NON-NLS
    dialogLabel = new JLabel();
    dialogLabel.setIcon(resultsIcon);
    dialog.add(dialogLabel);

    // ACTION LISTENER: Reverts to calling DR() and letting DR() handle logging/execution.
    final ActionListener rollAction = e -> {
      try {
        DR();
      }
      catch (RecursionLimitException ex) {
        RecursionLimiter.infiniteLoop(ex);
      }
    };

    final String desc = Resources.getString("Editor.SpecialDiceButton.symbols"); //$NON-NLS-1$
    setLaunchButton(makeLaunchButton(desc, desc, "/images/die.gif", rollAction)); //NON-NLS
    setAttribute(NAME, desc);
    setNamePrompt(Resources.getString(Resources.NAME_LABEL));
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.SpecialDiceButton.component_type"); //$NON-NLS-1$
  }

  @Override
  public String getDescription() {
    return description;
  }

  /**
   * The text reported before the results of the roll
   */
  protected String getReportPrefix() {
    return " *** " + getConfigureName() + " = "; //$NON-NLS-1$ //$NON-NLS-2$
  }


  /**
   * Forwards the result of the roll to the {@link Chatter#send} method of the {@link Chatter} of the {@link GameModule}.
   * Format is prefix+[comma-separated roll list]+suffix additionally a command for every die is generated
   * **REVISION**: Logs the old results to enable single-step undo for the visual state.
   */
  protected void DR() throws RecursionLimitException {
    // 1. Capture the visual state *before* the roll for the undo command
    final int[] previousRollResults = Arrays.copyOf(lastRollResults, lastRollResults.length);

    // 2. Roll the new dice
    final int[] newRollResults = new int[dice.size()];
    int i = 0;
    for (final SpecialDie sd : dice) {
      final int faceCount = sd.getFaceCount();
      newRollResults[i++] = faceCount == 0 ? 0 : ran.nextInt(sd.getFaceCount());
    }

    // 3. Set format, properties, and execute local updates for the *new* roll
    setFormat(newRollResults);
    Command chatAndPropertyCommand = new NullCommand();
    if (reportResultAsText) {
      chatAndPropertyCommand = chatAndPropertyCommand.append(reportTextResults(newRollResults));
    }
    // This command is the one that's reversed to undo the property change
    Command propertyUpdateCommand = property.setPropertyValue(String.valueOf(getTotal(newRollResults)));
    chatAndPropertyCommand = chatAndPropertyCommand.append(propertyUpdateCommand);

    resultsIcon.setResults(newRollResults);
    // Execute chat/prop immediately to update local state
    chatAndPropertyCommand.execute();

    // The lastRollResults tracker must be updated for local visual execution too!
    lastRollResults = newRollResults;

    if (getLaunchButton() != null) {
      getLaunchButton().repaint();
    }
    if (reportResultInWindow) {
      dialogLabel.setSize(new Dimension(resultsIcon.width, resultsIcon.height));
      dialogLabel.setMinimumSize(new Dimension(resultsIcon.width, resultsIcon.height));
      format.setFormat(windowTitleResultFormat);
      dialog.setTitle(format.getLocalizedText(this, "Editor.SpecialDiceButton.window_title"));
      dialog.pack();
      dialog.setVisible(true);
      dialogLabel.repaint();
    }
    // *************************************************************************

    // FIX: Integration of Looping and Hotkey execution from DoActionButton.doActions()

    final Command c = chatAndPropertyCommand; // Start with the dice roll/prop update command
    final GameModule mod = GameModule.getGameModule();

    // Check for looping, as defined in DoActionButton.doActions()
    if (!doLoop) {
      // Non looping case: Execute the base actions and log the whole chain
      executeActions(c); // Adds sound/hotkeys/report to c
      Command showResultsCommand = new ShowResults(this, newRollResults, previousRollResults);
      mod.sendAndLog(c.append(showResultsCommand));
      return;
    }

    // Looping case (restored from DoActionButton.doActions())
    // Note: We use the index/loop variables from the superclass (this)

    // Set up Index Property
    indexValue = indexStart;
    setIndexPropertyValue();

    // Issue the Pre-loop key
    doHotKey(c, preLoopKey);

    // Set up counters for a counted loop
    int loopCounter = 0;
    int loopCountLimit = 1;
    final PropertySource ps = (PropertySource) GameModule.getGameModule(); // Simplified PropertySource lookup

    if (LoopControl.LOOP_COUNTED.equals(loopType)) {
      loopCountLimit = loopCount.getTextAsInt(ps, Resources.getString("Editor.LoopControl.loop_count"), this);
    }

    RecursionLimitException loopException = null;

    if (loopCountLimit > 0) {
      for (;;) {

        // While loop - test condition is still true before actions
        if (LoopControl.LOOP_WHILE.equals(loopType)) {
          if (!whileExpression.isTrue(ps)) {
            break;
          }
        }

        // Execute the base actions and catch looping.
        try {
          executeActions(c); // Adds sound/hotkeys/report to c
        }
        catch (RecursionLimitException ex) {
          loopException = ex;
          break;
        }

        // Until loop - test condition is not false after loop
        if (LoopControl.LOOP_UNTIL.equals(loopType)) {
          if (untilExpression.isTrue(ps)) {
            break;
          }
        }

        // Counted loop - Check if looped enough times
        loopCounter++;
        if (LoopControl.LOOP_COUNTED.equals(loopType)) {
          if (loopCounter >= loopCountLimit) {
            break;
          }
        }
        // Otherwise check for too much looping.
        else {
          if (loopCounter >= LoopControl.LOOP_LIMIT) {
            loopException = new RecursionLimitException(this);
            break;
          }
        }

        // Increment the Index Variable
        indexValue += indexStep;
        setIndexPropertyValue();
      }
    }

    // Issue the Post-loop key
    doHotKey(c, postLoopKey);

    // Now that the full action chain (c) is built, append the visual update (undo fix) and log the whole thing.
    Command showResultsCommand = new ShowResults(this, newRollResults, previousRollResults);
    mod.sendAndLog(c.append(showResultsCommand));

    // If the loop ended due to excessive looping, throw the Exception out to the caller.
    if (loopException != null) {
      throw loopException;
    }
  }

  // Helper method to get total, simplified logic
  private int getTotal(int[] results) {
    int total = 0;
    for (int i = 0; i < dice.size() && i < results.length; ++i) {
      total += dice.get(i).getIntValue(results[i]);
    }
    return total;
  }

  // This method is ONLY called by the ShowResults command (for remote clients and Redo/Undo command execution).
  private Command reportResults(int[] results) {
    // FIX: Set resultsIcon for the visual state update
    resultsIcon.setResults(results);

    if (reportResultInWindow) {
      dialogLabel.setSize(new Dimension(resultsIcon.width, resultsIcon.height));
      dialogLabel.setMinimumSize(new Dimension(resultsIcon.width, resultsIcon.height));
      format.setFormat(windowTitleResultFormat);
      dialog.setTitle(format.getLocalizedText(this, "Editor.SpecialDiceButton.window_title"));
      dialog.pack();
      dialog.setVisible(true);
      dialogLabel.repaint();
    }
    if (reportResultInButton) {
      // FIX: Repaint button for the visual state update
      getLaunchButton().repaint();
    }
    return new NullCommand();
  }

  /**
   * Report the results of the roll as text to the chat window.
   *
   * @param results the results of the roll (unused in body, but needed for consistent method signature)
   */
  @SuppressWarnings({"PMD.UnusedFormalParameter"}) // Suppress warning as 'results' is indirectly used by being passed to this method for compatibility/call sequencing.
  private Command reportTextResults(int[] results) {
    format.setFormat(chatResultFormat);
    String msg = format.getLocalizedText(this, "Editor.report_format");
    if (!msg.isEmpty()) {
      if (msg.startsWith("*")) { //$NON-NLS-1$
        msg = "*" + msg; //$NON-NLS-1$
      }
      else {
        msg = "* " + msg; //$NON-NLS-1$
      }
    }
    return msg.isEmpty() ? new NullCommand() : new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
  }

  protected void setFormat(int[] results) {
    format.setProperty(NAME, getLocalizedConfigureName());
    int total = 0;
    for (int i = 0; i < dice.size() && i < results.length; ++i) {
      final SpecialDie die = dice.get(i);
      format.setProperty("result" + (i + 1), die.getTextValue(results[i])); //$NON-NLS-1$
      total += die.getIntValue(results[i]);
    }
    format.setProperty(RESULT_TOTAL, String.valueOf(total)); //$NON-NLS-1$
    format.setFormat(chatResultFormat);
  }

  /**
  * The Attributes of a DiceButton are:
   * <code>BUTTON_TEXT</code> the label of the button in the toolbar <code>ICON</code> the icon of the button in the
   * toolbar <code>HOTKEY</code> the hotkey equivalent of the button <code>DICE_SET</code> list of dice sets, an
   * entry can be: [number]name of die[+|-modifier] "name of die" must be SpecialDie "modifier" is added/subtracted
   * to/from total of dice [number]Dnumber of sides (e.g. 2D6) <code>NUMERIC</code> result of all dice is numeric
   * <code>REPORT_TOTAL</code> If numeric and true, add the results of the dice together and report the total.
   * Otherwise, report the individual results <code>SORT</code> if true sort results per die by numeric value
   * <code>RESULT_CHATTER</code> if true report results in chatter <code>RESULT_WINDOW</code> if true show result
   * graphical in extra window <code>WINDOW_X</code> width of window or button <code>WINDOW_Y</code> height of
   * window or button <code>RESULT_MAP</code> :TODO: if true show result in special area in map <code>MAP_NAME</code>
   * :TODO: name of map <code>RESULT_BUTTON</code> if true show result graphical in button
   */
  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      DESCRIPTION,
      RESULT_CHATTER,
      CHAT_RESULT_FORMAT,
      RESULT_WINDOW,
      WINDOW_TITLE_RESULT_FORMAT,
      RESULT_BUTTON,
      WINDOW_X,
      WINDOW_Y,
      BACKGROUND_COLOR
    );
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString(Resources.DESCRIPTION),
      Resources.getString("Editor.SpecialDiceButton.report_results_text"), //$NON-NLS-1$
      Resources.getString("Editor.report_format"), //$NON-NLS-1$
      Resources.getString("Editor.SpecialDiceButton.result_window"), //$NON-NLS-1$
      Resources.getString("Editor.SpecialDiceButton.window_title"), //$NON-NLS-1$
      Resources.getString("Editor.SpecialDiceButton.result_button"), //$NON-NLS-1$
      Resources.getString("Editor.width"), //$NON-NLS-1$
      Resources.getString("Editor.height"), //$NON-NLS-1$
      Resources.getString("Editor.background_color") //$NON-NLS-1$
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      String.class,
      Boolean.class,
      ReportFormatConfig.class,
      Boolean.class,
      ReportFormatConfig.class,
      Boolean.class,
      Integer.class,
      Integer.class,
      Color.class
    );
  }

  /** @deprecated Use {@link VASSAL.build.AbstractToolbarItem.IconConfig} instead. */
  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/die.gif"); //$NON-NLS-1$
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedExpressionConfigurer(key, name, new String[]{NAME, RESULT_N, RESULT_TOTAL});
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    // get size only when output in window or on button
    if (List.of(WINDOW_X, WINDOW_Y, BACKGROUND_COLOR).contains(name)) {
      return () -> reportResultInWindow || reportResultInButton;
    }
    else if (CHAT_RESULT_FORMAT.equals(name)) {
      return () -> reportResultAsText;
    }
    else if (WINDOW_TITLE_RESULT_FORMAT.equals(name)) {
      return () -> reportResultInWindow;
    }
    else if (REPORT_FORMAT.equals(name) || DO_REPORT.equals(name)) {
      return () -> false; // SpecialDiceButton always handles its own reporting/chat logic
    }
    else
      return super.getAttributeVisibility(name); // AbstractToolbarItem
  }

  public void addSpecialDie(SpecialDie d) {
    dice.add(d);
  }

  public void removeSpecialDie(SpecialDie d) {
    dice.remove(d);
  }

  private HierarchyListener hl;

  /**
   * Expects to be added to a SymbolDice. Adds the button to the control window's toolbar and registers itself as a
   * {@link KeyStrokeListener}
   */
  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder)parent).getNonFolderAncestor();
    }

    resultsIcon.setResults(new int[dice.size()]);
    // FIX: Set lastRollResults to the initial state (empty/zero)
    lastRollResults = new int[dice.size()];

    final LaunchButton lb = getLaunchButton();

    lb.setForceVisible(reportResultInButton);
    if (reportResultInButton) {
      lb.setIcon(resultsIcon);
    }

    hl = new HierarchyListener() {
      @Override
      public void hierarchyChanged(HierarchyEvent e) {
        if (lb.isShowing()) {
          dialog.setLocationRelativeTo(lb);
          lb.removeHierarchyListener(this);
        }
      }
    };
    lb.addHierarchyListener(hl);
    final GameModule mod = GameModule.getGameModule();
    mod.getGameState().addGameComponent(this);
    ran = mod.getRNG();
    mod.getToolBar().add(lb);
    idMgr.add(this);
    mod.addCommandEncoder(this);
    property.addTo((MutablePropertiesContainer)parent);

    // Ensure the loop index property from the base class is also managed if applicable
    updateLoopPropertyRegistration();
  }

  @Override
  public void removeFrom(Buildable b) {
    final GameModule mod = GameModule.getGameModule();
    mod.removeCommandEncoder(this);
    mod.getToolBar().remove(getLaunchButton());
    mod.getToolBar().revalidate();
    mod.getGameState().removeGameComponent(this);
    idMgr.remove(this);
    getLaunchButton().removeHierarchyListener(hl);
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public String getId() {
    return id;
  }

  /**
   * Make a best guess for a unique identifier for the target. Use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getConfigureName} if non-null, otherwise use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getId}
   */
  public String getIdentifier() {
    return UniqueIdManager.getIdentifier(this);
  }

  /**
   * Get boolean value of object.
   *
   * @param o object as input for setAttribute()
   * @return boolean value of object
   */
  private boolean getBoolVal(Object o) {
    if (o instanceof Boolean) {
      return (Boolean) o;
    }
    else {
      return o instanceof String && "true".equals(o); //$NON-NLS-1$
    }
  }

  @Override
  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
      if (property != null) {
        property.setPropertyName(getConfigureName() + "_result"); //$NON-NLS-1$
      }
      if (getLaunchButton() != null) {
        getLaunchButton().setToolTipText((String) o);
      }
    }
    else if (RESULT_CHATTER.equals(key)) {
      reportResultAsText = getBoolVal(o);
    }
    else if (CHAT_RESULT_FORMAT.equals(key)) {
      chatResultFormat = (String) o;
    }
    else if (RESULT_BUTTON.equals(key)) {
      reportResultInButton = getBoolVal(o);
      if (reportResultInButton) {
        if (getLaunchButton() != null) {
          getLaunchButton().setIcon(resultsIcon);
        }
      }
    }
    else if (RESULT_WINDOW.equals(key)) {
      reportResultInWindow = getBoolVal(o);
    }
    else if (WINDOW_TITLE_RESULT_FORMAT.equals(key)) {
      windowTitleResultFormat = (String) o;
    }
    else if (WINDOW_X.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      resultsIcon.width = (Integer) o;
      dialog.pack();
    }
    else if (WINDOW_Y.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      resultsIcon.height = (Integer) o;
      dialog.pack();
    }
    else if (BACKGROUND_COLOR.equals(key)) {
      if (o instanceof String) {
        o = ColorConfigurer.stringToColor((String) o);
      }
      bgColor = (Color) o;
    }
    else if (TOOLTIP.equals(key)) {
      tooltip = (String) o;
      if (getLaunchButton() != null) {
        getLaunchButton().setAttribute(key, o);
      }
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String)o;
    }
    else if (DO_REPORT.equals(key)) {
      doReport = false; // Always false, handled by dice-specific logic
    }
    else {
      super.setAttribute(key, o);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (RESULT_CHATTER.equals(key)) {
      return String.valueOf(reportResultAsText);
    }
    else if (CHAT_RESULT_FORMAT.equals(key)) {
      return chatResultFormat;
    }
    else if (RESULT_BUTTON.equals(key)) {
      return String.valueOf(reportResultInButton);
    }
    else if (RESULT_WINDOW.equals(key)) {
      return String.valueOf(reportResultInWindow);
    }
    else if (WINDOW_TITLE_RESULT_FORMAT.equals(key)) {
      return windowTitleResultFormat;
    }
    else if (WINDOW_X.equals(key)) {
      return String.valueOf(resultsIcon.width);
    }
    else if (WINDOW_Y.equals(key)) {
      return String.valueOf(resultsIcon.height);
    }
    else if (BACKGROUND_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(bgColor);
    }
    else if (TOOLTIP.equals(key)) {
      return tooltip.isEmpty() ? getLaunchButton().getAttributeValueString(BUTTON_TEXT) : tooltip;
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    else if (DO_REPORT.equals(key)) {
      return String.valueOf(false); // Always false
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {SpecialDie.class};
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SpecialDiceButton.html"); //$NON-NLS-1$
  }

  /**
   * create String from int array
   *
   * @param ia
   * int-array
   * @return encoded String
   */
  public static String intArrayToString(int[] ia) {
    if (ia == null || ia.length == 0) {
      return ""; //$NON-NLS-1$
    }
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final int value : ia) {
      se.append(String.valueOf(value));
    }
    return se.getValue();
  }

  /**
   * get int array from string
   *
   * @param s
   * string with encoded int array
   * @return int array
   */
  public static int[] stringToIntArray(String s) {
    if (s == null || s.isEmpty()) {
      return EMPTY;
    }
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    final List<String> l = new ArrayList<>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    final int[] val = new int[l.size()];
    for (int i = 0; i < val.length; ++i) {
      val[i] = Integer.parseInt(l.get(i));
    }
    return val;
  }

  /**
   * Implement PropertyNameSource - Expose roll result property
   */
  @Override
  public List<String> getPropertyNames() {
    final List<String> l = super.getPropertyNames();
    l.add(getConfigureName() + "_result"); //NON-NLS
    return l;
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof ShowResults)) {
      return null;
    }
    final ShowResults c2 = (ShowResults) c;
    // Only encode the current roll results. The previous results are for local undo logic.
    final SequenceEncoder se = new SequenceEncoder(c2.target.getIdentifier(), '\t');
    for (int i = 0; i < c2.rolls.length; ++i) {
      se.append(Integer.toString(c2.rolls[i])); //$NON-NLS-1$
    }
    return SHOW_RESULTS_COMMAND + se.getValue();
  }

  @Override
  public Command decode(String s) {
    SequenceEncoder.Decoder st = null;
    if (s.startsWith(SHOW_RESULTS_COMMAND + getConfigureName()) || s.startsWith(SHOW_RESULTS_COMMAND + getId())) {
      st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
      st.nextToken();
    }
    else if (s.startsWith(getId() + '\t')) { // Backward compatibility
      st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
    }
    if (st == null) {
      return null;
    }
    final List<String> l = new ArrayList<>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    final int[] results = new int[l.size()];
    int i = 0;
    for (final String n : l) {
      results[i++] = Integer.parseInt(n);
    }
    // When decoding for remote clients/redo, we don't have the previous results, so we pass EMPTY.
    // The three-argument constructor will be used, and EMPTY will be stored for previousRolls.
    return new ShowResults(this, results, EMPTY);
  }
  /**
   * Command for displaying the results of a roll of the dice
   * **REVISION**: `executeCommand()` now correctly updates the button's internal `lastRollResults`
   * state, fixing the chain-of-undo bug.
   * **COMPATIBILITY FIX**: Added back the old two-argument constructor.
   */
  public static class ShowResults extends Command {
    private final SpecialDiceButton target;
    private final int[] rolls;           // State to apply on execute/redo
    private final int[] previousRolls;   // State to apply on undo

    /**
     * **BINARY COMPATIBILITY CONSTRUCTOR**
     * This is the old constructor signature that must be preserved.
     * It defaults the previous roll to an empty array.
     */
    @Deprecated
    public ShowResults(SpecialDiceButton oTarget, int[] rollsToApply) {
      this(oTarget, rollsToApply, EMPTY);
    }


    public ShowResults(SpecialDiceButton oTarget, int[] rollsToApply, int[] previousRollsToRestore) {
      target = oTarget;
      rolls = Arrays.copyOf(rollsToApply, rollsToApply.length);
      previousRolls = Arrays.copyOf(previousRollsToRestore, previousRollsToRestore.length);
    }

    @Override
    protected void executeCommand() {
      // This applies the "rolls" state (the forward state)
      target.setFormat(rolls);
      target.reportResults(rolls);
      // CRITICAL FIX: Update the visual state tracker in the button when the command is executed
      target.lastRollResults = rolls;
    }

    @Override
    protected Command myUndoCommand() {
      // This is executed when the current command is undone.
      // We return a *new* ShowResults command that will execute the visual state
      // of the *previous* roll (previousRolls), and put the current state (rolls)
      // on its own Redo stack.
      return new ShowResults(target, previousRolls, rolls);
    }
  }

  /** Icon class for graphical display of a dice roll */
  private class ResultsIcon implements Icon {
// FIXME: because Sun checks what class Icon implementations are,
// this won't display as disabled properly

    private int width, height;
    private Icon[] icons;

    public ResultsIcon() {
    }

    private void setResults(int[] results) {
      icons = new Icon[results.length];
      if (results.length > dice.size()) {
        logger.warn(
                "Special Die Button (" + getConfigureName() + //NON-NLS
                        "): more results (" + results.length + ") requested than dice (" + //NON-NLS
                        dice.size() + ")" //NON-NLS
        );
      }
      for (int i = 0; i < results.length; ++i) {
        if (i >= dice.size()) break;
        final String imageName = dice.get(i).getImageName(results[i]);

        if (!imageName.isEmpty()) {
          final ImageOp sop = Op.load(imageName);
          if (sop.getImage() != null) {
            icons[i] = new ImageIcon(new OwningOpMultiResolutionImage(sop));
          }
        }
        else {
          icons[i] = null;
        }
      }
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
      if (bgColor != null) {
        g.setColor(bgColor);
        g.fillRect(x, y, width, height);
      }
      int offset = 0;
      for (final Icon icon : icons) {
        if (icon != null) {
          icon.paintIcon(c, g, x + offset, y);
          offset += icon.getIconWidth();
        }
      }
    }

    @Override
    public int getIconWidth() {
      return width;
    }

    @Override
    public int getIconHeight() {
      return height;
    }
  }


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    final List<String> list = super.getFormattedStringList();
    list.add(windowTitleResultFormat);
    list.add(chatResultFormat);
    return list;
  }

  /**
   * In case reports use HTML and  refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    super.addLocalImageNames(s);
    final HTMLImageFinder h = new HTMLImageFinder(chatResultFormat);
    h.addImageNames(s);
  }
}