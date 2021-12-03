/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.MutableProperty.Impl;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.ConfigurerWindow;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PlayerIdFormattedExpressionConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.swing.SwingUtils;
import org.apache.commons.lang3.ArrayUtils;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button generates random numbers and displays the
 * result in the Chatter */
public class DiceButton extends AbstractToolbarItem {
  protected java.util.Random ran;
  protected int nSides = 6, nDice = 2, plus = 0, addToTotal = 0;
  protected boolean reportTotal = false;
  protected boolean promptAlways = false;
  protected boolean sortDice = false;
  protected final FormattedString reportFormat = new FormattedString("** $" + REPORT_NAME + "$ = $" + RESULT + "$ *** &lt;$" + GlobalOptions.PLAYER_NAME + "$&gt;"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

  /** Locking of options for roll pronpts */
  protected boolean lockSides = false;
  protected boolean lockDice = false;
  protected boolean lockPlus = false;
  protected boolean lockAdd = false;

  /** A list of individual rolls kept */
  protected int[] keepDice;

  /** Total of all dice kept, including per-die and per-roll adjustments */
  protected int numericTotal;

  /** Are we keeping only specific rolls? */
  protected boolean keepingDice = false;

  /** What rule are we using to keep rolls? */
  protected String keepOption = KEEP_GREATER;

  /** How many rolls, or what size rolls are we keeping? */
  protected int keepValue = 1;

  /** Number of dice kept from those rolled */
  protected int keepCount;

  /** The Raw rolls before keeping */
  protected int[] rawRolls;

  /** Counts of each face prior to keeping */
  protected int[] rawCounts;

  /** Counts of each modified face of kept dice */
  protected int[] counts;

  /** Keep Options */
  protected static final String KEEP_SMALLEST = "s";
  protected static final String KEEP_LARGEST = "l";
  protected static final String KEEP_EQUAL = "=";
  protected static final String KEEP_GREATER = ">";
  protected static final String KEEP_LESS = "<";

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected String tooltip = ""; //$NON-NLS-1$
  protected final MutableProperty.Impl property = new Impl("", this);
  protected final MutableProperty.Impl totalProp = new Impl("", this);
  protected final MutableProperty.Impl keepProp = new Impl("", this);

  // These five identical to AbstractToolbarItem, and are only here for "clirr purposes"
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String NAME = "name"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; //$NON-NLS-1$
  @Deprecated(since = "2020-10-21", forRemoval = true) public static final String HOTKEY = "hotkey"; //$NON-NLS-1$

  public static final String DEPRECATED_NAME = "label"; //$NON-NLS-1$
  public static final String N_DICE = "nDice"; //$NON-NLS-1$
  public static final String N_SIDES = "nSides"; //$NON-NLS-1$
  public static final String PLUS = "plus"; //$NON-NLS-1$
  public static final String ADD_TO_TOTAL = "addToTotal"; //$NON-NLS-1$
  public static final String REPORT_TOTAL = "reportTotal"; //$NON-NLS-1$
  public static final String PROMPT_ALWAYS = "prompt"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$
  public static final String SORT_DICE_RESULTS = "sortDice"; //NON-NLS

  public static final String KEEP_DICE = "keepDice";
  public static final String KEEP_OPTION = "keepOption";
  public static final String KEEP_COUNT = "keepCount";

  public static final String LOCK_DICE = "lockDice";
  public static final String LOCK_SIDES = "lockSides";
  public static final String LOCK_PLUS = "lockPlus";
  public static final String LOCK_ADD = "lockAdd";

  /** Variable name for reporting format */
  public static final String RESULT = "result"; //$NON-NLS-1$
  public static final String REPORT_NAME = "name"; //$NON-NLS-1$
  public static final String RESULT_N = "result#"; //$NON-NLS-1$
  public static final String NUMERIC_TOTAL = "numericalTotal"; //$NON-NLS-1$

  public DiceButton() {
    initLaunchButton();
  }

  protected void initLaunchButton() {
    final ActionListener rollAction = e -> {

      // Determine which elements are not locked
      final List<String> keepAttributes = new ArrayList<>();
      if (promptAlways) {
        if (!lockDice) {
          keepAttributes.add(N_DICE);
        }
        if (!lockSides) {
          keepAttributes.add(N_SIDES);
        }
        if (!lockPlus) {
          keepAttributes.add(PLUS);
        }
        if (!lockAdd) {
          keepAttributes.add(ADD_TO_TOTAL);
        }
      }

      // Only show a prompt if at least one of the 4 attributes is not locked, otherwise it is a standard roll
      if (promptAlways && !keepAttributes.isEmpty()) {
        final DiceButton delegate = new DiceButton() {
          @Override
          protected void initLaunchButton() {
            setLaunchButton(makeLaunchButton("", AbstractToolbarItem.BUTTON_TEXT, "", null));
          }
        };

        for (final String key : keepAttributes) {
          delegate.setAttribute(key, getAttributeValueString(key));
        }

        final AutoConfigurer ac = new AutoConfigurer(delegate);
        final ConfigurerWindow w = new ConfigurerWindow(ac, true);
        w.setTitle(getConfigureName());
        for (final String key : getAttributeNames()) {
          if (!keepAttributes.contains(key)) {
            final Component controls = ac.getConfigurer(key).getControls();
            final Container parent = controls.getParent();
            parent.remove(controls);
            parent.remove(ac.getLabel(key));
          }
        }
        w.setLocationRelativeTo(getLaunchButton().getTopLevelAncestor());
        SwingUtils.repack(w, true);
        w.setVisible(true);

        for (final String key : keepAttributes) {
          setAttribute(key, delegate.getAttributeValueString(key));
        }
        if (! w.isCancelled()) {
          DR();
        }
      }
      else {
        DR();
      }
    };

    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.DiceButton.dice_button_text"),
      Resources.getString("Editor.DiceButton.dice_button_tooltip"),
      "/images/die.gif", //NON-NLS
      rollAction
    ));
    launch = getLaunchButton(); // for compatibility

    setAttribute(AbstractToolbarItem.NAME, Resources.getString("Editor.DiceButton.dice_name")); //NON-NLS
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DiceButton.component_type"); //$NON-NLS-1$
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format is
   * prefix+[comma-separated roll list]+suffix
   *
   * New-style reporting is numericTotal and individual results
   * */
  protected void DR() {

    keepDice = new int[nDice];
    numericTotal = addToTotal;
    keepCount = 0;

    rawRolls = new int[nDice];
    rawCounts = new int[nSides];
    counts = new int[nSides];

    // Make nDice rolls.
    for (int i = 0; i < nDice; ++i) {

      // Roll next die
      final int rawRoll = ran.nextInt(nSides) + 1;
      final int roll = rawRoll + plus;

      rawRolls[i] = rawRoll;
      rawCounts[rawRoll - 1] += 1;

      // Handle Keep ==, >= or <= here. Just totally ignore them if they are out of range.
      if (keepingDice) {
        if (KEEP_GREATER.equals(keepOption) && roll < keepValue) {
          continue;
        }
        else if (KEEP_LESS.equals(keepOption) && roll > keepValue) {
          continue;
        }
        else if (KEEP_EQUAL.equals(keepOption) && roll != keepValue) {
          continue;
        }
      }

      keepCount += 1;
      keepDice[keepCount - 1] = roll;
      numericTotal += roll;
      counts[rawRoll - 1] += 1;
    }

    // Reduce the size of the kept Dice array if necessary to allow for correct sorting
    if (keepCount < keepDice.length) {
      keepDice = Arrays.copyOf(keepDice, keepCount);
    }

    // if Keep smallest or Keep largest requested, rebuild keepDice and numericTotal
    // with just the requested dice
    if (List.of(KEEP_SMALLEST, KEEP_LARGEST).contains(keepOption)) {
      final int[] tempDice = Arrays.copyOf(keepDice, keepDice.length);
      Arrays.sort(tempDice);

      // New Keep count is the number asked for, or the number of dice rolled, whichever is smaller.
      keepCount = Math.max(0, Math.min(keepValue, tempDice.length));
      keepDice = new int[keepCount];

      // tempDice has been sorted, so can just pull the required number from the the front or back of the array
      if (KEEP_SMALLEST.equals(keepOption)) {
        System.arraycopy(tempDice, 0, keepDice, 0, keepCount);
      }
      else {
        for (int i = 0; i < keepCount; i++) {
          keepDice[i] = tempDice[tempDice.length - keepCount + i];
        }
      }

      // Re-calculate the total
      numericTotal = addToTotal;
      for (int i = 0; i < keepCount; i++) {
        numericTotal += keepDice[i];
      }
    }

    // Sort what we are keeping if requested
    if (sortDice) {
      Arrays.sort(keepDice);
    }

    // Build the legacy $result$ string
    final StringBuilder val = new StringBuilder();
    if (reportTotal) {
      val.append(numericTotal);
    }
    else {
      for (int i = 0; i < keepCount; ++i) {
        val.append(keepDice[i]);
        if (i < keepCount - 1) {
          val.append(',');
        }
      }
    }

    final String report = formatResult(val.toString());
    Command c = report.length() == 0 ? new NullCommand() : new Chatter.DisplayText(GameModule.getGameModule().getChatter(), report);
    c.execute();
    c = c.append(property.setPropertyValue(val.toString()))
      .append(totalProp.setPropertyValue(Integer.toString(numericTotal)))
      .append(keepProp.setPropertyValue(Integer.toString(keepCount)));
    GameModule.getGameModule().sendAndLog(c);
  }

  /**
   * Use the configured FormattedString to format the result of a roll
   * @param result Result format
   * @return Formatted result
   */
  protected String formatResult(final String result) {
    reportFormat.clearProperties();

    reportFormat.setProperty(REPORT_NAME, getLocalizedConfigureName());
    reportFormat.setProperty(RESULT, result);
    reportFormat.setProperty(N_DICE, Integer.toString(nDice));
    reportFormat.setProperty(N_SIDES, Integer.toString(nSides));
    reportFormat.setProperty(PLUS, Integer.toString(plus));
    reportFormat.setProperty(ADD_TO_TOTAL, Integer.toString(addToTotal));

    // Set the $resultn$ values for kept dice only
    for (int i = 0; i < keepCount; i++) {
      reportFormat.setProperty("result" + (i + 1), Integer.toString(keepDice[i]));
    }
    reportFormat.setProperty(NUMERIC_TOTAL, Integer.toString(numericTotal));
    reportFormat.setProperty(KEEP_DICE, Integer.toString(keepValue));
    reportFormat.setProperty(KEEP_COUNT, Integer.toString(keepCount));

    // Raw Rolls
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < nDice; ++i) {
      if (i > 0) {
        sb.append(", ");
      }
      sb.append(Integer.toString(rawRolls[i]));
    }
    reportFormat.setProperty("rawRolls", sb.toString());

    // Counts
    for (int i = 0; i < nSides; ++i) {
      reportFormat.setProperty("rawCount" + (i + 1), Integer.toString(rawCounts[i]));
      reportFormat.setProperty("count" + (i + 1), Integer.toString(counts[i]));
    }

    final String text = reportFormat.getLocalizedText(this, "Editor.report_format");
    String report = text;
    if (text.length() > 0) {
      report = text.startsWith("*") ? "*" + text : "* " + text; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
    return report;
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      REPORT_FORMAT,
      PROMPT_ALWAYS,
      N_DICE,
      LOCK_DICE,
      N_SIDES,
      LOCK_SIDES,
      PLUS,
      LOCK_PLUS,
      ADD_TO_TOTAL,
      LOCK_ADD,
      REPORT_TOTAL,
      SORT_DICE_RESULTS,
      KEEP_DICE,
      KEEP_OPTION,
      KEEP_COUNT
    );
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.report_format"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.prompt_value"), //$NON-NLS-1$
      Resources.getString("Dice.number_of_dice"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.lock_number_of_dice"), //$NON-NLS-1$
      Resources.getString("Dice.number_of_sides"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.lock_number_of_sides"), //$NON-NLS-1$
      Resources.getString("Dice.add_to_each_side"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.lock_add_to_each_side"), //$NON-NLS-1$
      Resources.getString("Dice.add_to_total"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.lock_add_to_total"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.report_total"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.sort_results"), //$NON-NLS-1$
      Resources.getString("Editor.DiceButton.keep_dice"),
      Resources.getString("Editor.DiceButton.keep_option"),
      Resources.getString("Editor.DiceButton.keep_count")
    );
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(final AutoConfigurable c, final String key, final String name) {
      return new IconConfigurer(key, name, "/images/die.gif"); //$NON-NLS-1$
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(final AutoConfigurable c, final String key, final String name) {
      return new PlayerIdFormattedExpressionConfigurer(key, name, new String[]{REPORT_NAME, RESULT, NUMERIC_TOTAL, RESULT_N, N_DICE, N_SIDES, PLUS, ADD_TO_TOTAL, KEEP_DICE, KEEP_COUNT});
    }
  }

  public static class KeepConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new TranslatingStringEnumConfigurer(key, name,
        new String[] {KEEP_GREATER, KEEP_EQUAL, KEEP_LESS, KEEP_LARGEST, KEEP_SMALLEST},
        new String[] {"Editor.DiceButton.keep_greater_than", "Editor.DiceButton.keep_equal", "Editor.DiceButton.keep_less_than", "Editor.DiceButton.keep_largest", "Editor.DiceButton.keep_smallest"});
    }
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      ReportFormatConfig.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      KeepConfig.class,
      Integer.class
    );
  }

  private final VisibilityCondition cond = () -> promptAlways;

  //  private final VisibilityCondition canSort = () -> !reportTotal;

  private final VisibilityCondition keep = () -> keepingDice;

  @Override
  public VisibilityCondition getAttributeVisibility(final String name) {
    if (List.of(LOCK_DICE, LOCK_SIDES, LOCK_PLUS, LOCK_ADD).contains(name)) {
      return cond;
    }
    else if (List.of(KEEP_OPTION, KEEP_COUNT).contains(name)) {
      return keep;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  /**
   * Expects to be added to a GameModule.  Adds the button to the
   * control window's toolbar and registers itself as a @link
   * KeyStrokeListener */
  @Override
  public void addTo(Buildable parent) {
    super.addTo(parent);

    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder)parent).getNonFolderAncestor();
    }

    ran = GameModule.getGameModule().getRNG();
    property.setPropertyValue("1"); // Initialize with a numeric value //$NON-NLS-1$
    property.addTo((MutablePropertiesContainer)parent);

    totalProp.setPropertyValue("0");
    totalProp.addTo((MutablePropertiesContainer)parent);

    keepProp.setPropertyValue("0");
    keepProp.addTo((MutablePropertiesContainer)parent);
  }


  @Override
  public void setAttribute(final String key, final Object o) {
    if (DEPRECATED_NAME.equals(key)) { // Backward compatibility.  Before v1.3, name and button text were combined into one attribute
      setAttribute(NAME, o);
      setAttribute(BUTTON_TEXT, o);
    }
    else if (NAME.equals(key)) {
      setConfigureName((String) o);
      property.setPropertyName(getConfigureName() + "_result"); //$NON-NLS-1$
      totalProp.setPropertyName(getConfigureName() + "_total"); //$NON-NLS-1$
      keepProp.setPropertyName(getConfigureName() + "_keep"); //$NON-NLS-1$
      getLaunchButton().setToolTipText((String) o);
    }
    else if (N_DICE.equals(key)) {
      if (o instanceof Integer) {
        nDice = (Integer) o;
      }
      else if (o instanceof String) {
        nDice = Integer.parseInt((String) o);
      }
    }
    else if (LOCK_DICE.equals(key)) {
      if (o instanceof Boolean) {
        lockDice = (Boolean) o;
      }
      else if (o instanceof String) {
        lockDice = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (N_SIDES.equals(key)) {
      if (o instanceof Integer) {
        nSides = (Integer) o;
      }
      else if (o instanceof String) {
        nSides = Integer.parseInt((String) o);
      }
    }
    else if (LOCK_SIDES.equals(key)) {
      if (o instanceof Boolean) {
        lockSides = (Boolean) o;
      }
      else if (o instanceof String) {
        lockSides = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (PLUS.equals(key)) {
      if (o instanceof Integer) {
        plus = (Integer) o;
      }
      else if (o instanceof String) {
        plus = Integer.parseInt((String) o);
      }
    }
    else if (LOCK_PLUS.equals(key)) {
      if (o instanceof Boolean) {
        lockPlus = (Boolean) o;
      }
      else if (o instanceof String) {
        lockPlus = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (ADD_TO_TOTAL.equals(key)) {
      if (o instanceof Integer) {
        addToTotal = (Integer) o;
      }
      else if (o instanceof String) {
        addToTotal = Integer.parseInt((String) o);
      }
    }
    else if (LOCK_ADD.equals(key)) {
      if (o instanceof Boolean) {
        lockAdd = (Boolean) o;
      }
      else if (o instanceof String) {
        lockAdd = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (REPORT_TOTAL.equals(key)) {
      if (o instanceof Boolean) {
        reportTotal = (Boolean) o;
      }
      else if (o instanceof String) {
        reportTotal = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (PROMPT_ALWAYS.equals(key)) {
      if (o instanceof Boolean) {
        promptAlways = (Boolean) o;
      }
      else if (o instanceof String) {
        promptAlways = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) o);
    }
    else if (TOOLTIP.equals(key)) {
      tooltip = (String) o;
      super.setAttribute(key, o);
    }
    else if (SORT_DICE_RESULTS.equals(key)) {
      if (o instanceof Boolean) {
        sortDice = (Boolean) o;
      }
      else if (o instanceof String) {
        sortDice = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (KEEP_DICE.equals(key)) {
      if (o instanceof Boolean) {
        keepingDice = (Boolean) o;
      }
      else if (o instanceof String) {
        keepingDice = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (KEEP_OPTION.equals(key)) {
      keepOption = (String) o;
    }
    else if (KEEP_COUNT.equals(key)) {
      if (o instanceof Integer) {
        keepValue = (Integer) o;
      }
      else if (o instanceof String) {
        keepValue = Integer.parseInt((String) o);
      }
    }
    else {
      super.setAttribute(key, o);
    }
  }

  @Override
  public String getAttributeValueString(final String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (N_DICE.equals(key)) {
      return String.valueOf(nDice);
    }
    else if (LOCK_DICE.equals(key)) {
      return String.valueOf(lockDice);
    }
    else if (N_SIDES.equals(key)) {
      return String.valueOf(nSides);
    }
    else if (LOCK_SIDES.equals(key)) {
      return String.valueOf(lockSides);
    }
    else if (PLUS.equals(key)) {
      return String.valueOf(plus);
    }
    else if (LOCK_PLUS.equals(key)) {
      return String.valueOf(lockPlus);
    }
    else if (ADD_TO_TOTAL.equals(key)) {
      return String.valueOf(addToTotal);
    }
    else if (LOCK_ADD.equals(key)) {
      return String.valueOf(lockAdd);
    }
    else if (REPORT_TOTAL.equals(key)) {
      return String.valueOf(reportTotal);
    }
    else if (PROMPT_ALWAYS.equals(key)) {
      return String.valueOf(promptAlways);
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (TOOLTIP.equals(key)) {
      return tooltip.length() == 0 ? super.getAttributeValueString(BUTTON_TEXT) : tooltip;
    }
    else if (SORT_DICE_RESULTS.equals(key)) {
      return String.valueOf(sortDice);
    }
    else if (KEEP_DICE.equals(key)) {
      return String.valueOf(keepingDice);
    }
    else if (KEEP_OPTION.equals(key)) {
      return keepOption;
    }
    else if (KEEP_COUNT.equals(key)) {
      return String.valueOf(keepValue);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("DiceButton.html"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * Implement PropertyNameSource - Expose roll result property
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(getConfigureName() + "_result"); //NON-NLS
    return l;
  }

  /**
   * In case reports use HTML and  refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(final Collection<String> s) {
    final HTMLImageFinder h = new HTMLImageFinder(reportFormat.getFormat());
    h.addImageNames(s);
    super.addLocalImageNames(s);
  }
}

