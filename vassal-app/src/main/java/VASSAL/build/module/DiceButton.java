package VASSAL.build.module;

import VASSAL.build.AbstractFolder;
import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import VASSAL.build.module.properties.MutableProperty.Impl;

public class DiceButton extends AbstractToolbarItem {
  protected java.util.Random ran;
  protected int nSides = 6, nDice = 2, plus = 0, addToTotal = 0;
  protected boolean reportTotal = false;
  protected boolean promptAlways = false;
  protected boolean sortDice = false;
  protected final FormattedString reportFormat = new FormattedString("** $" + REPORT_NAME + "$ = $" + RESULT + "$ *** &lt;$" + GlobalOptions.PLAYER_NAME + "$&gt;");

  protected boolean lockSides = false;
  protected boolean lockDice = false;
  protected boolean lockPlus = false;
  protected boolean lockAdd = false;

  protected int[] keepDice;
  protected int numericTotal;
  protected boolean keepingDice = false;
  protected String keepOption = KEEP_GREATER;
  protected int keepValue = 1;
  protected int keepCount;
  protected int[] rawRolls;
  protected int[] rawCounts;
  protected int[] counts;

  protected static final String KEEP_SMALLEST = "s";
  protected static final String KEEP_LARGEST = "l";
  protected static final String KEEP_EQUAL = "=";
  protected static final String KEEP_GREATER = ">";
  protected static final String KEEP_LESS = "<";

  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected String tooltip = "";
  protected final MutableProperty.Impl property = new Impl("", this);
  protected final MutableProperty.Impl totalProp = new Impl("", this);
  protected final MutableProperty.Impl keepProp = new Impl("", this);
  protected final MutableProperty.Impl summaryProp = new Impl("", this);

  @Deprecated(since = "2020-10-21", forRemoval = true)
  public static final String BUTTON_TEXT = "text";
  @Deprecated(since = "2020-10-21", forRemoval = true)
  public static final String TOOLTIP = "tooltip";
  @Deprecated(since = "2020-10-21", forRemoval = true)
  public static final String NAME = "name";
  @Deprecated(since = "2020-10-21", forRemoval = true)
  public static final String ICON = "icon";
  @Deprecated(since = "2020-10-21", forRemoval = true)
  public static final String HOTKEY = "hotkey";

  public static final String DEPRECATED_NAME = "label";
  public static final String N_DICE = "nDice";
  public static final String N_SIDES = "nSides";
  public static final String PLUS = "plus";
  public static final String ADD_TO_TOTAL = "addToTotal";
  public static final String REPORT_TOTAL = "reportTotal";
  public static final String PROMPT_ALWAYS = "prompt";
  public static final String REPORT_FORMAT = "reportFormat";
  public static final String SORT_DICE_RESULTS = "sortDice";
  public static final String KEEP_DICE = "keepDice";
  public static final String KEEP_OPTION = "keepOption";
  public static final String KEEP_COUNT = "keepCount";
  public static final String LOCK_DICE = "lockDice";
  public static final String LOCK_SIDES = "lockSides";
  public static final String LOCK_PLUS = "lockPlus";
  public static final String LOCK_ADD = "lockAdd";
  public static final String SUMMARY = "summary";
  public static final String RESULT = "result";
  public static final String REPORT_NAME = "name";
  public static final String RESULT_N = "result#";
  public static final String NUMERIC_TOTAL = "numericalTotal";

  public DiceButton() {
    initLaunchButton();
  }

  protected void initLaunchButton() {
    final ActionListener rollAction = e -> {
      final List<String> keepAttributes = new ArrayList<>();
      if (promptAlways) {
        if (!lockDice) keepAttributes.add(N_DICE);
        if (!lockSides) keepAttributes.add(N_SIDES);
        if (!lockPlus) keepAttributes.add(PLUS);
        if (!lockAdd) keepAttributes.add(ADD_TO_TOTAL);
      }
      if (promptAlways && !keepAttributes.isEmpty()) {
        final DiceButton delegate = new DiceButton() {
          @Override
          protected void initLaunchButton() {
            setLaunchButton(makeLaunchButton("", AbstractToolbarItem.BUTTON_TEXT, "", null));
          }
        };
        for (final String key : keepAttributes) delegate.setAttribute(key, getAttributeValueString(key));
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
        for (final String key : keepAttributes) setAttribute(key, delegate.getAttributeValueString(key));
        if (!w.isCancelled()) DR();
      } else {
        DR();
      }
    };
    setLaunchButton(makeLaunchButton(Resources.getString("Editor.DiceButton.dice_button_text"), Resources.getString("Editor.DiceButton.dice_button_tooltip"), "/images/die.gif", rollAction));
    launch = getLaunchButton();
    setAttribute(AbstractToolbarItem.NAME, Resources.getString("Editor.DiceButton.dice_name"));
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DiceButton.component_type");
  }

  protected void DR() {
    keepDice = new int[nDice];
    numericTotal = addToTotal;
    keepCount = 0;
    rawRolls = new int[nDice];
    rawCounts = new int[nSides];
    counts = new int[nSides];

    for (int i = 0; i < nDice; ++i) {
      final int rawRoll = ran.nextInt(nSides) + 1;
      final int roll = rawRoll + plus;
      rawRolls[i] = rawRoll;
      rawCounts[rawRoll - 1] += 1;
      if (keepingDice) {
        if (KEEP_GREATER.equals(keepOption) && roll < keepValue) continue;
        else if (KEEP_LESS.equals(keepOption) && roll > keepValue) continue;
        else if (KEEP_EQUAL.equals(keepOption) && roll != keepValue) continue;
      }
      keepCount += 1;
      keepDice[keepCount - 1] = roll;
      numericTotal += roll;
      counts[rawRoll - 1] += 1;
    }

    if (keepCount < keepDice.length) keepDice = Arrays.copyOf(keepDice, keepCount);

    if (List.of(KEEP_SMALLEST, KEEP_LARGEST).contains(keepOption)) {
      final int[] tempDice = Arrays.copyOf(keepDice, keepDice.length);
      Arrays.sort(tempDice);
      keepCount = Math.max(0, Math.min(keepValue, tempDice.length));
      keepDice = new int[keepCount];
      if (KEEP_SMALLEST.equals(keepOption)) System.arraycopy(tempDice, 0, keepDice, 0, keepCount);
      else System.arraycopy(tempDice, tempDice.length - keepCount, keepDice, 0, keepCount);
      numericTotal = addToTotal;
      for (int i = 0; i < keepCount; i++) numericTotal += keepDice[i];
    }

    if (sortDice) Arrays.sort(keepDice);

    final StringBuilder val = new StringBuilder();
    if (reportTotal) val.append(numericTotal);
    else for (int i = 0; i < keepCount; ++i) {
      val.append(keepDice[i]);
      if (i < keepCount - 1) val.append(',');
    }

    final Map<Integer, Integer> resultCounts = new HashMap<>();
    for (int result : keepDice) resultCounts.merge(result, 1, Integer::sum);
    final StringBuilder summaryVal = new StringBuilder();
    boolean first = true;
    for (Map.Entry<Integer, Integer> entry : resultCounts.entrySet()) {
      if (!first) summaryVal.append(Resources.getString("Dice.summary_separator"));
      summaryVal.append(entry.getKey()).append(Resources.getString("Dice.summary_times")).append(entry.getValue());
      first = false;
    }

    final String report = formatResult(val.toString(), summaryVal.toString());
    Command c = report.isEmpty() ? new NullCommand() : new Chatter.DisplayText(GameModule.getGameModule().getChatter(), report);
    c.execute();
    c = c.append(property.setPropertyValue(val.toString())).append(totalProp.setPropertyValue(Integer.toString(numericTotal))).append(keepProp.setPropertyValue(Integer.toString(keepCount))).append(summaryProp.setPropertyValue(summaryVal.toString()));
    GameModule.getGameModule().sendAndLog(c);
  }

  protected String formatResult(final String result, final String summary) {
    reportFormat.clearProperties();
    reportFormat.setProperty(REPORT_NAME, getLocalizedConfigureName());
    reportFormat.setProperty(RESULT, result);
    reportFormat.setProperty(SUMMARY, summary);
    reportFormat.setProperty(N_DICE, Integer.toString(nDice));
    reportFormat.setProperty(N_SIDES, Integer.toString(nSides));
    reportFormat.setProperty(PLUS, Integer.toString(plus));
    reportFormat.setProperty(ADD_TO_TOTAL, Integer.toString(addToTotal));
    for (int i = 0; i < keepCount; i++) reportFormat.setProperty("result" + (i + 1), Integer.toString(keepDice[i]));
    reportFormat.setProperty(NUMERIC_TOTAL, Integer.toString(numericTotal));
    reportFormat.setProperty(KEEP_DICE, Integer.toString(keepValue));
    reportFormat.setProperty(KEEP_COUNT, Integer.toString(keepCount));
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < nDice; ++i) {
      if (i > 0) sb.append(", ");
      sb.append(rawRolls[i]);
    }
    reportFormat.setProperty("rawRolls", sb.toString());
    for (int i = 0; i < nSides; ++i) {
      reportFormat.setProperty("rawCount" + (i + 1), Integer.toString(rawCounts[i]));
      reportFormat.setProperty("count" + (i + 1), Integer.toString(counts[i]));
    }
    final String text = reportFormat.getLocalizedText(this, "Editor.report_format");
    String report = text;
    if (!text.isEmpty()) report = text.startsWith("*") ? "*" + text : "* " + text;
    return report;
  }

  @Deprecated
  protected String formatResult(String result) {
    return formatResult(result, "");
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(super.getAttributeNames(), REPORT_FORMAT, PROMPT_ALWAYS, N_DICE, LOCK_DICE, N_SIDES, LOCK_SIDES, PLUS, LOCK_PLUS, ADD_TO_TOTAL, LOCK_ADD, REPORT_TOTAL, SORT_DICE_RESULTS, KEEP_DICE, KEEP_OPTION, KEEP_COUNT);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(super.getAttributeDescriptions(), Resources.getString("Editor.report_format"), Resources.getString("Editor.DiceButton.prompt_value"), Resources.getString("Dice.number_of_dice"), Resources.getString("Editor.DiceButton.lock_number_of_dice"), Resources.getString("Dice.number_of_sides"), Resources.getString("Editor.DiceButton.lock_number_of_sides"), Resources.getString("Dice.add_to_each_side"), Resources.getString("Editor.DiceButton.lock_add_to_each_side"), Resources.getString("Dice.add_to_total"), Resources.getString("Editor.DiceButton.lock_add_to_total"), Resources.getString("Editor.DiceButton.report_total"), Resources.getString("Editor.DiceButton.sort_results"), Resources.getString("Editor.DiceButton.keep_dice"), Resources.getString("Editor.DiceButton.keep_option"), Resources.getString("Editor.DiceButton.keep_count"));
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(final AutoConfigurable c, final String key, final String name) {
      return new IconConfigurer(key, name, "/images/die.gif");
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(final AutoConfigurable c, final String key, final String name) {
      return new PlayerIdFormattedExpressionConfigurer(key, name, new String[]{REPORT_NAME, RESULT, NUMERIC_TOTAL, RESULT_N, N_DICE, N_SIDES, PLUS, ADD_TO_TOTAL, KEEP_DICE, KEEP_COUNT, SUMMARY});
    }
  }

  public static class KeepConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new TranslatingStringEnumConfigurer(key, name, new String[]{KEEP_GREATER, KEEP_EQUAL, KEEP_LESS, KEEP_LARGEST, KEEP_SMALLEST}, new String[]{"Editor.DiceButton.keep_greater_than", "Editor.DiceButton.keep_equal", "Editor.DiceButton.keep_less_than", "Editor.DiceButton.keep_largest", "Editor.DiceButton.keep_smallest"});
    }
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(super.getAttributeTypes(), ReportFormatConfig.class, Boolean.class, Integer.class, Boolean.class, Integer.class, Boolean.class, Integer.class, Boolean.class, Integer.class, Boolean.class, Boolean.class, Boolean.class, Boolean.class, KeepConfig.class, Integer.class);
  }

  private final VisibilityCondition cond = () -> promptAlways;
  private final VisibilityCondition keep = () -> keepingDice;

  @Override
  public VisibilityCondition getAttributeVisibility(final String name) {
    if (List.of(LOCK_DICE, LOCK_SIDES, LOCK_PLUS, LOCK_ADD).contains(name)) return cond;
    else if (List.of(KEEP_OPTION, KEEP_COUNT).contains(name)) return keep;
    else return super.getAttributeVisibility(name);
  }

  @Override
  public void addTo(Buildable parent) {
    super.addTo(parent);
    if (parent instanceof AbstractFolder) parent = ((AbstractFolder) parent).getNonFolderAncestor();
    ran = GameModule.getGameModule().getRNG();
    property.setPropertyValue("1");
    property.addTo((MutablePropertiesContainer) parent);
    totalProp.setPropertyValue("0");
    totalProp.addTo((MutablePropertiesContainer) parent);
    keepProp.setPropertyValue("0");
    keepProp.addTo((MutablePropertiesContainer) parent);
    summaryProp.setPropertyValue("");
    summaryProp.addTo((MutablePropertiesContainer) parent);
  }

  @Override
  public void setAttribute(final String key, final Object o) {
    if (DEPRECATED_NAME.equals(key)) {
      setAttribute(NAME, o);
      setAttribute(BUTTON_TEXT, o);
    } else if (NAME.equals(key)) {
      setConfigureName((String) o);
      property.setPropertyName(getConfigureName() + "_result");
      totalProp.setPropertyName(getConfigureName() + "_total");
      keepProp.setPropertyName(getConfigureName() + "_keep");
      summaryProp.setPropertyName(getConfigureName() + "_summary");
      getLaunchButton().setToolTipText((String) o);
    } else if (N_DICE.equals(key)) {
      nDice = (o instanceof Integer) ? (Integer) o : Integer.parseInt((String) o);
      nDice = Math.max(nDice, 0);
    } else if (LOCK_DICE.equals(key)) {
      lockDice = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (N_SIDES.equals(key)) {
      nSides = (o instanceof Integer) ? (Integer) o : Integer.parseInt((String) o);
      nSides = Math.max(nSides, 0);
    } else if (LOCK_SIDES.equals(key)) {
      lockSides = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (PLUS.equals(key)) {
      plus = (o instanceof Integer) ? (Integer) o : Integer.parseInt((String) o);
    } else if (LOCK_PLUS.equals(key)) {
      lockPlus = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (ADD_TO_TOTAL.equals(key)) {
      addToTotal = (o instanceof Integer) ? (Integer) o : Integer.parseInt((String) o);
    } else if (LOCK_ADD.equals(key)) {
      lockAdd = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (REPORT_TOTAL.equals(key)) {
      reportTotal = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (PROMPT_ALWAYS.equals(key)) {
      promptAlways = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) o);
    } else if (TOOLTIP.equals(key)) {
      tooltip = (String) o;
      super.setAttribute(key, o);
    } else if (SORT_DICE_RESULTS.equals(key)) {
      sortDice = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (KEEP_DICE.equals(key)) {
      keepingDice = (o instanceof Boolean) ? (Boolean) o : "true".equals(o);
    } else if (KEEP_OPTION.equals(key)) {
      keepOption = (String) o;
    } else if (KEEP_COUNT.equals(key)) {
      keepValue = (o instanceof Integer) ? (Integer) o : Integer.parseInt((String) o);
    } else {
      super.setAttribute(key, o);
    }
  }

  @Override
  public String getAttributeValueString(final String key) {
    if (NAME.equals(key)) return getConfigureName();
    else if (N_DICE.equals(key)) return String.valueOf(nDice);
    else if (LOCK_DICE.equals(key)) return String.valueOf(lockDice);
    else if (N_SIDES.equals(key)) return String.valueOf(nSides);
    else if (LOCK_SIDES.equals(key)) return String.valueOf(lockSides);
    else if (PLUS.equals(key)) return String.valueOf(plus);
    else if (LOCK_PLUS.equals(key)) return String.valueOf(lockPlus);
    else if (ADD_TO_TOTAL.equals(key)) return String.valueOf(addToTotal);
    else if (LOCK_ADD.equals(key)) return String.valueOf(lockAdd);
    else if (REPORT_TOTAL.equals(key)) return String.valueOf(reportTotal);
    else if (PROMPT_ALWAYS.equals(key)) return String.valueOf(promptAlways);
    else if (REPORT_FORMAT.equals(key)) return reportFormat.getFormat();
    else if (TOOLTIP.equals(key)) return tooltip.isEmpty() ? super.getAttributeValueString(BUTTON_TEXT) : tooltip;
    else if (SORT_DICE_RESULTS.equals(key)) return String.valueOf(sortDice);
    else if (KEEP_DICE.equals(key)) return String.valueOf(keepingDice);
    else if (KEEP_OPTION.equals(key)) return keepOption;
    else if (KEEP_COUNT.equals(key)) return String.valueOf(keepValue);
    else return super.getAttributeValueString(key);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("DiceButton.html");
  }

  @Override
  public List<String> getPropertyNames() {
    final List<String> l = new ArrayList<>();
    l.add(property.getName());
    l.add(totalProp.getName());
    l.add(keepProp.getName());
    l.add(summaryProp.getName());
    return l;
  }

  @Override
  public void addLocalImageNames(final Collection<String> s) {
    final HTMLImageFinder h = new HTMLImageFinder(reportFormat.getFormat());
    h.addImageNames(s);
    super.addLocalImageNames(s);
  }
}