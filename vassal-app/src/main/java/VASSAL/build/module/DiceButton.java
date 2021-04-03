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

import VASSAL.build.AbstractToolbarItem;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.swing.SwingUtils;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

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
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;

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

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected String tooltip = ""; //$NON-NLS-1$
  protected final MutableProperty.Impl property = new Impl("", this);

  // These five identical to AbstractToolbarItem, and are only here for "clirr purposes"
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String NAME = "name"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String HOTKEY = "hotkey"; //$NON-NLS-1$

  public static final String DEPRECATED_NAME = "label"; //$NON-NLS-1$
  public static final String N_DICE = "nDice"; //$NON-NLS-1$
  public static final String N_SIDES = "nSides"; //$NON-NLS-1$
  public static final String PLUS = "plus"; //$NON-NLS-1$
  public static final String ADD_TO_TOTAL = "addToTotal"; //$NON-NLS-1$
  public static final String REPORT_TOTAL = "reportTotal"; //$NON-NLS-1$
  public static final String PROMPT_ALWAYS = "prompt"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$
  public static final String SORT_DICE_RESULTS = "sortDice"; //NON-NLS

  /** Variable name for reporting format */
  public static final String RESULT = "result"; //$NON-NLS-1$
  public static final String REPORT_NAME = "name"; //$NON-NLS-1$

  public DiceButton() {
    initLaunchButton();
  }

  protected void initLaunchButton() {
    final ActionListener rollAction = e -> {
      if (promptAlways) {
        final DiceButton delegate = new DiceButton() {
          @Override
          protected void initLaunchButton() {
            setLaunchButton(makeLaunchButton("", AbstractToolbarItem.BUTTON_TEXT, "", null));
          }
        };

        final List<String> keepAttributes =
          Arrays.asList(N_DICE, N_SIDES, PLUS, ADD_TO_TOTAL);

        for (final String key : keepAttributes) {
          delegate.setAttribute(key, getAttributeValueString(key));
        }

        final AutoConfigurer ac = new AutoConfigurer(delegate);
        final ConfigurerWindow w = new ConfigurerWindow(ac, true);
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
   * The text reported before the results of the roll
   * @deprecated No Replacement
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected String getReportPrefix() {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    return " *** " + getConfigureName() + " = "; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * The text reported after the results of the roll;
   * @deprecated No Replacement
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected String getReportSuffix() {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    return " ***  <" //$NON-NLS-1$
        + GlobalOptions.getInstance().getPlayerId() + ">"; //$NON-NLS-1$
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format is
   * prefix+[comma-separated roll list]+suffix */
  protected void DR() {
    final StringBuilder val = new StringBuilder();
    int total = addToTotal;
    int[] dice = null; // stays null if no sorting

    if (!reportTotal && nDice > 1 && sortDice) {
      dice = new int[nDice];
    }

    for (int i = 0; i < nDice; ++i) {
      final int roll = ran.nextInt(nSides) + 1 + plus;
      if (dice != null) {
        dice[i] = roll;
      }
      else if (reportTotal) {
        total += roll;
      }
      else { // do not sort
        val.append(roll);
        if (i < nDice - 1)
          val.append(',');
      }
    }

    if (reportTotal) {
      val.append(total);
    }
    else if (dice != null) {
      Arrays.sort(dice);
      for (int i = 0; i < nDice; ++i) {
        val.append(dice[i]);
        if (i < nDice - 1) {
          val.append(',');
        }
      }
    }

    final String report = formatResult(val.toString());
    final Command c = report.length() == 0 ? new NullCommand() : new Chatter.DisplayText(GameModule.getGameModule().getChatter(), report);
    c.execute();
    c.append(property.setPropertyValue(val.toString()));
    GameModule.getGameModule().sendAndLog(c);
  }

  /**
   * Use the configured FormattedString to format the result of a roll
   * @param result Result format
   * @return Formatted result
   */
  protected String formatResult(final String result) {
    reportFormat.setProperty(REPORT_NAME, getLocalizedConfigureName());
    reportFormat.setProperty(RESULT, result);
    reportFormat.setProperty(N_DICE, Integer.toString(nDice));
    reportFormat.setProperty(N_SIDES, Integer.toString(nSides));
    reportFormat.setProperty(PLUS, Integer.toString(plus));
    reportFormat.setProperty(ADD_TO_TOTAL, Integer.toString(addToTotal));
    final String text = reportFormat.getLocalizedText();
    String report = text;
    if (text.length() > 0) {
      report = text.startsWith("*") ? "*" + text : "* " + text; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
    return report;
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      NAME,
      BUTTON_TEXT,
      TOOLTIP,
      ICON,
      N_DICE,
      N_SIDES,
      PLUS,
      ADD_TO_TOTAL,
      REPORT_TOTAL,
      HOTKEY,
      PROMPT_ALWAYS,
      REPORT_FORMAT,
      SORT_DICE_RESULTS,
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString("Dice.number_of_dice"), //$NON-NLS-1$
        Resources.getString("Dice.number_of_sides"), //$NON-NLS-1$
        Resources.getString("Dice.add_to_each_side"), //$NON-NLS-1$
        Resources.getString("Dice.add_to_total"), //$NON-NLS-1$
        Resources.getString("Editor.DiceButton.report_total"), //$NON-NLS-1$
        Resources.getString(Resources.HOTKEY_LABEL),
        Resources.getString("Editor.DiceButton.prompt_value"), //$NON-NLS-1$
        Resources.getString("Editor.report_format"), //$NON-NLS-1$
        Resources.getString("Editor.DiceButton.sort_results") //$NON-NLS-1$
    };
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
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{REPORT_NAME, RESULT, N_DICE, N_SIDES, PLUS, ADD_TO_TOTAL});
    }
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class,
      IconConfig.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Boolean.class,
      NamedKeyStroke.class,
      Boolean.class,
      ReportFormatConfig.class,
      Boolean.class,
    };
  }

  private final VisibilityCondition cond = () -> !promptAlways;

  private final VisibilityCondition canSort = () -> !reportTotal;

  @Override
  public VisibilityCondition getAttributeVisibility(final String name) {
    if (List.of(N_DICE, N_SIDES, PLUS, ADD_TO_TOTAL).contains(name)) {
      return cond;
    }
    else if (SORT_DICE_RESULTS.equals(name)) {
      return canSort;
    }
    else {
      return null;
    }
  }

  /**
   * Expects to be added to a GameModule.  Adds the button to the
   * control window's toolbar and registers itself as a @link
   * KeyStrokeListener */
  @Override
  public void addTo(final Buildable parent) {
    super.addTo(parent);
    ran = GameModule.getGameModule().getRNG();
    property.setPropertyValue("1"); // Initialize with a numeric value //$NON-NLS-1$
    property.addTo((MutablePropertiesContainer)parent);
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
    else if (N_SIDES.equals(key)) {
      if (o instanceof Integer) {
        nSides = (Integer) o;
      }
      else if (o instanceof String) {
        nSides = Integer.parseInt((String) o);
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
    else if (ADD_TO_TOTAL.equals(key)) {
      if (o instanceof Integer) {
        addToTotal = (Integer) o;
      }
      else if (o instanceof String) {
        addToTotal = Integer.parseInt((String) o);
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
    else if (N_SIDES.equals(key)) {
      return String.valueOf(nSides);
    }
    else if (PLUS.equals(key)) {
      return String.valueOf(plus);
    }
    else if (ADD_TO_TOTAL.equals(key)) {
      return String.valueOf(addToTotal);
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
    return HelpFile.getReferenceManualPage("GameModule.html", "DiceButton"); //$NON-NLS-1$ //$NON-NLS-2$
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
