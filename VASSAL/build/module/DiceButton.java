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
package VASSAL.build.module;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.KeyStroke;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.MutableProperty.Impl;
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
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.LaunchButton;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button generates random numbers and displays the
 * result in the Chatter */
public class DiceButton extends AbstractConfigurable {
  protected java.util.Random ran;
  protected int nSides = 6, nDice = 2, plus = 0;
  protected boolean reportTotal = false;
  protected boolean promptAlways = false;
  protected FormattedString reportFormat = new FormattedString("** $" + REPORT_NAME + "$ = $" + RESULT + "$ *** <$" + GlobalOptions.PLAYER_NAME + "$>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  protected LaunchButton launch;
  protected String tooltip = ""; //$NON-NLS-1$
  protected MutableProperty.Impl property = new Impl("",this);

  public static final String DEPRECATED_NAME = "label"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String N_DICE = "nDice"; //$NON-NLS-1$
  public static final String N_SIDES = "nSides"; //$NON-NLS-1$
  public static final String PLUS = "plus"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String REPORT_TOTAL = "reportTotal"; //$NON-NLS-1$
  public static final String PROMPT_ALWAYS = "prompt"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$

  /** Variable name for reporting format */
  public static final String RESULT = "result"; //$NON-NLS-1$
  public static final String REPORT_NAME = "name"; //$NON-NLS-1$

  public DiceButton() {
    ActionListener rollAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (promptAlways) {
          promptAlways = false; // Show the usu
          // Remove label, hotkey, and prompt controls
          AutoConfigurer ac = (AutoConfigurer) getConfigurer();
          ConfigurerWindow w = new ConfigurerWindow(ac, true);
          ac.getConfigurer(NAME).getControls().setVisible(false);
          ac.getConfigurer(BUTTON_TEXT).getControls().setVisible(false);
          ac.getConfigurer(TOOLTIP).getControls().setVisible(false);
          ac.getConfigurer(ICON).getControls().setVisible(false);
          ac.getConfigurer(HOTKEY).getControls().setVisible(false);
          ac.getConfigurer(PROMPT_ALWAYS).getControls().setVisible(false);
          ac.getConfigurer(REPORT_FORMAT).getControls().setVisible(false);
          ac.getConfigurer(REPORT_TOTAL).getControls().setVisible(false);
          w.pack();
          w.setVisible(true);
          ac.getConfigurer(NAME).getControls().setVisible(true);
          ac.getConfigurer(BUTTON_TEXT).getControls().setVisible(true);
          ac.getConfigurer(TOOLTIP).getControls().setVisible(true);
          ac.getConfigurer(ICON).getControls().setVisible(true);
          ac.getConfigurer(HOTKEY).getControls().setVisible(true);
          ac.getConfigurer(PROMPT_ALWAYS).getControls().setVisible(true);
          ac.getConfigurer(REPORT_FORMAT).getControls().setVisible(true);
          ac.getConfigurer(REPORT_TOTAL).getControls().setVisible(true);
          DR();
          promptAlways = true;
        }
        else {
          DR();
        }
      }
    };
    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, rollAction);
    setAttribute(NAME, "2d6");
    setAttribute(BUTTON_TEXT, "2d6");
    launch.setAttribute(TOOLTIP, "2d6");
  }

  public static String getConfigureTypeName() {
    return "Dice Button";
  }

  /**
   * The text reported before the results of the roll
   * @deprecated
   */
  protected String getReportPrefix() {
    return " *** " + getConfigureName() + " = "; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * The text reported after the results of the roll;
   * @deprecated
   */
  protected String getReportSuffix() {
    return " ***  <" //$NON-NLS-1$
        + GameModule.getGameModule().getChatter().getHandle() + ">"; //$NON-NLS-1$
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format is
   * prefix+[comma-separated roll list]+suffix */
  protected void DR() {
    String val = ""; //$NON-NLS-1$
    int total = 0;
    for (int i = 0; i < nDice; ++i) {
      int roll = (int) (ran.nextFloat() * nSides + 1) + plus;
      if (reportTotal) {
        total += roll;
      }
      else {
        val += roll;
        if (i < nDice - 1)
          val += ","; //$NON-NLS-1$
      }
    }

    if (reportTotal)
      val += total;

    String report = formatResult(val);
    GameModule.getGameModule().getChatter().send(report);
    property.setPropertyValue(val);
  }

  /**
   * Use the configured FormattedString to format the result of a roll
   * @param result
   * @return
   */
  protected String formatResult(String result) {
    reportFormat.setProperty(REPORT_NAME, getLocalizedConfigureName());
    reportFormat.setProperty(RESULT, result);
    String text = reportFormat.getLocalizedText();
    String report = text.startsWith("*") ? "*" + text : "* " + text; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    return report;
  }

  public String[] getAttributeNames() {
    String s[] = {NAME, BUTTON_TEXT, TOOLTIP, ICON, N_DICE, N_SIDES, PLUS, REPORT_TOTAL, HOTKEY, PROMPT_ALWAYS, REPORT_FORMAT};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  ",
                        "Button text:  ",
                        "Tooltip text:  ",
                        "Button icon:  ",
                        Resources.getString("Dice.number_of_dice"), //$NON-NLS-1$
                        Resources.getString("Dice.number_of_sides"), //$NON-NLS-1$
                        Resources.getString("Dice.add_to_each_side"), //$NON-NLS-1$
                        "Report Total?",
                        "Hotkey:  ",
                        "Prompt for values when button pushed?",
                        "Report Format:  "};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/die.gif"); //$NON-NLS-1$
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{REPORT_NAME, RESULT});
    }
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       String.class,
                       String.class,
                       IconConfig.class,
                       Integer.class,
                       Integer.class,
                       Integer.class,
                       Boolean.class,
                       KeyStroke.class,
                       Boolean.class,
                       ReportFormatConfig.class};
  }

  private VisibilityCondition cond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !promptAlways;
    }
  };

  public VisibilityCondition getAttributeVisibility(String name) {
    if (N_DICE.equals(name)
        || N_SIDES.equals(name)
        || PLUS.equals(name)
        || REPORT_TOTAL.equals(name)) {
      return cond;
    }
    else {
      return null;
    }
  }

  /**
   * Expects to be added to a GameModule.  Adds the button to the
   * control window's toolbar and registers itself as a {@link
   * KeyStrokeListener} */
  public void addTo(Buildable parent) {
    ran = GameModule.getGameModule().getRNG();
    GameModule.getGameModule().getToolBar().add(getComponent());
    property.addTo((MutablePropertiesContainer)parent);
  }

  /**
   * The component to be added to the control window toolbar
   */
  protected java.awt.Component getComponent() {
    return launch;
  }

  public void setAttribute(String key, Object o) {
    if (DEPRECATED_NAME.equals(key)) { // Backward compatibility.  Before v1.3, name and button text were combined into one attribute
      setAttribute(NAME, o);
      setAttribute(BUTTON_TEXT, o);
    }
    else if (NAME.equals(key)) {
      setConfigureName((String) o);
      property.setPropertyName(getConfigureName()+"_result");
      launch.setToolTipText((String) o);
    }
    else if (N_DICE.equals(key)) {
      if (o instanceof Integer) {
        nDice = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        nDice = Integer.parseInt((String) o);
      }
    }
    else if (N_SIDES.equals(key)) {
      if (o instanceof Integer) {
        nSides = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        nSides = Integer.parseInt((String) o);
      }
    }
    else if (PLUS.equals(key)) {
      if (o instanceof Integer) {
        plus = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        plus = Integer.parseInt((String) o);
      }
    }
    else if (REPORT_TOTAL.equals(key)) {
      if (o instanceof Boolean) {
        reportTotal = ((Boolean) o).booleanValue();
      }
      else if (o instanceof String) {
        reportTotal = "true".equals(o); //$NON-NLS-1$
      }
    }
    else if (PROMPT_ALWAYS.equals(key)) {
      if (o instanceof Boolean) {
        promptAlways = ((Boolean) o).booleanValue();
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
      launch.setAttribute(key, o);
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (N_DICE.equals(key)) {
      return "" + nDice; //$NON-NLS-1$
    }
    else if (N_SIDES.equals(key)) {
      return "" + nSides; //$NON-NLS-1$
    }
    else if (PLUS.equals(key)) {
      return "" + plus; //$NON-NLS-1$
    }
    else if (REPORT_TOTAL.equals(key)) {
      return "" + reportTotal; //$NON-NLS-1$
    }
    else if (PROMPT_ALWAYS.equals(key)) {
      return "" + promptAlways; //$NON-NLS-1$
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (TOOLTIP.equals(name)) {
      return tooltip.length() == 0 ? launch.getAttributeValueString(name) : tooltip;
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getToolBar().revalidate();
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm","DiceButton"); //$NON-NLS-1$ //$NON-NLS-2$
  }
}
