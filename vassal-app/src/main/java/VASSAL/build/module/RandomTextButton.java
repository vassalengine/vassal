/*
 *
 * Copyright (c) 2004 by Michael Blumoehr
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

import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.ConfigurerWindow;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.ErrorDialog;

/**
 * @author Michael Blumoehr
 *
 * This component places a button into the controls window toolbar.
 * Pressing the button generates random numbers or strings and displays the
 * result in the Chatter
 */
// TODO Expose result as a property
public class RandomTextButton extends DiceButton {
  protected String[] m_faces;               // array with dice faces
  protected boolean isNumeric;

  public static final String FACES = "faces"; //$NON-NLS-1$
  public static final String NUMERIC = "numeric"; //$NON-NLS-1$

  @SuppressWarnings("removal")
  public RandomTextButton() {
    super();
    final ActionListener ranAction = e -> {
      if (promptAlways) {
        promptAlways = false; // Show the usu
        // Remove label, hotkey, and prompt controls
        final AutoConfigurer ac = (AutoConfigurer) getConfigurer();
        final ConfigurerWindow w = new ConfigurerWindow(ac, true);
        ac.getConfigurer(NAME).getControls().setVisible(false);
        ac.getConfigurer(BUTTON_TEXT).getControls().setVisible(false);
        ac.getConfigurer(TOOLTIP).getControls().setVisible(false);
        ac.getConfigurer(ICON).getControls().setVisible(false);
        ac.getConfigurer(HOTKEY).getControls().setVisible(false);
        ac.getConfigurer(PROMPT_ALWAYS).getControls().setVisible(false);
        ac.getConfigurer(REPORT_FORMAT).getControls().setVisible(false);
        ac.getConfigurer(REPORT_TOTAL).getControls().setVisible(false);
        ac.getConfigurer(FACES).getControls().setVisible(false);
        ac.getConfigurer(NUMERIC).getControls().setVisible(false);
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
        ac.getConfigurer(FACES).getControls().setVisible(true);
        ac.getConfigurer(NUMERIC).getControls().setVisible(true);
        if (! w.isCancelled()) {
          DR();
        }
        promptAlways = true;
      }
      else {
        DR();
      }
    };
    makeLaunchButton("", "", "", ranAction);
    setAttributeTranslatable(FACES, true);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.RandomTextButton.component_type"); //$NON-NLS-1$
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format is
   * prefix+[comma-separated roll list]+suffix */
  @Override
  protected void DR() {
    final StringBuilder result = new StringBuilder();
    int total = addToTotal;
    for (int i = 0; i < nDice; ++i) {
      int roll = ran.nextInt(nSides) + 1;

      // take the face value from user defined faces
      if (isNumeric) {
        try {
          if (roll <= m_faces.length) {
            roll = Integer.parseInt(m_faces[roll - 1]) + plus;
          }
          else {
            ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Dice.random_text_too_few_faces", name), String.valueOf(roll)));
            roll = plus;
          }
        }
        catch (NumberFormatException ex) {
          ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Dice.random_text_non_numeric", name), m_faces[roll - 1]));
          roll = 1;
        }
      }

      // no totals if text output
      if (reportTotal && isNumeric) {
        total += roll;
      }
      else {
        if (!isNumeric)
          if (roll <= m_faces.length) {
            result.append(m_faces[roll - 1]);
          }
          else {
            ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Dice.random_text_too_few_faces", name), String.valueOf(roll)));
            result.append('0');
          }
        else
          result.append(roll);
        if (i < nDice - 1)
          result.append(',');
      }
    }

    // totals only if no text output
    if (reportTotal && isNumeric)
      result.append(total);

    final String report = formatResult(result.toString());
    final Command c = report.length() == 0 ? new NullCommand() : new Chatter.DisplayText(GameModule.getGameModule().getChatter(), report);
    c.execute();
    c.append(property.setPropertyValue(result.toString()));
    GameModule.getGameModule().sendAndLog(c);
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(REPORT_TOTAL, PLUS, ADD_TO_TOTAL).contains(name)) {
      return () -> isNumeric;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }


  /**
   * The additional Attributes of a RandomTextButton are:
   *
   * <code>FACES</code> Text of the dice faces
   *                    must be integer if USE_FACES=NUMERIC

   * <code>NUMERIC</code>   If true, then face text must be an integer,
   *  and reportTotal is enabled
   */
  @Override
  public String[] getAttributeNames() {
    final ArrayList<String> l =
      new ArrayList<>(Arrays.asList(super.getAttributeNames()));
    l.remove(N_SIDES);
    l.add(FACES);
    l.add(NUMERIC);
    return l.toArray(new String[0]);
  }

  @Override
  public String[] getAttributeDescriptions() {
    final ArrayList<String> l =
      new ArrayList<>(Arrays.asList(super.getAttributeDescriptions()));
    final ArrayList<String> names =
      new ArrayList<>(Arrays.asList(super.getAttributeNames()));
    l.remove(names.indexOf(N_SIDES));
    l.add(Resources.getString("Editor.RandomTextButton.faces")); //$NON-NLS-1$
    l.add(Resources.getString("Editor.RandomTextButton.faces_numeric")); //$NON-NLS-1$
    return l.toArray(new String[0]);
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    final ArrayList<Class<?>> l =
      new ArrayList<>(Arrays.asList(super.getAttributeTypes()));
    final ArrayList<String> names =
      new ArrayList<>(Arrays.asList(super.getAttributeNames()));
    l.remove(names.indexOf(N_SIDES));
    l.add(String[].class);
    l.add(Boolean.class);
    return l.toArray(new Class<?>[0]);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NUMERIC.equals(key)) {
      isNumeric = Boolean.TRUE.equals(value) || "true".equals(value); //$NON-NLS-1$
    }
    else if (FACES.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      m_faces = (String[]) value;
      nSides = m_faces.length;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NUMERIC.equals(key)) {
      return String.valueOf(isNumeric);
    }
    else if (FACES.equals(key)) {
      return StringArrayConfigurer.arrayToString(m_faces);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.html", "RandomTextButton"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return new ArrayList<>(Arrays.asList(m_faces));
  }

  /**
   * In case reports use HTML and  refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    HTMLImageFinder h;
    for (final String f : m_faces) {
      h = new HTMLImageFinder(f);
      h.addImageNames(s);
    }
  }
}
