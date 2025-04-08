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

import VASSAL.tools.swing.SwingUtils;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.HashMap;
import java.util.Map;

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
 * This component places a button into the controls window toolbar.
 * Pressing the button generates random numbers or strings and displays the
 * result in the Chatter
 */
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
        final List<String> hideAttributes =
                Arrays.asList(NAME, BUTTON_TEXT, TOOLTIP, ICON, HOTKEY, PROMPT_ALWAYS, REPORT_FORMAT, REPORT_TOTAL, FACES, NUMERIC);

        for (final String attr : hideAttributes) {
          ac.getConfigurer(attr).getControls().setVisible(false);
          ac.getLabel(attr).setVisible(false);
        }

        SwingUtils.repack(w, true);
        w.setVisible(true);

        for (final String attr : hideAttributes) {
          ac.getConfigurer(attr).getControls().setVisible(true);
          ac.getLabel(attr).setVisible(true);
        }

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
    rawRolls = new int[nDice];
    rawCounts = new int[nSides];
    counts = new int[nSides];

    final StringBuilder result = new StringBuilder();
    final StringBuilder summaryResult = new StringBuilder();
    int total = addToTotal;

    final Map<String, Integer> resultCounts = new HashMap<>();
    final String[] results = new String[nDice]; // Array to store results

    // Generate all rolls
    for (int i = 0; i < nDice; ++i) {
      final int roll = ran.nextInt(nSides) + 1;
      rawRolls[i] = roll;
      rawCounts[roll - 1] += 1;
      counts[roll - 1] += 1;

      // Get face value
      String faceValue = (m_faces != null && roll <= m_faces.length)
              ? m_faces[roll - 1]
              : "0";

      // Apply plus modifier if numeric
      if (isNumeric) {
        try {
          faceValue = String.valueOf(Integer.parseInt(faceValue) + plus);
        }
        catch (NumberFormatException e) {
          ErrorDialog.dataWarning(new BadDataReport(
                  Resources.getString("Dice.random_text_non_numeric", name),
                  faceValue));
          faceValue = "0";
        }
      }

      results[i] = faceValue;
      resultCounts.merge(faceValue, 1, Integer::sum);

      if (reportTotal && isNumeric) {
        total += Integer.parseInt(faceValue);
      }
    }

    // Sort if requested (works for both numbers and text)
    if (sortDice) {
      Arrays.sort(results);
    }

    // Build result string
    if (!reportTotal || !isNumeric) {
      for (int i = 0; i < results.length; i++) {
        if (i > 0) result.append(',');
        result.append(results[i]);
      }
    }

// Build sorted summary
    final String[] summaryKeys = resultCounts.keySet().toArray(new String[0]);
    if (sortDice) {
      Arrays.sort(summaryKeys);
    }

    for (int i = 0; i < summaryKeys.length; i++) {
      if (i > 0) summaryResult.append(Resources.getString("Dice.summary_separator"));
      summaryResult.append(summaryKeys[i])
              .append(Resources.getString("Dice.summary_times"))
              .append(resultCounts.get(summaryKeys[i]));
    }

    if (reportTotal && isNumeric) {
      result.append(total);
    }

    // Send results
    final String report = formatResult(result.toString(), summaryResult.toString());
    final Command c = report.isEmpty()
            ? new NullCommand()
            : new Chatter.DisplayText(GameModule.getGameModule().getChatter(), report);
    c.execute();
    c.append(property.setPropertyValue(result.toString()))
            .append(totalProp.setPropertyValue(String.valueOf(total)))
            .append(summaryProp.setPropertyValue(summaryResult.toString()));
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

  @Override
  public String[] getAttributeNames() {
    final List<String> l =
            new ArrayList<>(Arrays.asList(super.getAttributeNames()));
    l.remove(N_SIDES);
    l.add(FACES);
    l.add(NUMERIC);
    return l.toArray(new String[0]);
  }

  @Override
  public String[] getAttributeDescriptions() {
    final List<String> l =
            new ArrayList<>(Arrays.asList(super.getAttributeDescriptions()));
    final List<String> names =
            new ArrayList<>(Arrays.asList(super.getAttributeNames()));
    l.remove(names.indexOf(N_SIDES));
    l.add(Resources.getString("Editor.RandomTextButton.faces")); //$NON-NLS-1$
    l.add(Resources.getString("Editor.RandomTextButton.faces_numeric")); //$NON-NLS-1$
    return l.toArray(new String[0]);
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    final List<Class<?>> l =
            new ArrayList<>(Arrays.asList(super.getAttributeTypes()));
    final List<String> names =
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