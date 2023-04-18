/*
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package VASSAL.build.module.turn;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.ArrayUtils;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;

public class ListTurnLevel extends TurnLevel implements ActionListener {

  protected static final String LIST = "list"; //$NON-NLS-1$
  protected static final String CONFIG_LIST = "configList"; //$NON-NLS-1$
  protected static final String CONFIG_FIRST = "configFirst"; //$NON-NLS-1$
  protected static final String PROMPT = "prompt"; //$NON-NLS-1$

  protected int first = 0;
  protected String[] list = new String[0];
  protected boolean[] active = new boolean[0];

  protected boolean configList = false;
  protected boolean configFirst = false;
  protected String prompt = null;

  protected JDialog configDialog;
  protected Component setControls;

  public ListTurnLevel() {
    super();
    turnFormat = new FormattedString("$" + LEVEL_VALUE + "$"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /*
   * Reset counter to initial state
   */
  @Override
  protected void reset() {
    super.reset();
    Arrays.fill(active, true);
    setLow();
  }

  @Override
  protected void setLow() {
    current = first;
    super.setLow();
  }

  @Override
  protected void setHigh() {
    current = first;
    current--;
    if (current < 0) {
      current = list.length - 1;
    }
    super.setHigh();
  }

  /*
   * Generate the state of the level
   */
  @Override
  protected String getState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(current);
    se.append(currentSubLevel);
    se.append(first);
    final String[] s = new String[active.length];
    for (int i = 0; i < s.length; i++) {
      s[i] = Boolean.toString(active[i]);
    }
    se.append(s);
    for (int i = 0; i < getTurnLevelCount(); i++) {
      se.append(getTurnLevel(i).getState());
    }
    return se.getValue();
  }

  /*
   * Set the state of the level
   */
  @Override
  protected void setState(String code) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    current = sd.nextInt(start);
    currentSubLevel = sd.nextInt(0); // change to 0 as default due to issue 3500
    first = sd.nextInt(0);

    final String[] s = sd.nextStringArray(0);
    active = new boolean[list.length];
    final int l = Math.min(s.length, active.length);
    for (int i = 0; i < l; i++) {
      active[i] = s[i].equals("true"); //$NON-NLS-1$
    }

    for (int i = 0; i < getTurnLevelCount(); i++) {
      getTurnLevel(i).setState(sd.nextToken("")); //$NON-NLS-1$
    }

    myValue.setPropertyValue(getValueString());
  }

  @Override
  protected String getValueString() {
    if (current >= 0 && current <= (list.length - 1)) {
      return list[current];
    }
    else {
      return ""; //$NON-NLS-1$
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see turn.TurnLevel#getLongestValueName()
   */
  @Override
  protected String getLongestValueName() {
    String s = "X"; //$NON-NLS-1$
    for (final String value : list) {
      if (value.length() > s.length()) {
        s = value;
      }
    }
    return s;
  }

  /*
   * Advance this level. 1. If there are any sub-levels, Advance the current
   * sub-level first. 2. If the sublevels roll over, then advance the counter 3.
   * If LOOP is reached, roll over the counter
   */
  @Override
  protected void advance() {
    super.advance();

    if (getTurnLevelCount() == 0 || (getTurnLevelCount() > 0 && hasSubLevelRolledOver())) {
      boolean done = false;
      for (int i = 0; i < list.length && !done; i++) {
        current++;
        if (current >= list.length) {
          current = 0;
        }
        if (current == first) {
          rolledOver = true;
        }
        done = active[current];
      }
      if (! done) {
        rolledOver = true;
      }
    }
    myValue.setPropertyValue(getValueString());
  }

  @Override
  protected void retreat() {
    super.retreat();

    if (getTurnLevelCount() == 0 || (getTurnLevelCount() > 0 && hasSubLevelRolledOver())) {
      boolean done = false;
      for (int i = 0; i < list.length && !done; i++) {
        if (current == first) {
          rolledOver = true;
        }
        current--;
        if (current < 0 || current > (list.length - 1)) {
          current = list.length - 1;
        }
        done = active[current];
      }
    }
    myValue.setPropertyValue(getValueString());
  }

  /* A list turn level is active only if at least one item is active */
  @Override
  protected boolean isActive() {
    for (final boolean b : active) {
      if (b) {
        return true;
      }
    }
    return false;
  }

  @Override
  protected void buildConfigMenu(JMenu configMenu) {
    final JMenu menu = getConfigMenu();
    if (menu != null) {
      configMenu.add(menu);
    }

    if (configFirst || configList) {
      final JMenuItem item = new JMenuItem(Resources.getString("TurnTracker.configure2", getConfigureName())); //$NON-NLS-1$
      item.addActionListener(this);
      configMenu.add(item);
    }

  }

  // Configure which Items are active
  @Override
  public void actionPerformed(ActionEvent arg0) {
    configDialog = new ConfigDialog();
    configDialog.setVisible(true);
  }

  @Override
  protected Component getSetControl() {

    final StringEnumConfigurer config = new StringEnumConfigurer("", " " + getConfigureName() + ":  ", list); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    config.setValue(list[current]);
    config.addPropertyChangeListener(e -> {
      final String option = ((StringEnumConfigurer) e.getSource()).getValueString();
      for (int i = 0; i < list.length; i++) {
        if (option.equals(list[i])) {
          current = i;
          myValue.setPropertyValue(getValueString());
        }
      }
    });

    return config.getControls();
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.ListTurnLevel.list_of_items"),
      Resources.getString("Editor.ListTurnLevel.allow_hide"),
      Resources.getString("Editor.ListTurnLevel.allow_reorder"),
      Resources.getString("Editor.ListTurnLevel.prompt_reorder")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      String[].class,
      Boolean.class,
      Boolean.class,
      String.class
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      LIST,
      CONFIG_LIST,
      CONFIG_FIRST,
      PROMPT
    );
  }

  @Override
  public void setAttribute(String key, Object value) {

    if (LIST.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      list = ((String[]) value);
      active = new boolean[list.length];
      Arrays.fill(active, true);
    }
    else if (CONFIG_LIST.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      configList = (Boolean) value;
    }
    else if (CONFIG_FIRST.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      configFirst = (Boolean) value;
    }
    else if (PROMPT.equals(key)) {
      prompt = (String) value;
    }
    else {
      super.setAttribute(key, value);
    }

  }

  @Override
  public String getAttributeValueString(String key) {
    if (LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(list);
    }
    else if (CONFIG_LIST.equals(key)) {
      return Boolean.toString(configList);
    }
    else if (CONFIG_FIRST.equals(key)) {
      return Boolean.toString(configFirst);
    }
    else if (PROMPT.equals(key)) {
      return prompt;
    }
    else
      return super.getAttributeValueString(key);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ListTurnLevel.component_type");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("TurnTracker.html", "List"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (PROMPT.equals(name)) {
      return promptCond;
    }
    else {
      return null;
    }
  }

  private final VisibilityCondition promptCond = () -> configFirst;

  @Override
  public boolean isConfigurable() {

    if (configFirst || configList) {
      return true;
    }
    else {
      return super.isConfigurable();
    }
  }

  protected class ConfigDialog extends JDialog {

    private static final long serialVersionUID = 1L;

    public ConfigDialog() {
      super(GameModule.getGameModule().getPlayerWindow(), Resources.getString("TurnTracker.configure2", getConfigureName())); //$NON-NLS-1$
      setLayout(new MigLayout("wrap 2", "[center]rel[left]"));

      if (configFirst) {
        if (prompt == null) {
          prompt = Resources.getString("Editor.ListTurnLevel.first_each", getConfigureName(), parent.getConfigureName());
        }
        final StringEnumConfigurer firstItem = new StringEnumConfigurer("", prompt + " :  ", list); //$NON-NLS-1$ //$NON-NLS-2$
        firstItem.setValue(list[first]);
        firstItem.addPropertyChangeListener(e -> {
          final String option = ((StringEnumConfigurer) e.getSource()).getValueString();
          for (int i = 0; i < list.length; i++) {
            if (list[i].equals(option)) {
              first = i;
            }
          }
        });
        add(firstItem.getControls(), "span 2");
      }

      if (configList) {

        add(new JLabel(Resources.getString("TurnTracker.turn_off")), "span 2"); //$NON-NLS-1$
        for (int i = 0; i < list.length; i++) {
          final BooleanConfigurer b = new BooleanConfigurer(null, list[i], active[i]);
          b.addPropertyChangeListener(e -> {
            final BooleanConfigurer b1 = (BooleanConfigurer) e.getSource();
            final String option = b1.getName();
            for (int i1 = 0; i1 < list.length; i1++) {
              if (list[i1].equals(option)) {
                active[i1] = b1.booleanValue();
              }
            }
          });
          add(b.getControls());
          add(new JLabel(list[i]));
        }
      }

      final JPanel p = new JPanel(new MigLayout("ins 0", "push[]rel[]push"));

      final JButton saveButton = new JButton(Resources.getString(Resources.SAVE));
      saveButton.setToolTipText(Resources.getString("TurnTracker.save_changes")); //$NON-NLS-1$
      p.add(saveButton);
      saveButton.addActionListener(e -> setVisible(false));

      final JButton cancelButton = new JButton(Resources.getString(Resources.CANCEL));
      cancelButton.setToolTipText(Resources.getString("TurnTracker.discard_changes")); //$NON-NLS-1$
      cancelButton.addActionListener(e -> setVisible(false));
      p.add(cancelButton);

      // Default actions for Enter/ESC
      SwingUtils.setDefaultButtons(getRootPane(), saveButton, cancelButton);

      add(p, "span 2, growx");
      pack();
    }
  }

  @Override
  public ComponentI18nData getI18nData() {
    final ComponentI18nData myI18nData = super.getI18nData();
    myI18nData.setAttributeTranslatable(LIST, true);
    return myI18nData;
  }

}
