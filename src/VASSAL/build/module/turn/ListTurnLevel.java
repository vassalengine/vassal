/*
 * $Id$
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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArrayUtils;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

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
  protected void reset() {
    super.reset();
    for (int i = 0; i < active.length; i++) {
      active[i] = true;
    }
    setLow();
  }

  protected void setLow() {
    current = first;
    super.setLow();
  }

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
  protected String getState() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(current);
    se.append(currentSubLevel);
    se.append(first);
    String s[] = new String[active.length];
    for (int i = 0; i < s.length; i++) {
      s[i] = active[i] + ""; //$NON-NLS-1$
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
  protected String getLongestValueName() {
    String s = "X"; //$NON-NLS-1$
    for (int i = 0; i < list.length; i++) {
      if (list[i].length() > s.length()) {
        s = list[i];
      }
    }
    return s;
  }

  /*
   * Advance this level. 1. If there are any sub-levels, Advance the current
   * sub-level first. 2. If the sublevels roll over, then advance the counter 3.
   * If LOOP is reached, roll over the counter
   */
  protected void advance() {
    super.advance();

    if (getTurnLevelCount() == 0 || (getTurnLevelCount() > 0 && hasSubLevelRolledOver())) {
      int idx = current;
      boolean done = false;
      for (int i = 0; i < list.length && !done; i++) {
        idx++;
        if (idx >= list.length) {
          idx = 0;
        }
        if (idx == first) {
          rolledOver = true;
        }
        done = active[idx];
      }
      current = idx;
      if (! done) {
        rolledOver = true;
      }
    }
    myValue.setPropertyValue(getValueString());
  }

  protected void retreat() {
    super.retreat();

    if (getTurnLevelCount() == 0 || (getTurnLevelCount() > 0 && hasSubLevelRolledOver())) {
      int idx = current;
      boolean done = false;
      for (int i = 0; i < list.length && !done; i++) {
        if (idx == first) {
          rolledOver = true;
        }
        idx--;
        if (idx < 0 || idx > (list.length - 1)) {
          idx = list.length - 1;
        }
        done = active[idx];
      }
      current = idx;
    }
    myValue.setPropertyValue(getValueString());
  }

  /* A list turn level is active only if at least one item is active */
  protected boolean isActive() {
    for (int i = 0; i < active.length; i++) {
      if (active[i]) {
        return true;
      }
    }
    return false;
  }

  protected void buildConfigMenu(JMenu configMenu) {
    JMenu menu = getConfigMenu();
    if (menu != null) {
      configMenu.add(menu);
    }

    if (configFirst || configList) {
      JMenuItem item = new JMenuItem(Resources.getString("TurnTracker.configure2", getConfigureName())); //$NON-NLS-1$
      item.addActionListener(this);
      configMenu.add(item);
    }

  }

  // Configure which Items are active
  public void actionPerformed(ActionEvent arg0) {
    configDialog = new ConfigDialog();
    configDialog.setVisible(true);
  }

  protected Component getSetControl() {

    StringEnumConfigurer config = new StringEnumConfigurer("", " " + getConfigureName() + ":  ", list); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    config.setValue(list[current]);
    config.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent e) {
        String option = ((StringEnumConfigurer) e.getSource()).getValueString();
        for (int i = 0; i < list.length; i++) {
          if (option.equals(list[i])) {
            current = i;
            myValue.setPropertyValue(getValueString());
          }
        }
      }
    });

    return config.getControls();
  }

  public String[] getAttributeDescriptions() {
    return ArrayUtils.append(
      super.getAttributeDescriptions(),
      "List of Items",
      "Allow players to hide items in this list?",
      "Allow players to change which item goes first?",
      "Prompt to players to select which item goes first:  "
    );
  }

  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.append(
      super.getAttributeTypes(),
      String[].class,
      Boolean.class,
      Boolean.class,
      String.class
    );
  }

  public String[] getAttributeNames() {
    return ArrayUtils.append(
      super.getAttributeNames(),
      LIST,
      CONFIG_LIST,
      CONFIG_FIRST,
      PROMPT
    );
  }

  public void setAttribute(String key, Object value) {

    if (LIST.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      list = ((String[]) value);
      active = new boolean[list.length];
      for (int i = 0; i < active.length; i++) {
        active[i] = true;
      }
    }
    else if (CONFIG_LIST.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      configList = ((Boolean) value).booleanValue();
    }
    else if (CONFIG_FIRST.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      configFirst = ((Boolean) value).booleanValue();
    }
    else if (PROMPT.equals(key)) {
      prompt = (String) value;
    }
    else {
      super.setAttribute(key, value);
    }

  }

  public String getAttributeValueString(String key) {
    if (LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(list);
    }
    else if (CONFIG_LIST.equals(key)) {
      return configList + ""; //$NON-NLS-1$
    }
    else if (CONFIG_FIRST.equals(key)) {
      return configFirst + ""; //$NON-NLS-1$
    }
    else if (PROMPT.equals(key)) {
      return prompt;
    }
    else
      return super.getAttributeValueString(key);
  }

  public static String getConfigureTypeName() {
    return "List";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("TurnTracker.htm","List"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (PROMPT.equals(name)) {
      return promptCond;
    }
    else {
      return null;
    }
  }

  private VisibilityCondition promptCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return configFirst;
    }
  };

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
      super(GameModule.getGameModule().getFrame(), Resources.getString("TurnTracker.configure2", getConfigureName())); //$NON-NLS-1$
      setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

      if (configFirst) {
        if (prompt == null) {
          prompt = "First " + getConfigureName() + " each " + parent.getConfigureName();
        }
        StringEnumConfigurer firstItem = new StringEnumConfigurer("", prompt + " :  ", list); //$NON-NLS-1$ //$NON-NLS-2$
        firstItem.setValue(list[first]);
        firstItem.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent e) {
            String option = ((StringEnumConfigurer) e.getSource()).getValueString();
            for (int i = 0; i < list.length; i++) {
              if (list[i].equals(option)) {
                first = i;
              }
            }
          }
        });
        add(firstItem.getControls());
      }

      if (configList) {

        add(new JLabel(Resources.getString("TurnTracker.turn_off"))); //$NON-NLS-1$
        for (int i = 0; i < list.length; i++) {

          BooleanConfigurer b =
            new BooleanConfigurer(null, list[i], Boolean.valueOf(active[i]));
          b.addPropertyChangeListener(new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent e) {
              BooleanConfigurer b = (BooleanConfigurer) e.getSource();
              String option = b.getName();
              for (int i = 0; i < list.length; i++) {
                if (list[i].equals(option)) {
                  active[i] = ((BooleanConfigurer) e.getSource()).booleanValue().booleanValue();
                }
              }
            }
          });
          add(b.getControls());
        }
      }

      JPanel p = new JPanel();

      JButton saveButton = new JButton(Resources.getString(Resources.SAVE));
      saveButton.setToolTipText(Resources.getString("TurnTracker.save_changes")); //$NON-NLS-1$
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });

      JButton cancelButton = new JButton(Resources.getString(Resources.CANCEL));
      cancelButton.setToolTipText(Resources.getString("TurnTracker.discard_changes")); //$NON-NLS-1$
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });
      p.add(cancelButton);

      add(p);
      pack();
    }
  }

  public ComponentI18nData getI18nData() {
    ComponentI18nData myI18nData = super.getI18nData();
    myI18nData.setAttributeTranslatable(LIST, true);
    return myI18nData;
  }

}
