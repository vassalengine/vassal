/*
 *
 * Copyright (c) 2000-2011 by Rodney Kinney, Brent Easton
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

package VASSAL.configure;

import VASSAL.counters.TraitLayout;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.ArrayUtils;

/**
 * A Configurer that returns an array of Strings
 */
public class StringArrayConfigurer extends Configurer implements ConfigurableList {

  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected JList<String> list;
  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected DefaultListModel<String> model;
  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected JTextField textField;
  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected int minRows = 3;
  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected int maxRows = 3;

  protected JPanel panel;
  private static final String[] EMPTY = new String[0];
  private final List<StringEntry> entries = new ArrayList<>();
  private int selectedEntryIndex = -1;
  private ConfigurableListController controller;
  private JPanel controls;
  private FocusListener sharedFocusListener;

  public StringArrayConfigurer(String key, String name, Object val) {
    super(key, name, val);
  }

  public StringArrayConfigurer(Object val) {
    this(null, "", val);
  }

  public StringArrayConfigurer(String key, String name) {
    super(key, name);
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  public DefaultListModel<String> getModel() {
    return model;
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  public void addValue(String s) {
    setValue(value == null ?
      new String[]{s} : ArrayUtils.add((String[]) value, s));
  }

  public void removeValue(String s) {
    final String[] oldValue = getStringArray();
    final String[] newValue = ArrayUtils.removeElement(oldValue, s);
    if (oldValue != newValue) setValue(newValue);
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel(new MigLayout("hidemode 3,ins 0," + TraitLayout.STANDARD_GAPY, "[grow,fill][]")); // NON-NLS

      controls = new JPanel(new MigLayout(ConfigurerLayout.STANDARD_GAPY, "[grow,fill][]")); // NON-NLS
      controls.setBorder(BorderFactory.createEtchedBorder());
      panel.add(controls, "grow"); // NON-NLS
      panel.add(getListController(), "growy 0,aligny center"); // NON-NLS

      rebuildControls();
    }
    return panel;
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  public void updateViewable(int rows) {
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected Component getTextComponent() {
    return textField;
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected String getTextValue() {
    return textField.getText();
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected void setTextValue(String s) {
    textField.setText(s);
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected void addTextActionListener(ActionListener a) {
    textField.addActionListener(a);
  }

  public String[] getStringArray() {
    if (value instanceof String[]) {
      return (String[]) value;
    }
    else {
      return EMPTY;
    }
  }

  @Override
  public String getValueString() {
    return arrayToString(getStringArray());
  }

  public static String arrayToString(String[] s) {
    if (s == null || s.length == 0) {
      return "";
    }
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final String item : s) {
      se.append(item != null ? item : "");
    }
    return se.getValue();
  }

  @Override
  public void setValue(Object o) {
    if (o == null) {
      o = EMPTY;
    }
    super.setValue(o);
    if (controls != null) {
      rebuildControls();
    }
  }

  @Override
  public void setValue(String s) {
    final String[] val = stringToArray(s);
    setValue(val);
  }

  public static String[] stringToArray(String s) {
    if (s == null
        || s.length() == 0) {
      return EMPTY;
    }
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    final List<String> l = new ArrayList<>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    return l.toArray(new String[0]);
  }

  @Deprecated(since = "2020-11-12", forRemoval = true)
  protected void updateModel() {

  }

  /**
   * Rebuild controls from scratch and set focus
   * @param focus index of entry to request focus
   */
  private void rebuildControls(int focus) {
    rebuildControls();
    entries.get(focus).requestFocus();
  }

  /**
   * Rebuild controls from scratch
   */
  private void rebuildControls() {
    for (final StringEntry entry : entries) {
      entry.removeFocusListener(sharedFocusListener);
      entry.removePropertyChangeListener(entry);
    }
    controls.removeAll();
    entries.clear();

    final String[] strings = getStringArray();

    for (final String string : strings) {
      final StringEntry entry = new StringEntry(this, string);
      entry.addFocusListener(sharedFocusListener);
      controls.add(entry.getConfigurer().getControls());
      controls.add(entry.getRemoveButton(), "growx 0,wrap"); // NON-NLS
      entries.add(entry); // NON-NLS
    }

    updateControls();
    repack();
  }

  /**
   * Refresh visible state of controls without rebuilding
   */
  private void updateControls() {
    int i = 0;
    for (final StringEntry entry : entries) {
      entry.setHighlighted(i++ == getSelectedEntryIndex());
    }
    getListController();
    controller.setCanMoveUp(getSelectedEntryIndex() > 0);
    controller.setCanMoveDown(getSelectedEntryIndex() >= 0 && getSelectedEntryIndex() < entries.size() - 1);

    controls.repaint();

  }

  public Configurer buildChildConfigurer(Object value) {
    final StringConfigurer s = new StringConfigurer((String) value);
    s.setHint(getHint());
    return s;
  }

  static class StringEntry extends AbstractConfigurableListEntry {

    public StringEntry(ConfigurableList parentConfig, Object value) {
      super(parentConfig, value);
    }

    @Override
    public Configurer buildChildConfigurer(Object value) {
      return ((StringArrayConfigurer) getParent()).buildChildConfigurer(value);
    }

    @Override
    public void setHighlighted(boolean b) {
      getConfigurer().setHighlighted(b);
    }

    public void addFocusListener(FocusListener listener) {
      getConfigurer().addFocusListener(listener);
    }

    public void removeFocusListener(FocusListener listener) {
      getConfigurer().removeFocusListener(listener);
    }
  }

  @Override
  public void moveEntryUp() {
    if (getSelectedEntryIndex() < 0) {
      return;
    }

    final int pos = getSelectedEntryIndex();
    String[] strings = getStringArray();
    final String moving = strings[pos];

    strings = ArrayUtils.remove(strings, pos);
    strings = ArrayUtils.insert(pos - 1, strings, moving);
    setValue(strings);

    setSelectedEntryIndex(pos - 1);

    rebuildControls();
  }

  @Override
  public void moveEntryDown() {
    if (getSelectedEntryIndex() < 0) {
      return;
    }

    final int pos = getSelectedEntryIndex();
    String[] strings = getStringArray();
    final String moving = strings[pos];

    strings = ArrayUtils.remove(strings, pos);
    strings = ArrayUtils.insert(pos + 1, strings, moving);
    setValue(strings);

    setSelectedEntryIndex(pos + 1);

    rebuildControls();
  }

  @Override
  public void addEntry() {
    int newEntry;
    final int pos = getSelectedEntryIndex();

    // Insert the new entry into the list at the appropriate place
    if (entries.isEmpty() || getSelectedEntryIndex() < 0) {
      setValue(ArrayUtils.add(getStringArray(), ""));
      newEntry = getStringArray().length - 1;
      setSelectedEntryIndex(newEntry);
    }
    else {
      newEntry = pos + 1;
      setValue(ArrayUtils.insert(newEntry, getStringArray(), ""));
      setSelectedEntryIndex(newEntry);
    }

    rebuildControls(newEntry);
  }

  @Override
  public void deleteEntry(ConfigurableListEntry entry) {
    final int pos = entries.indexOf(entry);

    setValue(ArrayUtils.remove(getStringArray(), pos));
    setSelectedEntryIndex(Math.min(pos, entries.size() - 1));

    if (selectedEntryIndex >= 0) {
      entries.get(selectedEntryIndex).requestFocus();
    }
  }

  @Override
  public JComponent getListController() {
    if (controller == null) {
      controller = new ConfigurableListController(this);
    }
    return controller;
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    sharedFocusListener = listener;
    for (final StringEntry entry : entries) {
      entry.addFocusListener(listener);
    }
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    for (final StringEntry entry : entries) {
      entry.removeFocusListener(listener);
    }
    sharedFocusListener = null;
  }

  @Override
  public void selectEntry(ConfigurableListEntry entry) {
    setSelectedEntryIndex(entry == null ? -1 : entries.indexOf(entry));
    updateControls();
  }

  @Override
  public void repack() {
    repack(panel);
  }

  @Override
  public void setSelectedEntryIndex(int index) {
    selectedEntryIndex = Math.min(index, entries.size() - 1);
  }

  @Override
  public int getSelectedEntryIndex() {
    return selectedEntryIndex;
  }

  @Override
  public void entryChanged(ConfigurableListEntry entry) {
    final String[] oldValue = ArrayUtils.clone(getStringArray());
    getStringArray()[entries.indexOf(entry)] = entry.getConfigurer().getValueString();
    if (!frozen) {
      changeSupport.firePropertyChange(key, oldValue, getStringArray());
    }
  }

  /**
   * Don't do anything special when highlighting, but deslect currently selected entry when unhighlighting
   * @param highlighted New Highlighted status
   */
  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    if (! highlighted) {
      selectEntry(null);
    }
  }

  public List<StringEntry> getEntries() {
    return entries;
  }
}
