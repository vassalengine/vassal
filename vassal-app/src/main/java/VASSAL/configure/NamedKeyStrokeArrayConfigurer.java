/*
 *
 * Copyright (c) 2004-2011 by Rodney Kinney, Brent Easton
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
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.ArrayUtils;

/**
 * Configures an array of {link NamedKeyStrokes}
 */
public class NamedKeyStrokeArrayConfigurer extends Configurer implements ConfigurableList {
  private JPanel controls;
  private JPanel panel;
  private int selectedEntryIndex = -1;
  private final List<NKSAEntry> entries = new ArrayList<>();
  private ConfigurableListController controller;

  public NamedKeyStrokeArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public NamedKeyStrokeArrayConfigurer(String key, String name, NamedKeyStroke[] val) {
    super(key, name, val);
  }

  public NamedKeyStrokeArrayConfigurer(String key, String name, List<NamedKeyStroke> val) {
    this(key, name, val.toArray(new NamedKeyStroke[0]));
  }

  public NamedKeyStrokeArrayConfigurer(NamedKeyStroke[] val) {
    this(null, "", val);
  }

  public NamedKeyStroke[] getNameKeyStrokeArrayValue() {
    return (NamedKeyStroke[]) getValue();
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel(new MigLayout("ins 0," + TraitLayout.STANDARD_GAPY, "[grow,fill][]")); // NON-NLS

      controls = new JPanel(new MigLayout(ConfigurerLayout.STANDARD_GAPY, "[grow,fill][]")); // NON-NLS
      controls.setBorder(BorderFactory.createEtchedBorder());
      panel.add(controls, "grow"); // NON-NLS
      panel.add(getListController(), "growy 0,aligny center"); // NON-NLS

      rebuildControls();

    }
    return panel;
  }

  private void rebuildControls(int focus) {
    rebuildControls();
    entries.get(focus).requestFocus();
  }
  /**
   * Rebuild controls from scratch
   */
  private void rebuildControls() {
    entries.clear();
    controls.removeAll();

    final NamedKeyStroke[] keys = getNameKeyStrokeArrayValue();

    for (final NamedKeyStroke key : keys) {
      final NKSAEntry entry = new NKSAEntry(this, key);
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
    for (final NKSAEntry entry : entries) {
      entry.setHighlighted(i++ == getSelectedEntryIndex());
    }
    getListController();
    controller.setCanMoveUp(getSelectedEntryIndex() > 0);
    controller.setCanMoveDown(getSelectedEntryIndex() >= 0 && getSelectedEntryIndex() < entries.size() - 1);

    controls.repaint();

  }

  @Override
  public String getValueString() {
    return encode(getKeyStrokes());
  }

  @Override
  public void setValue(String s) {
    setValue(decode(s));
  }

  @Override
  public void setValue(Object o) {
    super.setValue(ArrayUtils.clone((NamedKeyStroke[]) o));
    if (controls != null) {
      rebuildControls();
    }
  }

  private void setKeyValue(int pos, NamedKeyStroke stroke) {
    final NamedKeyStroke[] oldValue = ArrayUtils.clone(getNameKeyStrokeArrayValue());
    getNameKeyStrokeArrayValue()[pos] = stroke;
    if (!frozen) {
      changeSupport.firePropertyChange(key, oldValue, getNameKeyStrokeArrayValue());
    }
  }

  public NamedKeyStroke[] getKeyStrokes() {
    final ArrayList<NamedKeyStroke> l = new ArrayList<>();
    for (final NKSAEntry entry : entries) {
      final NamedKeyStroke value = (NamedKeyStroke)  entry.getConfigurer().getValue();
      if (value != null) {
        l.add(value);
      }
    }
    return l.toArray(new NamedKeyStroke[0]);
  }

  public static NamedKeyStroke[] decode(String s) {
    if (s == null) {
      return null;
    }
    final ArrayList<NamedKeyStroke> l = new ArrayList<>();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    while (st.hasMoreTokens()) {
      final String token = st.nextToken();
      if (!token.isEmpty()) {
        l.add(NamedHotKeyConfigurer.decode(token));
      }
    }
    return l.toArray(new NamedKeyStroke[0]);
  }

  public static String encode(NamedKeyStroke[] keys) {
    if (keys == null) {
      return null;
    }
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final NamedKeyStroke key : keys) {
      if (!key.isNull()) {
        se.append(NamedHotKeyConfigurer.encode(key));
      }
    }
    return se.getValue() != null ? se.getValue() : "";
  }

  @Override
  public void moveEntryUp() {
    if (getSelectedEntryIndex() < 0) {
      return;
    }

    final int pos = getSelectedEntryIndex();
    NamedKeyStroke[] keys = getValue() == null ? new NamedKeyStroke[0] : getNameKeyStrokeArrayValue();
    final NamedKeyStroke moving = keys[pos];

    keys = ArrayUtils.remove(keys, pos);
    keys = ArrayUtils.insert(pos - 1, keys, moving);
    setValue(keys);

    setSelectedEntryIndex(pos - 1);

    rebuildControls();
  }

  @Override
  public void moveEntryDown() {
    if (getSelectedEntryIndex() < 0) {
      return;
    }

    final int pos = getSelectedEntryIndex();
    NamedKeyStroke[] keys = getValue() == null ? new NamedKeyStroke[0] : getNameKeyStrokeArrayValue();
    final NamedKeyStroke moving = keys[pos];

    keys = ArrayUtils.remove(keys, pos);
    keys = ArrayUtils.insert(pos + 1, keys, moving);
    setValue(keys);

    setSelectedEntryIndex(pos + 1);

    rebuildControls();
  }

  @Override
  public void addEntry() {

    final int pos = getSelectedEntryIndex();
    final NamedKeyStroke[] keys = getValue() == null ? new NamedKeyStroke[0] : getNameKeyStrokeArrayValue();

    int newEntry;
    // Insert the new entry into the list at the appropriate place
    if (entries.isEmpty() || getSelectedEntryIndex() < 0) {
      setValue(ArrayUtils.add(keys, NamedKeyStroke.NULL_KEYSTROKE));
      newEntry = getNameKeyStrokeArrayValue().length - 1;
      setSelectedEntryIndex(newEntry);
    }
    else {
      newEntry = pos + 1;
      setValue(ArrayUtils.insert(newEntry, keys, NamedKeyStroke.NULL_KEYSTROKE));
      setSelectedEntryIndex(newEntry);
    }

    rebuildControls(newEntry);

  }

  @Override
  public void deleteEntry(ConfigurableListEntry entry) {

    final int pos = entries.indexOf(entry);
    final NamedKeyStroke[] keys = getValue() == null ? new NamedKeyStroke[0] : getNameKeyStrokeArrayValue();

    setValue(ArrayUtils.remove(keys, pos));
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

  /** A List entry has had it's value changed. */
  @Override
  public void entryChanged(ConfigurableListEntry entry) {
    setKeyValue(entries.indexOf(entry), (NamedKeyStroke) entry.getConfigurer().getValue());
  }

  static class NKSAEntry extends AbstractConfigurableListEntry {

    /**
     * Build a new List Entry
     *
     * @param parentConfig Parent List
     * @param value        Initial value for entry
     */
    public NKSAEntry(ConfigurableList parentConfig, Object value) {
      super(parentConfig, value);
    }

    public NKSAEntry(ConfigurableList parentConfig) {
      this(parentConfig, null);
    }

    @Override
    public Configurer buildChildConfigurer(Object value) {
      final Configurer c = new NamedHotKeyConfigurer();
      c.setValue(value);
      return c;
    }

    @Override
    public void setHighlighted(boolean b) {
      getConfigurer().setHighlighted(b);
    }

  }
}
