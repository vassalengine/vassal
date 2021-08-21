/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.configure.AbstractConfigurableListEntry;
import VASSAL.configure.ConfigurableList;
import VASSAL.configure.ConfigurableListController;
import VASSAL.configure.ConfigurableListEntry;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerLayout;
import VASSAL.counters.TraitLayout;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

public class SortParameterArrayConfigurer extends Configurer implements ConfigurableList {

  private JPanel controls;
  private JPanel panel;
  private int selectedEntryIndex = -1;
  private ConfigurableListController controller;
  private final List<SortParameterEntry> entries = new ArrayList<>();

  public SortParameterArrayConfigurer(String key, String name) {
    this(key, name, null);
  }

  public SortParameterArrayConfigurer(String key, String name, Object val) {
    super(key, name, val);
  }

  @Override
  public void moveEntryUp() {
    if (getSelectedEntryIndex() < 0) {
      return;
    }

    final int pos = getSelectedEntryIndex();
    final List<SortParameter> params = getValue() == null ? new ArrayList<>() : getSortParameterListValue();
    final SortParameter moving = params.remove(pos);
    params.add(pos - 1, moving);
    setValue(params);

    setSelectedEntryIndex(pos - 1);

    rebuildControls();
  }

  @Override
  public void moveEntryDown() {
    if (getSelectedEntryIndex() < 0) {
      return;
    }

    final int pos = getSelectedEntryIndex();
    final List<SortParameter> params = getValue() == null ? new ArrayList<>() : getSortParameterListValue();
    final SortParameter moving = params.remove(pos);
    params.add(pos + 1, moving);
    setValue(params);

    setSelectedEntryIndex(pos + 1);

    rebuildControls();
  }

  @Override
  public void addEntry() {
    final int pos = getSelectedEntryIndex();
    final List<SortParameter> params = getValue() == null ? new ArrayList<>() : getSortParameterListValue();

    int newEntry;
    // Insert the new entry into the list at the appropriate place
    if (entries.isEmpty() || getSelectedEntryIndex() < 0) {
      params.add(new SortParameter());
      setValue(params);
      newEntry = getSortParameterListValue().size() - 1;
    }
    else {
      newEntry = pos + 1;
      params.add(pos, new SortParameter());
      setValue(params);
    }
    setSelectedEntryIndex(newEntry);
    rebuildControls(newEntry);
  }

  @Override
  public void deleteEntry(ConfigurableListEntry entry) {
    final int pos = entries.indexOf(entry);
    final List<SortParameter> params = getValue() == null ? new ArrayList<>() : getSortParameterListValue();
    params.remove(pos);
    setValue(params);
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

  public static List<SortParameter> decode(String s) {
    if (s == null) {
      return null;
    }

    final ArrayList<SortParameter> l = new ArrayList<>();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    while (st.hasMoreTokens()) {
      final String token = st.nextToken();
      if (!token.isEmpty()) {
        l.add(new SortParameter(token));
      }
    }
    return l;
  }

  public static String encode(List<SortParameter> params) {
    if (params == null) {
      return null;
    }
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final SortParameter param : params) {
      se.append(param.toString());
    }
    return se.getValue() != null ? se.getValue() : "";
  }

  @Override
  public String getValueString() {
    return encode(getSortParameterListValue());
  }

  @Override
  public void setValue(String s) {
    setValue(decode(s));
  }

  @Override
  public void setValue(Object o) {
    final List<SortParameter> newParams = o == null ? new ArrayList<>() : new ArrayList<>((List<SortParameter>) o);
    if (o == null) {
      newParams.add(new SortParameter());
    }
    super.setValue(newParams);
    if (controls != null) {
      rebuildControls();
    }
  }

  private void setParameterValue(int pos, SortParameter param) {
    getSortParameterListValue().get(pos).setValue(param);
    fireUpdate();
  }

  public List<SortParameter> getSortParameterListValue() {
    return (List<SortParameter>) getValue();
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

    final List<SortParameter> params = getSortParameterListValue();

    for (final SortParameter param : params) {
      final SortParameterEntry entry = new SortParameterEntry(this, param);
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
    for (final SortParameterEntry entry : entries) {
      entry.setHighlighted(i++ == getSelectedEntryIndex());
    }
    getListController();
    controller.setCanMoveUp(getSelectedEntryIndex() > 0);
    controller.setCanMoveDown(getSelectedEntryIndex() >= 0 && getSelectedEntryIndex() < entries.size() - 1);

    controls.repaint();

  }

  @Override
  public void repack() {
    repack(panel);
  }

  @Override
  public void setSelectedEntryIndex(int index) {
    selectedEntryIndex = Math.min(index, entries.size() - 1);
    updateControls();
  }

  @Override
  public int getSelectedEntryIndex() {
    return selectedEntryIndex;
  }

  @Override
  public void entryChanged(ConfigurableListEntry entry) {
    setParameterValue(entries.indexOf(entry), (SortParameter) entry.getConfigurer().getValue());
  }

  static class SortParameterEntry extends AbstractConfigurableListEntry {

    public SortParameterEntry(ConfigurableList parentConfig, Object value) {
      super(parentConfig, value);
    }

    @Override
    public Configurer buildChildConfigurer(Object value) {
      final Configurer c = new SortParameterConfigurer();
      c.setFrozen(true);
      c.setValue(value);
      c.setFrozen(false);
      return c;
    }

    @Override
    public void setHighlighted(boolean b) {
      getConfigurer().setHighlighted(b);
    }
  }
}
