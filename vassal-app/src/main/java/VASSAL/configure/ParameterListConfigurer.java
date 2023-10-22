/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

import net.miginfocom.swing.MigLayout;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.Component;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;

/**
 * Configurer for a List of Parameters
 * The object stored in the value of the Configurer is a List<Parameter>
 */
public class ParameterListConfigurer extends Configurer implements ConfigurableList {

  // The number of Components added to the header of the Controls panel
  private static final int HEADER_COMPONENT_COUNT = 2;
  // The number of Components added to the Controls panel for each Entry
  private static final int COMPONENT_COUNT = 3;

  private ConfigurableListController controller;
  private int selectedEntryIndex = -1;
  private JPanel panel;
  private JPanel controls;
  private JPanel configControls;
  private final List<ConfigurableListEntry> entries = new ArrayList<>();

  public static String encode(List<Parameter> parameters) {
    if (parameters == null) return "";

    final SequenceEncoder se = new SequenceEncoder(',');
    for (final Parameter param : parameters) {
      if (param != null) {
        se.append(param.encode());
      }
    }
    return se.getValue();
  }

  public static List<Parameter> decode(String s) {
    final List<Parameter> parameters = new ArrayList<>();
    if (s != null && s.length() > 0) {
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      while (sd.hasMoreTokens()) {
        parameters.add(new Parameter(sd.nextToken("")));
      }
    }
    return parameters;
  }

  public ParameterListConfigurer(String key, String name, List<Parameter> params) {
    super(key, name);
    value = params;
  }

  public ParameterListConfigurer(String key, String name) {
    this(key, name, new ArrayList<>());
  }

  public ParameterListConfigurer(List<Parameter> params) {
    this("", "", params);
  }


  /**
   * Return a list of the Parameter names in the current configurer value
   * @return  Parameter names
   */
  public List<String> getParameterNames() {
    final List<String> l = new ArrayList<>();
    if (getListValue() != null) {
      for (final Object p : getListValue()) {
        l.add(((Parameter) p).getPropertyName());
      }
    }
    return l;
  }

  public List<Parameter> getParameterList() {
    final List<Parameter> parameterList = new ArrayList<>();
    for (final Object o : getListValue()) {
      parameterList.add((Parameter) o);
    }
    return parameterList;
  }

  /**
   * Return a list of the Parameter values in the current configurer value
   * @return  Parameter values
   */
  public List<String> getParameterValues() {
    final List<String> l = new ArrayList<>();
    if (getListValue() != null) {
      for (final Object p : getListValue()) {
        l.add(((Parameter) p).getValue());
      }
    }
    return l;
  }

  @Override
  public void moveEntryUp() {
    final int pos = getSelectedEntryIndex();
    if (pos < 0 || pos > entries.size() - 1) {
      return;
    }

    final int listPos = getSelectedEntryIndex();
    final ParameterEntry entry = (ParameterEntry) entries.get(listPos);

    // Remove the Configurer controls from their current position and re-insert one position up
    removeConfigControls(entry);
    insertConfigControls(entry, listPos - 1);

    // Remove the entry from its current position on the list and re-insert one position up
    entries.remove(entry);
    entries.add(listPos - 1, entry);

    // Swap the entries in the value
    final Object temp = getListValue().get(pos);
    getListValue().set(pos, getListValue().get(pos - 1));
    getListValue().set(pos - 1, temp);

    // Selection to follow moving entry
    setSelectedEntryIndex(pos - 1);

    updateControls();
  }

  @Override
  public void moveEntryDown() {
    final int pos = getSelectedEntryIndex();
    if (pos < 0 || pos >= entries.size()) {
      return;
    }

    final int listPos = getSelectedEntryIndex();
    final ParameterEntry entry = (ParameterEntry) entries.get(listPos);

    // Remove the Configurer controls from their current position and re-insert one position down
    removeConfigControls(entry);
    insertConfigControls(entry, listPos + 1);

    // Remove the entry from its current position on the list and re-insert one position down
    entries.remove(entry);
    entries.add(listPos + 1, entry);

    // Swap entries in the value
    final Object temp = getListValue().get(pos);
    getListValue().set(pos, getListValue().get(pos + 1));
    getListValue().set(pos + 1, temp);

    // Selection to follow moving entry
    setSelectedEntryIndex(pos + 1);

    updateControls();

  }

  @Override
  public void addEntry() {

    // Find the position of currently selected entry. The new entry will be created below it.
    final int pos = getSelectedEntryIndex();

    // Create a new empty entry
    final ParameterEntry newEntry = new ParameterEntry(this);

    // Insert the entry into the value
    getListValue().add(pos + 1, newEntry.getConfigurer().getValue());

    // Insert the entry into the model
    entries.add(pos + 1, newEntry);

    // Insert the controls
    insertConfigControls(newEntry, pos + 1);

    // Select the newly created entry
    setSelectedEntryIndex(pos + 1);

    // Refresh the view
    updateControls(pos + 1);

  }

  @Override
  public void deleteEntry(ConfigurableListEntry entry) {

    final int listPos = entries.indexOf(entry);
    final int componentPos = listPosToControlsPos(listPos);
    for (int i = 0; i < COMPONENT_COUNT; i++) {
      configControls.remove(componentPos);
    }

    entries.remove(entry);

    if (getSelectedEntryIndex() > entries.size() - 1) {
      setSelectedEntryIndex(entries.size() - 1);
    }

    getListValue().remove(listPos);
    updateControls();

  }


  @Override
  public void selectEntry(ConfigurableListEntry entry) {
    setSelectedEntryIndex(entry == null ? -1 : entries.indexOf(entry));
    updateControls();
  }

  @Override
  public void setSelectedEntryIndex(int index) {
    selectedEntryIndex = index;
  }

  @Override
  public int getSelectedEntryIndex() {
    return selectedEntryIndex;
  }

  @Override
  public void entryChanged(ConfigurableListEntry entry) {
    final int pos = entries.indexOf(entry);
    if (pos >= 0) {
      getListValue().set(pos, entry.getConfigurer().getValue());
      fireUpdate();
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
  public String getValueString() {
    return encode(getParameterListValue());
  }

  public List<Object> getListValue() {
    return (List<Object>) getValue();
  }

  public List<Parameter> getParameterListValue() {
    return (List<Parameter>) getValue();
  }

  @Override
  public void setValue(String s) {
    getListValue().clear();
    getListValue().addAll(decode(s));
    rebuildControls();
  }

  @Override
  public void setValue(Object o) {
    if (o == null) {
      o = new ArrayList<>();
    }
    super.setValue(o);
    rebuildControls();
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel(new MigLayout(TraitLayout.STANDARD_GAPY, "[grow,fill][]")); // NON-NLS
      panel.setBorder(BorderFactory.createEtchedBorder());

      controls = new JPanel(new MigLayout("hidemode 3,ins 2", "[grow,fill]", "[grow,fill]")); // NON-NLS

      configControls = new JPanel(new MigLayout("hidemode 3,wrap 3," + ConfigurerLayout.STANDARD_INSETS_GAPY, "[fill]rel[grow,fill]rel[grow 0]", "[center]")); // NON-NLS

      controls.add(configControls, "grow, aligny center"); // NON-NLS
      panel.add(controls, "grow"); // NON-NLS
      panel.add(getListController(), "growy 0,aligny center"); // NON-NLS

      rebuildControls();
    }
    return panel;
  }

  protected void rebuildControls() {
    if (controls != null) {

      // Remove all entries and Controls
      entries.clear();
      configControls.removeAll();

      // Build a new header
      buildHeader();

      // Create a new set of entries and add the controls
      for (final Object value : getListValue()) {
        final ParameterEntry entry = new ParameterEntry(this, value);
        entries.add(entry);
        appendConfigControls(entry);
      }

      updateControls();
      repack();
    }
  }

  private void updateControls(int focus) {
    updateControls();
    entries.get(focus).requestFocus();
  }

  /**
   * Update visibility, enabled state as required
   */
  private void updateControls() {

    configControls.setVisible(!getListValue().isEmpty());

    for (int i = 0; i < entries.size(); i++) {
      entries.get(i).updateVisibility();
      entries.get(i).setHighlighted(i == getSelectedEntryIndex());
    }

    updateListController();
    configControls.revalidate();
    repack();
  }

  private void updateListController() {
    getListController();
    controller.setCanMoveUp(getSelectedEntryIndex() > 0);
    controller.setCanMoveDown(getSelectedEntryIndex() >= 0 && getSelectedEntryIndex() < entries.size() - 1);
  }


  private void buildHeader() {

    final JLabel propertyNameHeader = new JLabel(Resources.getString("Editor.ParameterListConfigurer.dynamic_property_name"));
    final Font boldFont = new Font(propertyNameHeader.getFont().getFontName(), Font.BOLD, propertyNameHeader.getFont().getSize());
    propertyNameHeader.setFont(boldFont);
    configControls.add(propertyNameHeader, "alignx center"); // NON-NLS

    final JLabel valueHeader  = new JLabel(Resources.getString("Editor.ParameterListConfigurer.value"));
    valueHeader.setFont(boldFont);
    configControls.add(valueHeader, "alignx center,wrap"); // NON-NLS

    configControls.setVisible(false);

  }

  /**
   * Append an Entry's controls to the bottom of the panel
   */
  private void appendConfigControls(ParameterEntry entry) {
    final ParameterConfigurer c = entry.getParameterConfigurer();
    getControls();
    configControls.add(c.getPropertyNameControls());
    configControls.add(c.getValueControls());
    configControls.add(entry.getRemoveButton());
  }

  /**
   * Append an Entry's controls after the currently selected entry
   */
  private void insertConfigControls(ParameterEntry entry, int listPos) {

    int controlPos = listPosToControlsPos(listPos);
    final ParameterConfigurer c = entry.getParameterConfigurer();
    getControls();
    configControls.add(c.getPropertyNameControls(), controlPos++);
    configControls.add(c.getValueControls(), controlPos++);
    configControls.add(entry.getRemoveButton(), controlPos++);

  }

  /**
   * Remove an Entry's controls from the specified position
   */
  private void removeConfigControls(ParameterEntry entry) {
    final ParameterConfigurer c = entry.getParameterConfigurer();
    getControls();
    configControls.remove(c.getPropertyNameControls());
    configControls.remove(c.getValueControls());
    configControls.remove(entry.getRemoveButton());
  }

  /**
   * Find the position of the first control for the specified list Entry
   */
  private int listPosToControlsPos(int listPos) {
    return HEADER_COMPONENT_COUNT + listPos * COMPONENT_COUNT;
  }

  @Override
  public void repack() {
    repack(panel);
  }


  private static class ParameterEntry extends AbstractConfigurableListEntry {

    private final ParameterListConfigurer listConfig;

    public ParameterEntry(ParameterListConfigurer listConfig) {
      this(listConfig, null);
    }

    public ParameterEntry(ParameterListConfigurer listConfig, Object value) {
      this(listConfig, value, ConfigurableList.DEFAULT_ICON_SIZE);
    }

    public ParameterEntry(ParameterListConfigurer listConfig, Object value, int iconSize) {
      super(listConfig, value, iconSize);
      this.listConfig = listConfig;
    }

    public ParameterConfigurer getParameterConfigurer() {
      return (ParameterConfigurer) getConfigurer();
    }

    @Override
    public Configurer buildChildConfigurer(Object value) {
      final Configurer c = new ParameterConfigurer();
      c.setFrozen(true);
      c.setValue(value);
      c.setFrozen(false);
      return c;
    }

    @Override
    public void focusGained() {
      listConfig.selectEntry(this);
    }

    @Override
    public void setHighlighted(boolean highlighted) {
      getConfigurer().setHighlighted(highlighted);
    }

  }
}
