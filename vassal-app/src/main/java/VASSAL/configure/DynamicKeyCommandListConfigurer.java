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

import VASSAL.counters.DynamicProperty;
import VASSAL.counters.TraitLayout;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

public class DynamicKeyCommandListConfigurer extends Configurer implements ConfigurableList {

  // The number of Components added to the header of the Controls panel
  private static final int HEADER_COMPONENT_COUNT = 4;
  // The number of Components added to the Controls panel for each Entry
  private static final int COMPONENT_COUNT = 6;

  private DynamicProperty target;
  private ConfigurableListController controller;
  private int selectedEntryIndex = -1;
  private JPanel panel;
  private JPanel controls;
  private JPanel configControls;
  private final List<ConfigurableListEntry> entries = new ArrayList<>();

  public DynamicKeyCommandListConfigurer(String key, String name,  DynamicProperty target) {
    super(key, name);
    setTarget(target);
    value = new ArrayList<>(0);
  }

  public DynamicProperty getTarget() {
    return target;
  }

  public void setTarget(DynamicProperty target) {
    this.target = target;
  }

  @Override
  public void moveEntryUp() {
    final int pos = getSelectedEntryIndex();
    if (pos < 0 || pos > entries.size() - 1) {
      return;
    }

    final int listPos = getSelectedEntryIndex();
    final DKCEntry entry = (DKCEntry) entries.get(listPos);

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
    final DKCEntry entry = (DKCEntry) entries.get(listPos);

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
    final DKCEntry newEntry = new DKCEntry(this);

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
    if (getListValue().isEmpty()) {
      return "";
    }
    final DynamicKeyCommandConfigurer c = new DynamicKeyCommandConfigurer(getTarget());
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final Object value : getListValue()) {
      c.setFrozen(true); // Prevent subsidiary Configurers from firing PropertyChange Events
      c.setValue(value);
      c.setFrozen(false);
      se.append(c.getValueString());
    }
    return se.getValue();
  }

  public List<Object> getListValue() {
    return (List<Object>) getValue();
  }

  @Override
  public void setValue(String s) {
    getListValue().clear();
    if (s.length() > 0) {
      final Configurer c = new DynamicKeyCommandConfigurer(getTarget());
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      while (sd.hasMoreTokens()) {
        c.setFrozen(true);
        c.setValue(sd.nextToken());
        c.setFrozen(false);
        getListValue().add(c.getValue());
      }
    }
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

      configControls = new JPanel(new MigLayout("hidemode 3," + ConfigurerLayout.STANDARD_INSERTS_GAPY, "[grow,fill]rel[grow,fill]rel[]rel[grow,fill]rel[]", "[center]")); // NON-NLS

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
        final DKCEntry entry = new DKCEntry(this, value);
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

    configControls.setVisible(getListValue().size() > 0);

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

    final JLabel commandHeader = new JLabel(Resources.getString("Editor.menu_command"));
    final Font boldFont = new Font(commandHeader.getFont().getFontName(), Font.BOLD, commandHeader.getFont().getSize());
    commandHeader.setFont(boldFont);
    configControls.add(commandHeader, "growx 0,alignx center"); // NON-NLS

    final JLabel keyHeader  = new JLabel(Resources.getString("Editor.keyboard_command"));
    keyHeader.setFont(boldFont);
    configControls.add(keyHeader, "growx 0,alignx center"); // NON-NLS

    final JLabel typeLabel = new JLabel(Resources.getString("Editor.PropertyChangeConfigurer.type"));
    typeLabel.setFont(boldFont);
    configControls.add(typeLabel, "growx 0,alignx center"); // NON-NLS

    final JLabel promptLabel = new JLabel(Resources.getString("Editor.PropertyChangeConfigurer.prompt"));
    promptLabel.setFont(boldFont);
    configControls.add(promptLabel, "growx 0,alignx center,wrap"); // NON-NLS

    configControls.setVisible(false);

  }

  /**
   * Append an Entry's controls to the bottom of the panel
   */
  private void appendConfigControls(DKCEntry entry) {
    final DynamicKeyCommandConfigurer c = entry.getDkcConfigurer();
    getControls();
    configControls.add(c.getCommandControls(), "growx"); // NON-NLS
    configControls.add(c.getKeyControls(), "growx"); // NON-NLS
    configControls.add(c.getTypeControls(), "growx"); // NON-NLS
    configControls.add(entry.getPromptPanel(), "growx"); // NON-NLS
    configControls.add(entry.getRemoveButton(), "growx 0,wrap"); // NON-NLS
    configControls.add(c.getValuesControls(), "skip 2,span 2,grow,wrap"); // NON-NLS

  }

  /**
   * Append an Entry's controls after the currently selected entry
   */
  private void insertConfigControls(DKCEntry entry, int listPos) {

    int controlPos = listPosToControlsPos(listPos);
    final DynamicKeyCommandConfigurer c = entry.getDkcConfigurer();
    getControls();
    configControls.add(c.getCommandControls(), "growx", controlPos++); // NON-NLS
    configControls.add(c.getKeyControls(), "growx", controlPos++); // NON-NLS
    configControls.add(c.getTypeControls(), "growx 0", controlPos++); // NON-NLS
    configControls.add(entry.getPromptPanel(), "growx", controlPos++); // NON-NLS
    configControls.add(entry.getRemoveButton(), "growx 0,wrap", controlPos++); // NON-NLS
    configControls.add(c.getValuesControls(), "skip 2,span 2,grow,wrap", controlPos); // NON-NLS

  }

  /**
   * Remove an Entry's controls from the specified position
   */
  private void removeConfigControls(DKCEntry entry) {
    final DynamicKeyCommandConfigurer c = entry.getDkcConfigurer();
    getControls();
    configControls.remove(c.getCommandControls());
    configControls.remove(c.getKeyControls());
    configControls.remove(c.getTypeControls());
    configControls.remove(entry.getPromptPanel());
    configControls.remove(entry.getRemoveButton());
    configControls.remove(c.getValuesControls());

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


  private static class DKCEntry extends AbstractConfigurableListEntry {

    private final DynamicKeyCommandListConfigurer listConfig;
    private NoInsetButton showHideValuesButton;
    private JPanel promptPanel;
    private boolean valuesShowing = false;

    public DKCEntry(DynamicKeyCommandListConfigurer listConfig) {
      this(listConfig, null);
    }

    public DKCEntry(DynamicKeyCommandListConfigurer listConfig, Object value) {
      this(listConfig, value, ConfigurableList.DEFAULT_ICON_SIZE);
    }

    public DKCEntry(DynamicKeyCommandListConfigurer listConfig, Object value, int iconSize) {
      super(listConfig, value, iconSize);
      this.listConfig = listConfig;
    }

    private DynamicKeyCommandConfigurer getDkcConfigurer() {
      return (DynamicKeyCommandConfigurer) getConfigurer();
    }

    @Override
    public Configurer buildChildConfigurer(Object value) {
      final Configurer c = new DynamicKeyCommandConfigurer(listConfig.getTarget());
      c.setFrozen(true);
      c.setValue(value);
      c.setFrozen(false);
      return c;
    }

    public JPanel getPromptPanel() {
      if (promptPanel == null) {
        getDkcConfigurer().getValuesControls().setVisible(false);
        promptPanel = new JPanel(new MigLayout("ins 0,hidemode 3", "[grow,fill]2[]")); // NON-NLS
        promptPanel.add(getDkcConfigurer().getChangerControls(), "growx,aligny center"); // NON-NLS
        promptPanel.add(getShowHideValuesButton(), "aligny center"); // NON-NLS
      }
      return promptPanel;
    }

    @Override
    public void focusGained() {
      listConfig.selectEntry(this);
    }

    @Override
    public void setHighlighted(boolean highlighted) {
      getConfigurer().setHighlighted(highlighted);
    }

    private void showHideValues() {
      valuesShowing = !valuesShowing;
      getDkcConfigurer().getValuesControls().setVisible(valuesShowing);
      getParent().repack();
    }

    public JButton getShowHideValuesButton() {
      if (showHideValuesButton == null) {
        showHideValuesButton = new NoInsetButton("edit-find", ConfigurableList.DEFAULT_ICON_SIZE, "Editor.PropertyChangeConfigurer.showHide_hint"); // NON-NLS
        showHideValuesButton.setVisible(false);
        showHideValuesButton.addActionListener(e -> showHideValues());
      }
      return showHideValuesButton;
    }

    @Override
    public void updateVisibility() {
      final boolean show = getDkcConfigurer().isEnumType();

      getShowHideValuesButton().setVisible(show);
      getDkcConfigurer().getValuesControls().setVisible(show && valuesShowing);

      getParent().repack();
    }
  }
}