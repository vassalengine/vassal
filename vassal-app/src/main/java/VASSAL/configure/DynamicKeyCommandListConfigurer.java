/*
 *
 * Copyright (c) 2020 by The VASSAL Development Team
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
import VASSAL.tools.icon.IconFamily;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

/**
 * A customised ListConfigurer for Lists of DynamicKeyCommands
 * Use a tabular layout format to reduce screen real estate requirements
 *
 * The method of inserting/removing components from the Layout instead of just rebuilding it each time the
 * entries are changed is needed due to the complexities of the Layout resulting in ugly refresh flashing
 * as the layout increases in size.
 *
 */
public class DynamicKeyCommandListConfigurer extends Configurer implements PropertyChangeListener, ConfigurableList {

  // The number of Components added to the header of the Controls panel
  private static final int HEADER_COMPONENT_COUNT = 4;
  // The number of Components added to the Controls panel for each Entry
  private static final int COMPONENT_COUNT = 6;

  private static final int CONTROLLER_ICON_SIZE = IconFamily.XSMALL;

  private JPanel controls;
  private JPanel configControls;
  private JPanel panel;
  private final List<ConfigurableListEntry> entries = new ArrayList<>();
  private DynamicProperty target;
  private ConfigurableListController controller;
  private int selectedEntryIndex = -1;
  private JPanel emptyPanel;

  public DynamicKeyCommandListConfigurer(String key, String name, DynamicProperty target) {
    super(key, name);
    setTarget(target);
  }

  public DynamicProperty getTarget() {
    return target;
  }

  public void setTarget(DynamicProperty target) {
    this.target = target;
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

  protected void updateValue() {
    noUpdate = true;
    final List<Object> newArray = new ArrayList<>();
    for (final ConfigurableListEntry entry : entries) {
      newArray.add(entry.getConfigurer().getValue());
    }
    setValue(newArray);
    noUpdate = false;
  }

  @Override
  public void setValue(Object o) {
    if (o == null) {
      o = new ArrayList<>();
    }
    super.setValue(o);
    if (!noUpdate) {
      rebuildControls();
    }
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel(new MigLayout(TraitLayout.STANDARD_GAPY, "[grow,fill]", "[grow,fill]")); // NON-NLS
      panel.setBorder(BorderFactory.createEtchedBorder());
      controls = new JPanel(new MigLayout("hidemode 3,ins 2", "[grow,fill]", "[grow,fill]")); // NON-NLS

      configControls = new JPanel(new MigLayout("hidemode 3," + ConfigurerLayout.STANDARD_INSERTS_GAPY, "[grow,fill]rel[grow,fill]rel[]rel[grow,fill]rel[]", "[center]")); // NON-NLS

      emptyPanel = new JPanel();
      emptyPanel.setMinimumSize(new Dimension(20, 20));
      controls.add(emptyPanel, "grow,push"); // NON-NLS
      controls.add(configControls, "grow, aligny center"); // NON-NLS
      panel.add(controls, "grow"); // NON-NLS

      rebuildControls();
    }
    return panel;
  }

  @Override
  public JPanel getListController() {
    if (controller == null) {
      controller = new ConfigurableListController(this, CONTROLLER_ICON_SIZE);
    }
    return controller;
  }

  private void updateListController() {
    getListController();
    controller.setCanMoveUp(getSelectedEntryIndex() > 0);
    controller.setCanMoveDown(getSelectedEntryIndex() < entries.size() - 1);
  }

  @Override
  public void selectEntry(ConfigurableListEntry entry) {
    setSelectedEntryIndex(entries.indexOf(entry));
    updateControls();
  }

  @SuppressWarnings("unchecked")
  public List<Object> getListValue() {
    return (List<Object>) getValue();
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    updateValue();
  }

  /**
   * Rebuild the Configurer Controls from scratch
   */
  protected void rebuildControls() {
    if (controls != null) {
      // Remove any existing Listeners
      for (final ConfigurableListEntry entry : entries) {
        entry.removePropertyChangeListener(this);
      }

      // Remove all entries and Controls
      entries.clear();
      configControls.removeAll();

      // Build a new header
      buildHeader();

      // Create a new set of entries and add the controls
      for (final Object value : getListValue()) {
        final DKCEntry entry = new DKCEntry(this, value, CONTROLLER_ICON_SIZE);
        entry.addPropertyChangeListener(this);
        entries.add(entry);
        appendConfigControls(entry);
      }

      updateControls();
      repack();
    }
  }

  @Override
  public void setSelectedEntryIndex(int index) {
    selectedEntryIndex = index;
    updateListController();
  }

  @Override
  public int getSelectedEntryIndex() {
    return selectedEntryIndex;
  }

  /**
   * Append an Entry's controls to the bottom of the panel
   */
  private void appendConfigControls(DKCEntry entry) {
    final DynamicKeyCommandConfigurer c = entry.getDkcConfigurer();

    configControls.add(c.getCommandControls(), "growx"); // NON-NLS
    configControls.add(c.getKeyControls(), "growx"); // NON-NLS
    configControls.add(c.getTypeControls(), "growx"); // NON-NLS
    configControls.add(entry.getPromptPanel(), "growx"); // NON-NLS
    configControls.add(entry.getRemoveButton(), "growx 0,wrap"); // NON-NLS
    configControls.add(c.getValuesControls(), "span 4,grow,wrap"); // NON-NLS

  }

  /**
   * Insert an Entry's controls at the specified position
   */
  private void insertConfigControls(DKCEntry entry, int listPos) {

    int controlPos = listPosToControlsPos(listPos);
    final DynamicKeyCommandConfigurer c = entry.getDkcConfigurer();

    configControls.add(c.getCommandControls(), "growx", controlPos++); // NON-NLS
    configControls.add(c.getKeyControls(), "growx", controlPos++); // NON-NLS
    configControls.add(c.getTypeControls(), "growx 0", controlPos++); // NON-NLS
    configControls.add(entry.getPromptPanel(), "growx", controlPos++); // NON-NLS
    configControls.add(entry.getRemoveButton(), "growx 0,wrap", controlPos++); // NON-NLS
    configControls.add(c.getValuesControls(), "span 4,grow,wrap", controlPos); // NON-NLS

  }

  /**
   * Remove an Entry's controls from the specified position
   */
  private void removeConfigControls(DKCEntry entry) {
    final DynamicKeyCommandConfigurer c = entry.getDkcConfigurer();

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

  /**
   * Update visibility, enabled state as required
   */
  private void updateControls() {

    // If there are entries, but none selected, select the first one.
    if (getSelectedEntryIndex() < 0 && entries.size() > 0) {
      setSelectedEntryIndex(0);
    }

    configControls.setVisible(getListValue().size() > 0);
    emptyPanel.setVisible(getListValue().size() == 0);

    for (int i = 0; i < entries.size(); i++) {
      entries.get(i).updateVisibility();
      entries.get(i).setHighlighted(i == getSelectedEntryIndex());
    }

    configControls.revalidate();
    repack();
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

  // Move currentl selected entry up
  @Override
  public void moveEntryUp() {
    if (getSelectedEntryIndex() < 0) {
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

    // Remove the item in the value list from its current position on the list and re-insert one position up
    updateValue();

    setSelectedEntryIndex(listPos - 1);

    updateControls();

  }

  // Move currently selected entry down
  @Override
  public void moveEntryDown() {
    if (getSelectedEntryIndex() < 0) {
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

    // Move the selection down 1 to follow the moved entry
    setSelectedEntryIndex(getSelectedEntryIndex() + 1);

    updateValue();
    updateControls();
  }

  /**
   * Add a new entry after the currently selected entry, or at the end if none selected
   */
  @Override
  public void addEntry() {

    // Find the position of currently selected entry. The new entry will be created below it.
    final int pos = getSelectedEntryIndex();

    // Create a new empty entry
    final DKCEntry newEntry = new DKCEntry(this, CONTROLLER_ICON_SIZE);

    // Insert the new entry into the list at the appropriate place
    if (entries.isEmpty() || getSelectedEntryIndex() < 0) {
      entries.add(newEntry);
      updateValue();
      appendConfigControls(newEntry);
      setSelectedEntryIndex(entries.size() - 1);
    }
    else {
      entries.add(pos + 1, newEntry);
      updateValue();
      newEntry.adjustPreferredSize((DKCEntry) entries.get(0)); // Ensure the inserted components respect the existing preferred size
      insertConfigControls(newEntry, pos + 1);
      setSelectedEntryIndex(pos + 1);
    }

    updateControls();

  }

  // An Entry has had its Remove button clicked
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
    updateValue();
    updateControls();
  }

  @Override
  public void repack() {
    repack(panel);
  }

  @Override
  public int hashCode() {
    final String valueString = getValueString();
    final int prime = 31;
    int result = 1;
    result = prime * result
      + ((valueString == null) ? 0 : valueString.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    final DynamicKeyCommandListConfigurer other = (DynamicKeyCommandListConfigurer) obj;
    final String valueString = getValueString();
    final String otherValueString = other.getValueString();
    if (valueString == null) {
      return otherValueString == null;
    }
    else return valueString.equals(otherValueString);
  }

  private static class DKCEntry extends AbstractConfigurableListEntry {

    private final DynamicKeyCommandListConfigurer listConfig;
    private NoInsetButton showHideValuesButton;
    private final JPanel promptPanel;

    public DKCEntry(DynamicKeyCommandListConfigurer listConfig, Object value, int iconSize) {
      super(listConfig, value, iconSize);
      this.listConfig = listConfig;

      getDkcConfigurer().getValuesControls().setVisible(false);
      addPropertyChangeListener(e -> updateVisibility());

      promptPanel = new JPanel(new MigLayout("ins 0,hidemode 3", "[grow,fill]2[]")); // NON-NLS
      promptPanel.add(getDkcConfigurer().getChangerControls(), "growx,aligny center"); // NON-NLS
      promptPanel.add(getShowHideValuesButton(), "aligny center"); // NON-NLS

      addPropertyChangeListener(listConfig);
    }

    @Override
    public Configurer buildChildConfigurer(Object value) {
      final Configurer c = new DynamicKeyCommandConfigurer(listConfig.getTarget());
      c.setFrozen(true);
      c.setValue(value);
      c.setFrozen(false);
      return c;
    }

    private DynamicKeyCommandConfigurer getDkcConfigurer() {
      return (DynamicKeyCommandConfigurer) getConfigurer();
    }

    @Override
    public void focusGained() {
      listConfig.selectEntry(this);
    }

    public DKCEntry(DynamicKeyCommandListConfigurer listConfig, int iconSize) {
      this(listConfig, null, iconSize);
    }

    public JPanel getPromptPanel() {
      return promptPanel;
    }

    // Update the preferred size to prevent inserted entries forcing the display wider (MigLayout funny when inserting components)
    public void adjustPreferredSize(DKCEntry e) {
      getDkcConfigurer().getCommandControls().setPreferredSize(e.getDkcConfigurer().getCommandControls().getPreferredSize());
      getDkcConfigurer().getKeyControls().setPreferredSize(e.getDkcConfigurer().getKeyControls().getPreferredSize());
      getDkcConfigurer().getTypeControls().setPreferredSize(e.getDkcConfigurer().getTypeControls().getPreferredSize());
      getDkcConfigurer().getChangerControls().setPreferredSize(e.getDkcConfigurer().getChangerControls().getPreferredSize());
      getDkcConfigurer().getValuesControls().setPreferredSize(e.getDkcConfigurer().getValuesControls().getPreferredSize());
    }

    private void showHideValues() {
      getDkcConfigurer().getValuesControls().setVisible(!getDkcConfigurer().getValuesControls().isVisible());
      getParent().repack();
    }

    public JButton getShowHideValuesButton() {
      if (showHideValuesButton == null) {
        showHideValuesButton = new NoInsetButton("edit-find", CONTROLLER_ICON_SIZE, "Editor.PropertyChangeConfigurer.showHide_hint"); // NON-NLS
        showHideValuesButton.setVisible(false);
        showHideValuesButton.addActionListener(e -> showHideValues());
      }
      return showHideValuesButton;
    }

    @Override
    public void updateVisibility() {
      getShowHideValuesButton().setVisible(getDkcConfigurer().isEnumType());
      if (!getDkcConfigurer().isEnumType()) {
        getDkcConfigurer().getValuesControls().setVisible(false);
      }
      getParent().repack();
    }

    @Override
    public void setHighlighted(boolean highlighted) {
      getConfigurer().setHighlighted(highlighted);
    }

  }
}
