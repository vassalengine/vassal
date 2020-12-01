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

import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import java.awt.Component;
import java.awt.Font;
import java.awt.Insets;
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
 * A customise ListConfigurer for Lists of DynamicKeyCommands
 * Use a tabular layout format to reduce screen real estate requirements
 */
public class DynamicKeyCommandListConfigurer extends Configurer implements PropertyChangeListener {

  // The number of Components added to the header of the Controls panel
  private static final int HEADER_COMPONENT_COUNT = 4;

  private static final int ICON_SIZE = IconFamily.XSMALL;

  private JPanel controls;
  private JPanel configControls;
  private JPanel panel;
  private final List<DKCEntry> entries = new ArrayList<>();
  private DynamicProperty target;
  private JButton initialAddButton;


  public DynamicKeyCommandListConfigurer(String key, String name) {
    super(key, name);
  }

  public DynamicKeyCommandListConfigurer(String key, String name, List<?> val) {
    super(key, name, val);
  }

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
    final DynamicKeyCommandConfigurer c = buildChildConfigurer();
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
      final Configurer c = buildChildConfigurer();
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      while (sd.hasMoreTokens()) {
        c.setValue(sd.nextToken());
        getListValue().add(c.getValue());
      }
    }
    rebuildControls();
  }

  protected void updateValue() {
    noUpdate = true;
    final ArrayList<Object> newArray = new ArrayList<>();
    for (final DKCEntry entry : entries) {
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
      panel = new JPanel(new MigLayout(TraitLayout.STANDARD_GAPY, "[grow,fill]", "[]")); // NON-NLS
      panel.setBorder(BorderFactory.createEtchedBorder());
      controls = new JPanel(new MigLayout("hidemode 3,ins 2", "[grow,fill]")); // NON-NLS

      configControls = new JPanel(new MigLayout("hidemode 3," + ConfigurerLayout.STANDARD_INSERTS_GAPY, "[grow,fill]rel[grow,fill]rel[]rel[grow,fill]rel[]", "[center]")); // NON-NLS

      initialAddButton = new NoInsetButton("add", IconFamily.SMALL); // NON-NLS
      initialAddButton.addActionListener(e -> {
        addEntry();
      });

      controls.add(initialAddButton, "growx 0,alignx center,wrap"); // NON-NLS
      controls.add(configControls, "grow"); // NON-NLS
      panel.add(controls, "grow"); // NON-NLS
      rebuildControls();
    }
    return panel;
  }

  @SuppressWarnings("unchecked")
  public List<Object> getListValue() {
    return (List<Object>) getValue();
  }

  /**
   * The objects in the list are specified by the Configurer returned here
   *
   * @return objects in the list
   */
  protected DynamicKeyCommandConfigurer buildChildConfigurer() {
    final DynamicKeyCommandConfigurer c = new DynamicKeyCommandConfigurer(getTarget());
    c.addPropertyChangeListener(e -> repack());
    return c;
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
      for (final DKCEntry entry : entries) {
        entry.removePropertyChangeListener(this);
      }

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

  /**
   * Append an Entry's controls to the bottom of the panel
   */
  private void appendConfigControls(DKCEntry entry) {
    final DynamicKeyCommandConfigurer c = entry.getConfigurer();

    configControls.add(c.getCommandControls(), "growx"); // NON-NLS
    configControls.add(c.getKeyControls(), "growx"); // NON-NLS
    configControls.add(c.getTypeControls(), "growx 0"); // NON-NLS
    configControls.add(c.getChangerControls(), "growx"); // NON-NLS
    configControls.add(entry.getButtons(), "growx 0,wrap"); // NON-NLS
    configControls.add(c.getValuesControls(), "span 4,grow,wrap"); // NON-NLS

  }

  /**
   * Insert an Entry's controls at the specified position
   */
  private void insertConfigControls(DKCEntry entry, int listPos) {

    int controlPos = listPosToControlsPos(listPos);
    final DynamicKeyCommandConfigurer c = entry.getConfigurer();

    configControls.add(c.getCommandControls(), "growx", controlPos++); // NON-NLS
    configControls.add(c.getKeyControls(), "growx", controlPos++); // NON-NLS
    configControls.add(c.getTypeControls(), "growx 0", controlPos++); // NON-NLS
    configControls.add(c.getChangerControls(), "growx", controlPos++); // NON-NLS
    configControls.add(entry.getButtons(), "growx 0,wrap", controlPos++); // NON-NLS
    configControls.add(c.getValuesControls(), "span 4,grow,wrap", controlPos); // NON-NLS

  }

  /**
   * Remove an Entry's controls from the specified position
   */
  private void removeConfigControls(DKCEntry entry) {
    final DynamicKeyCommandConfigurer c = entry.getConfigurer();

    configControls.remove(c.getCommandControls());
    configControls.remove(c.getKeyControls());
    configControls.remove(c.getTypeControls());
    configControls.remove(c.getChangerControls());
    configControls.remove(entry.getButtons());
    configControls.remove(c.getValuesControls());

  }

  /**
   * Find the position of the first control for the specified list Entry
   */
  private int listPosToControlsPos(int listPos) {
    return HEADER_COMPONENT_COUNT + listPos * DKCEntry.COMPONENT_COUNT;
  }

  /**
   * Update visibility, enabled state as required
   */
  private void updateControls() {

    configControls.setVisible(getListValue().size() > 0);
    initialAddButton.setVisible(getListValue().size() == 0);

    for (int i = 0; i < entries.size(); i++) {
      entries.get(i).setTop(i == 0);
      entries.get(i).setBottom(i >= (entries.size() - 1));
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

  // An Entry has had its Up button clicked
  private void moveUp(DKCEntry entry) {
    final int listPos = entries.indexOf(entry);

    // Remove the Configurer controls from their current position and re-insert one position up
    removeConfigControls(entry);
    insertConfigControls(entry, listPos - 1);

    // Remove the entry from its current position on the list and re-insert one position up
    entries.remove(entry);
    entries.add(listPos - 1, entry);

    // Remove the item in the value list from its current position on the list and re-insert one position up
    getListValue().remove(listPos);
    getListValue().add(listPos - 1, entry.getConfigurer());

    updateControls();

  }

  // An Entry has had its Down button clicked
  private void moveDown(DKCEntry entry) {
    final int listPos = entries.indexOf(entry);

    // Remove the Configurer controls from their current position and re-insert one position down
    removeConfigControls(entry);
    insertConfigControls(entry, listPos + 1);

    // Remove the entry from its current position on the list and re-insert one position down
    entries.remove(entry);
    entries.add(listPos + 1, entry);

    // Remove the item in the value list from its current position on the list and re-insert one position down
    getListValue().remove(listPos);
    getListValue().add(listPos + 1, entry.getConfigurer());

    updateControls();
  }

  // Add the first entry
  private void addEntry() {
    final DKCEntry entry = new DKCEntry(this);
    entries.add(entry);
    getListValue().add(entry.getConfigurer());
    appendConfigControls(entry);
    updateControls();
    repack();
  }

  // An Entry has had its Add button clicked
  private void addEntry(DKCEntry entry) {

    // Find the position of the entry that was clicked. The new entry will be created below it.
    final int pos = entries.indexOf(entry);

    // Create a new empty entry
    final DKCEntry newEntry = new DKCEntry(this);

    // Insert the new entry into the list at the appropriate place
    if (pos < entries.size() - 1) {
      entries.add(pos + 1, newEntry);
    }
    else {
      entries.add(newEntry);
    }

    // Insert the Configurer components into the configsPanel at the appropriate place.
    // Rebuilding the entire panel for every insert causes unsightly flashing.
    if (pos < entries.size()) {
      // Ensure the inserted components respect the existing preferred size
      newEntry.adjustPreferredSize(entries.get(0));
      insertConfigControls(newEntry, pos + 1);
    }
    else {
      appendConfigControls(newEntry);
    }

    updateControls();

  }

  // An Entry has had its Remove button clicked
  private void removeEntry(DKCEntry entry) {
    final int listPos = entries.indexOf(entry);
    final int componentPos = listPosToControlsPos(listPos);
    for (int i = 0; i < DKCEntry.COMPONENT_COUNT; i++) {
      configControls.remove(componentPos);
    }
    entries.remove(entry);
    getListValue().remove(listPos);
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

  private static class DKCEntry {

    // The number of Components added to the Controls panel for each Entry
    private static final int COMPONENT_COUNT = 6;

    private final DynamicKeyCommandConfigurer configurer;
    private DynamicKeyCommandListConfigurer listConfig;
    private final JPanel buttonPanel = new JPanel(new MigLayout("ins 0", "[]rel[]rel[]rel[]")); // NON-NLS
    private final JButton upButton = new NoInsetButton("go-up", ICON_SIZE); // NON-NLS
    private final JButton dnButton = new NoInsetButton("go-down", ICON_SIZE); // NON-NLS

    public DKCEntry(DynamicKeyCommandListConfigurer listConfig, Object value) {
      setListConfig(listConfig);
      configurer = new DynamicKeyCommandConfigurer(listConfig.getTarget());

      configurer.setValue(value);
      configurer.addPropertyChangeListener(listConfig);

      upButton.addActionListener(e -> getListConfig().moveUp(this));
      buttonPanel.add(upButton);

      dnButton.addActionListener(e -> getListConfig().moveDown(this));
      buttonPanel.add(dnButton);

      final JButton addButton = new NoInsetButton("add", ICON_SIZE); // NON-NLS
      addButton.addActionListener(e -> getListConfig().addEntry(this));
      buttonPanel.add(addButton);

      final JButton delButton = new NoInsetButton("remove", ICON_SIZE); // NON-NLS
      delButton.addActionListener(e -> getListConfig().removeEntry(this));
      buttonPanel.add(delButton);
    }

    public DKCEntry(DynamicKeyCommandListConfigurer listConfig) {
      this(listConfig, null);
    }

    public JPanel getButtons() {
      return buttonPanel;
    }

    public DynamicKeyCommandConfigurer getConfigurer() {
      return configurer;
    }

    public DynamicKeyCommandListConfigurer getListConfig() {
      return listConfig;
    }

    public void setListConfig(DynamicKeyCommandListConfigurer listConfig) {
      this.listConfig = listConfig;
    }

    public void setTop(boolean isTop) {
      upButton.setEnabled(!isTop);
    }

    public void setBottom(boolean isBottom) {
      dnButton.setEnabled(!isBottom);
    }

    public void setPropertyChangeListener(PropertyChangeListener l) {
      getConfigurer().addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
      getConfigurer().removePropertyChangeListener(l);
    }

    // Update the preferred size to prevent inserted entries forcing the display wider
    public void adjustPreferredSize(DKCEntry e) {
      configurer.getCommandControls().setPreferredSize(e.getConfigurer().getCommandControls().getPreferredSize());
      configurer.getKeyControls().setPreferredSize(e.getConfigurer().getKeyControls().getPreferredSize());
      configurer.getTypeControls().setPreferredSize(e.getConfigurer().getTypeControls().getPreferredSize());
      configurer.getChangerControls().setPreferredSize(e.getConfigurer().getChangerControls().getPreferredSize());
      configurer.getValuesControls().setPreferredSize(e.getConfigurer().getValuesControls().getPreferredSize());
    }
  }

  private static class NoInsetButton extends JButton {
    private static final long serialVersionUID = 1L;
    final Insets NO_INSETS = new Insets(0, 0, 0, 0);
    final Font ITALIC = new Font(Font.DIALOG, Font.ITALIC, 12);

    public NoInsetButton(String icon, int size) {
      this(icon, size, null);
    }

    public NoInsetButton(String icon, int size, String toolTipKey) {
      super(IconFactory.getIcon(icon, size));
      setFont(ITALIC);
      setMargin(NO_INSETS);
      if (toolTipKey != null && !toolTipKey.isEmpty()) {
        setToolTipText(Resources.getString(toolTipKey));
      }
    }
  }
}
