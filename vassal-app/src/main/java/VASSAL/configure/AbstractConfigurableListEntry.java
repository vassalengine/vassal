package VASSAL.configure;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import java.beans.PropertyChangeEvent;
import javax.swing.JButton;

/**
 * Base class for a List Entry maintained by a {@link ConfigurableList}
 *
 * A {@link ConfigurableListEntry} must be configurered by a single Configurer.
 */
public abstract class AbstractConfigurableListEntry implements ConfigurableListEntry {

  /** The Parent ConfigurableList that owns this entry  */
  private final ConfigurableList parent;
  /** A button to remove this entry */
  private JButton removeButton;
  /** The Configurer that Configures the data maintained by this entry */
  private Configurer configurer;
  /** A holder for the initial value to set the Configurer when it is eventually lazily built */
  private final Object savedValue;
  /** Icon Size for remove button */
  private final int iconSize;

  /**
   * Build a new List Entry
   *
   * @param parentConfig Parent List
   * @param value Initial value for entry
   * @param iconSize Icon size for the Remove button
   */
  public AbstractConfigurableListEntry(ConfigurableList parentConfig, Object value, int iconSize) {
    this.parent = parentConfig;
    this.savedValue = value;
    this.iconSize = iconSize;

  }

  public AbstractConfigurableListEntry(ConfigurableList parentConfig, Object value) {
    this(parentConfig, value, ConfigurableList.DEFAULT_ICON_SIZE);
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    parent.entryChanged(this);
    updateVisibility();
  }

  /**
   * Build a suitable Configurer that Configures this List Entry
   *
   * @param value Initial value for the Configurer
   * @return A Configurer
   */
  public abstract Configurer buildChildConfigurer(Object value);

  /**
   * Return the parent ConfigurableList that owns this Entry.
   *
   * @return Parent List
   */
  public ConfigurableList getParent() {
    return parent;
  }

  /**
   * Return the Remove button that deletes this List Entry
   *
   * @return Remove Button
   */
  @Override
  public JButton getRemoveButton() {
    if (removeButton == null) {
      removeButton = new NoInsetButton("no", iconSize, "Editor.ConfigurableListEntryController.remove_button_tip"); // NON-NLS
      removeButton.addActionListener(e -> parent.deleteEntry(this));
    }
    return removeButton;
  }

  /**
   * Return the Configurer that configures this entry
   *
   * @return Entry Configurer
   */
  @Override
  public Configurer getConfigurer() {
    if (configurer == null) {
      configurer = buildChildConfigurer(savedValue);
      configurer.addPropertyChangeListener(e -> updateVisibility());
      addPropertyChangeListener(this);

      final FocusListener fl = new FocusListener() {
        @Override
        public void focusGained(FocusEvent e) {
          AbstractConfigurableListEntry.this.focusGained();
        }

        @Override
        public void focusLost(FocusEvent e) {
        }
      };
      configurer.addFocusListener(fl);
    }
    return configurer;
  }

  /**
   * Called when this Entry gains the focus.
   */
  @Override
  public void focusGained() {
    getParent().selectEntry(this);
  }

  /**
   * Ensure this entry has the focus
   */
  @Override
  public  void requestFocus() {
    getConfigurer().requestFocus();
  }

  /**
   * Update the visibility of any optional components in this entry
   * Most Configurer entries will not have option components.
   */
  @Override
  public void updateVisibility() {

  }

  /**
   * Set the Highlighted status of fields in this entry.
   * Used to highlight the currently selected entry.
   *
   * @param b Highlight status
   */
  @Override
  public abstract void setHighlighted(boolean b);

}
