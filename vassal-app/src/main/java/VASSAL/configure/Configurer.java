/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.SwingUtils;

import javax.swing.JDialog;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

/**
 * A property editor class.  Wraps an Object value and provides
 * methods for saving and restoring the Object from a String.  Also
 * includes a {@link java.awt.Component} that can be placed into a
 * property editing window, allowing the user to change the value
 * interactively.
 * */
public abstract class Configurer {
// FIXME: maybe parameterize this so that value can have the right type
// in subclasses?
  public static final String NAME_PROPERTY = "Configurer.name";  //NON-NLS
  //    public static final String VALUE_PROPERTY = "value";

  /** A String the uniquely identifies this property */
  protected String key;
  /** A String that provides a short description of the property to the user */
  protected String name;
  /** The value */
  protected Object value;
  protected PropertyChangeSupport changeSupport;
  /** When noUpdate is true, setting the value programmatically will not
   * result in an update of the GUI Component */
  protected boolean noUpdate = false;
  /** When frozen is true, setting the value programmatically will not
   * result in a PropertyChangeEvent being fired */
  protected boolean frozen = false;
  /** A Hint to be displayed in an empty field */
  protected String hint = "";
  /** The current highlight status of the Configurer */
  private boolean highlighted = false;
  /** Default Highlight Color for Configurable Lists */
  public static final Color LIST_ENTRY_HIGHLIGHT_COLOR = new Color(255, 230, 230);

  /**
   * The ContextLevel of a Configurer defines the level that it is defined at.
   * It is used by the FunctionBuilder (FB) to provide better contextual help for Function calls.
   *  o PIECE  - The Configurer is defined at the Piece level. Pieces move around, so we can make no assumptions
   *             about where the piece may be. All functions are to be displayed by the FB
   *  o MAP    - The Configurer is defined at the Map level, with the actual Map recorded in context
   *             FB will not show 'Trait Only' versions of functions.
   *             Any reference to map or zone level properties on a different map to be wrapped in an appropriate GetXXXProperty function.
   *  o MODULE - The Configurer is defined at the Module level
   *             FB will not show 'Trait Only' versions of functions.
   *             Any references to Map or Zone level properties to be wrapped in an appropriate GetXXXProperty function.
   */
  public enum ContextLevel { PIECE, MAP, MODULE };

  protected ContextLevel contextLevel;
  protected AbstractBuildable context;

  protected Configurer parentConfigurer;

  public Configurer(String key, String name) {
    this(key, name, null);
  }

  public Configurer(String key, String name, Object val) {
    this.key = key;
    this.name = name == null ? "" : name;
    changeSupport = new PropertyChangeSupport(this);
    setValue(val);
  }

  /**
   * Unique identifier
   */
  public String getKey() {
    return key;
  }

  /**
   * Plain English description of the Object
   */
  public String getName() {
    return name;
  }

  public void setName(String s) {
    final String oldName = name;
    name = s;
    if (!frozen) {
      changeSupport.firePropertyChange(NAME_PROPERTY, oldName, name);
    }
  }

  /**
   * The Object value
   * May be null if the Object has not been initialized
   */
  public Object getValue() {
    return value;
  }

  /**
   * @return a String representation of the Object value
   */
  public abstract String getValueString();

  /**
   * Set the Object value
   */
  public void setValue(Object o) {
    final Object oldValue = getValue();
    value = o;
    if (!frozen) {
      changeSupport.firePropertyChange(key, oldValue, value);
    }
  }

  /**
   * If true, then don't fire PropertyChangeEvents when the value is reset
   */
  public void setFrozen(boolean val) {
    frozen = val;
  }

  public boolean isFrozen() {
    return frozen;
  }

  /**
   * Fire a PropertyChangeEvent as if the value had been set from null
   */
  public void fireUpdate() {
    changeSupport.firePropertyChange(key, null, value);
  }

  /**
   * Set the Object value from a String
   */
  public abstract void setValue(String s);

  /**
   * GUI interface for setting the option in an editing window
   */
  public abstract Component getControls();

  /**
   * Add a listener to be notified when the Object state changes
   */
  public void addPropertyChangeListener(PropertyChangeListener l) {
    changeSupport.addPropertyChangeListener(l);
  }

  public void removePropertyChangeListener(PropertyChangeListener l) {
    changeSupport.removePropertyChangeListener(l);
  }

  /**
   * Repack the current configurer
   */
  protected void repack() {
    repack(getControls());
  }

  /**
   * Repack a dialog or window
   */
  protected void repack(Component controls) {
    SwingUtils.repack(controls);
  }


  /**
   * Return the current screen size for use by List type configurers to allow them to take up
   * maximum screen real estate if needed.
   *
   * The headless check is required in case any Configurers try to initialize during tests.
   *
   * @return Screen Size.
   */
  protected Dimension getScreenSize() {
    return SwingUtils.getScreenSize();
  }

  /**
   * Return the current hint String
   *
   * @return Current Hint String
   */
  public String getHint() {
    return hint;
  }

  /**
   * Set the Hint String
   *
   * @param hint New Hint string
   */
  public void setHint(String hint) {
    this.hint = hint;
  }

  /**
   * Set the Hint String via a I18n key
   * @param hintKey I18n key for the hint
   */
  public void setHintKey(String hintKey) {
    this.hint = Resources.getString(hintKey);
  }

  /**
   * Show/Hide the internal label maintained by this Configurer. It is up
   * to individual Configurers to track and hide the label (if they can).
   *
   * This method is currently only utilized by the Preference configs
   * {@link VASSAL.preferences.PrefsEditor#addOption(String, Configurer)}
   * to extract an existing label in a configurer, display correctly
   * aligned and suppress the original label. This keeps compatibility with
   * custom module code setting up preferences.
   *
   * This method only needs to be implemented in Configurers that are
   * added as preferences.
   *
   * @param visible Hide label if true
   */
  public void setLabelVisible(boolean visible) {
  }

  /** @deprecated Use {@link #setLabelVisible} instead. */
  @Deprecated(since = "2023-01-14", forRemoval = true)
  public void setLabelVisibile(boolean visible) {
    setLabelVisible(visible);
  }

  /**
   * Set the highlighted status of this configurer.
   * It is up to individual Configurers to override this method and implement a suitable visual highlighting scheme
   * Note: Cannot make this abstract as it will break custom code.
   *
   * @param highlighted New Highlighted status
   */
  public void setHighlighted(boolean highlighted) {
    this.highlighted = highlighted;
  }

  /**
   * Return the current highlighted status
   *
   * @return Highlight status
   */
  public boolean isHighlighted() {
    return highlighted;
  }

  /**
   * Add a FocusListener to the Swing Components that make up this Configurer.
   *
   * @param listener Focus Listener
   */
  public void addFocusListener(FocusListener listener) {

  }

  /**
   * Remove a FocusListener from the Swing Components that make up this Configurer.
   *
   * @param listener Focus Listener
   */
  public void removeFocusListener(FocusListener listener) {

  }

  /**
   * Move the cursor to the first input field of this Configurer
   */
  public void requestFocus() {

  }

  /**
   * Initialize any custom controls / keystrokes
   */
  public void initCustomControls(JDialog d, Configurable target) {

  }

  /**
   * Enable or Disable the action of this configurer/
   * @param enabled
   */
  public void setEnabled(boolean enabled) {

  }

  /**
   * Get the Context for this Configurer
   * @return  Owning Configurable
   */
  public AbstractBuildable getContext() {
    return context;
  }

  /**
   * Set the Context for this Configurer to the first ancestor that is of type GameModule or Map
   *
   * NOTE: The ContextLevel may already have been preset by the ConfigureFactory when the Configurer was created.
   *       Don't let the AutoConfigurer over-write this
   *
   * @param context Owning Configurable
   */
  public void setContext(AbstractBuildable context) {

    AbstractBuildable c = context;
    this.context = null;

    while (c != null) {
      if (c instanceof GameModule || c instanceof Map) {
        this.context = c;
        if (this.contextLevel == null) {
          this.contextLevel = c instanceof GameModule ? ContextLevel.MODULE : ContextLevel.MAP;
        }
        return;
      }
      if (c instanceof AbstractBuildable) {
        c = (AbstractBuildable) c.getAncestor();
      }
      // If we hit a straight Buildable in the parent tree, can't go any further. Defer to GameModule
      else {
        c = null;
      }
    }

    if (c == null) this.context = GameModule.getGameModule();
    if (this.contextLevel == null) this.contextLevel = ContextLevel.PIECE;
  }

  public void setContext(Configurable context) {
    if (context instanceof AbstractBuildable) {
      setContext((AbstractBuildable) context);
    }
  }

  public void setContext(Buildable context) {
    if (context instanceof AbstractBuildable) {
      setContext((AbstractBuildable) context);
    }
  }

  public ContextLevel getContextLevel() {
    return contextLevel;
  }

  public boolean isPieceContext() {
    // If the ContextLevel has not been set specifically, then it will be Piece Level
    return contextLevel == null || ContextLevel.PIECE == contextLevel;
  }

  public void setContextLevel(ContextLevel contextLevel) {
    this.contextLevel = contextLevel;
  }

  public Configurer getParentConfigurer() {
    return parentConfigurer;
  }

  public void setParentConfigurer(Configurer parentConfigurer) {
    this.parentConfigurer = parentConfigurer;
  }

  public void refreshParent() {
    if (parentConfigurer != null) {
      parentConfigurer.refresh();
    }
  }

  public void refresh() {

  }
}
