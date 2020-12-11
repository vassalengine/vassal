/*
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

import java.beans.PropertyChangeListener;
import javax.swing.JButton;

/**
 * An interface to be implemented by Configurers that manage variable sized lists.
 * Each Configurable List entry maintains all info about a particular list entry
 *
 * See also {@link ConfigurableList}, {@link ConfigurableListController}, {@link AbstractConfigurableListEntry}
 */
public interface ConfigurableListEntry extends PropertyChangeListener {
  /**
   * Return the Remove button associated with this entry
   * @return Remove Button
   */
  JButton getRemoveButton();

  /**
   * Return the Configurer associated with this entry
   * @return Entry Configurer
   */
  Configurer getConfigurer();

  /**
   * Update the visibility of sub-items of this Entry
   */
  void updateVisibility();

  /**
   * Set the Highlight status of this enytry
   *
   * @param b Highlight status
   */
  void setHighlighted(boolean b);

  /**
   * This Entry has gained the focus
   */
  void focusGained();

  /**
   * Ensure this entry has the focus
   */
  void requestFocus();

  /**
   * Add a Property Change Listener to the Configurer for this Entru
   * @param l Listener
   */
  default void addPropertyChangeListener(PropertyChangeListener l) {
    getConfigurer().addPropertyChangeListener(l);
  }

  /**
   * Remove a Property Change Listener from the Configurer for this Entru
   * @param l Listener
   */
  default void removePropertyChangeListener(PropertyChangeListener l) {
    getConfigurer().removePropertyChangeListener(l);
  }
}
