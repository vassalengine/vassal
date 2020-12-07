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
import javax.swing.JComponent;

/**
 * An interface to be implemented by Configurers that manage variable sized lists.
 * See also {@link ConfigurableListEntry}, {@link ConfigurableListController}
 */
public interface ConfigurableList extends PropertyChangeListener {
  /**
   * Move the currently selected list entry up one position
   * The moved entry should retain the selection
   */
  void moveEntryUp();

  /**
   * Move the currently selected list entry down one position
   * The moved entry should retain the selection
   */
  void moveEntryDown();

  /**
   * Add a new list entry after the currently selected entry, or at the bottom of the
   * list if no entries selected.
   * The new entry should be selected.
   */
  void addEntry();

  /**
   * Remove the specified entry from the list. The entry following the removed entry
   * should be selected, or the last entry in the list if the deleted entry was last.
   *
   * @param entry Entry to delete.
   */
  void deleteEntry(ConfigurableListEntry entry);

  /**
   * Return the ListController associated with this Configurable List
   *
   * @return List Controller
   */
  JComponent getListController();

  /**
   * Set the selection to the specified list entry.
   *
   * @param entry Entry to select
   */
  void selectEntry(ConfigurableListEntry entry);

  /**
   * Repack the Configurer
   */
  void repack();
}
