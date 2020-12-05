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

import javax.swing.JPanel;

/**
 * An interface to be implemented by Configurers that manage variable sized lists.
 * See also {@link ConfigurableListEntry}, {@link ConfigurableListController}
 */
public interface ConfigurableList {
  void moveEntryUp();
  void moveEntryDown();
  void moveEntryTop();
  void moveEntryBottom();
  void addEntry();
  void deleteEntry(ConfigurableListEntry entry);
  void editEntry();
  JPanel getListController();
  void selectEntry(ConfigurableListEntry entry);
}
