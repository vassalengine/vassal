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

import javax.swing.JButton;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

/**
 * A small panel of buttons for controlling the entries in a Configurablelist
 *
 * See also {@link ConfigurableList}, {@link ConfigurableListEntry}, {@link ConfigurableListController}
 */
public class ConfigurableListController extends JPanel {
  private static final long serialVersionUID = 1L;

  private final JButton upButton;
  private final JButton dnButton;

  public ConfigurableListController(final ConfigurableList list) {
    this(list, ConfigurableList.DEFAULT_ICON_SIZE);
  }

  public ConfigurableListController(final ConfigurableList list, final int iconSize) {
    super(new MigLayout("ins 0", "[]rel[]rel[]")); // NON-NLS

    upButton = new NoInsetButton("go-up", iconSize, "Editor.ConfigurableListEntryController.up_button_tip"); // NON-NLS
    dnButton = new NoInsetButton("go-down", iconSize, "Editor.ConfigurableListEntryController.down_button_tip"); // NON-NLS
    final JButton addButton = new NoInsetButton("add", iconSize, "Editor.ConfigurableListEntryController.add_button_tip"); // NON-NLS

    upButton.setEnabled(false);
    dnButton.setEnabled(false);

    upButton.addActionListener(e -> list.moveEntryUp());
    dnButton.addActionListener(e -> list.moveEntryDown());
    addButton.addActionListener(e -> list.addEntry());

    add(upButton);
    add(dnButton);
    add(addButton);
  }

  public void setCanMoveUp(boolean b) {
    upButton.setEnabled(b);
  }

  public void setCanMoveDown(boolean b) {
    dnButton.setEnabled(b);
  }

}
