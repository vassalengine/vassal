/*
 *
 * Copyright (c) 2021 by vassalengine.org, Brent Easton, Brian Reynolds
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

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.DrawPile;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Class to display a drop-down list of Decks by Map and set an owning Configurer to the selected value
 */
public class DeckSelector extends JButton implements ActionListener {
  private static final long serialVersionUID = 1L;
  private Configurer owner;
  private String value;

  public DeckSelector() {
    this.owner = null;
    this.value = "";
    setText(Resources.getString("Editor.select"));
    addActionListener(this);
  }

  public DeckSelector(Configurer owner) {
    this();
    this.owner = owner;
  }

  public String getValue() {
    return value;
  }

  public void doPopup() {
    
  }

  public void showPopup() {
    final JPopupMenu mapMenu = new JPopupMenu();
    for (final Map m: GameModule.getGameModule().getAllDescendantComponentsOf(Map.class)) {
      final JMenu deckMenu = new JMenu(m.getMapName());
      for (final DrawPile d : m.getAllDescendantComponentsOf(DrawPile.class)) {
        final JMenuItem item = new JMenuItem(d.getConfigureName());
        item.addActionListener(ev -> setValue(d.getConfigureName()));
        deckMenu.add(item);
      }
      if (deckMenu.getItemCount() > 0) {
        mapMenu.add(deckMenu);
      }
    }
    mapMenu.show(this, 0, 0);
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    showPopup();
  }

  private void setValue(String value) {
    this.value = value;
    owner.setValue(value);
  }
}
