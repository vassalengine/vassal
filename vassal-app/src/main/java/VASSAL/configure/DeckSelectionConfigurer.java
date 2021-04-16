/*
 *
 * Copyright (c) 2021 by The Vassal Development Team
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
import VASSAL.build.module.properties.PropertyChangerConfigurer;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;

import java.awt.Component;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;

public class DeckSelectionConfigurer extends FormattedExpressionConfigurer {

  private JButton select;

  public DeckSelectionConfigurer(String s, GamePiece p) {
    super(s, p);
    select = new JButton(Resources.getString("Editor.select"));
    select.addActionListener(e -> showPopup());
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
    mapMenu.show(select, 0, 0);
  }

  @Override
  public Component getControls() {
    final JPanel p = (JPanel) super.getControls();
    p.add(select, "gapleft 2");
    return p;
  }
}
