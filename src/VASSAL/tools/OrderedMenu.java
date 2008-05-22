/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.tools;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;

import VASSAL.i18n.Resources;

/**
 * Allows the specification of a specific order in which items will appear
 * in the menu, regardless of the order in which they are actually added
 * in the code.
 * 
 * Also provides some i18n support: automatically looks up resources and
 * sets menu mnemonics in a locale-independent way.
 * 
 * @author rodneykinney
 * @since 3.1.0 
 */
public class OrderedMenu extends JMenu {
  private static final long serialVersionUID = 1L;

  private List<String> items;

  private OrderedMenu(String title, List<String> items) {
    super(title);
    setMnemonic(title.charAt(0));
    this.items = items;
  }

   // Insert the menu item at the correct index so that the menu maintains the same order specified in the items list
  @Override
  public JMenuItem add(JMenuItem item) {
// FIMXE: Setting the mnemonic from the first letter is a bug. Should
// accept whatever mnemonic has already been set.
    item.setMnemonic(item.getText().charAt(0));

    // Note: This is extremely inefficient, but menus are small and
    // coding it this way kept the code simple.

//System.out.println("adding " + item.getText());

    // find location of this item in pre-built list
    final int target = items.indexOf(item.getText());
    if (target < 0) return super.add(item);

    // strip out separators
    for (int i = 0; i < getMenuComponentCount(); i++) {
      final Component c = getMenuComponent(i); 
      if (c instanceof JSeparator) remove(i--);
    }

    int actual = -1;
    JMenuItem ret = null;

    // find greatest exisitng predecessor
    previous: for (int prev = target-1; prev >= 0; prev--) {
      final String prevText = items.get(prev);
      if (prevText == null) continue;

      for (int i = 0; i < getMenuComponentCount(); i++) {
        final Component c = getMenuComponent(i);
        if (!(c instanceof JMenuItem)) continue;
        if (!prevText.equals(((JMenuItem) c).getText())) continue;

        ret = super.insert(item, ++i);
        actual = i;
        break previous;
      }
    }

    if (actual < 0) {
      actual = 0;
      ret = super.insert(item, 0);
    }

//System.out.println(actual + " " + item.getText());

    // reinsert separators
    for (int i = 0; i < items.size(); i++) {
      if (items.get(i) != null) continue;

      final String prev = items.get(i-1);
      for (int j = 0; j < getMenuComponentCount(); j++) {
        final Component c = getMenuComponent(j);
        if (!(c instanceof JMenuItem)) continue;

        if (prev.equals(((JMenuItem) c).getText())) {
          insertSeparator(j+1);
          break;
        }
      }
    }

    return ret;
  }

  /**
   * Builder pattern for specifying the order at instantiation time
   * 
   * @return
   */
  public static Builder builder(String title) {
    return new Builder(title);
  }

  public static class Builder {
    private String title;
    private List<String> items = new ArrayList<String>();

    public Builder(String title) {
      this.title = Resources.getString(title);
    }

    public Builder appendItem(String item) {
      items.add(Resources.getString(item));
      return this;
    }

    public Builder appendSeparator() {
      items.add(null);
      return this;
    }

    public OrderedMenu create() {
      return new OrderedMenu(title, items);
    }
  }
}
