/*
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

import java.util.ArrayList;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import VASSAL.i18n.Resources;

/**
 * Allows the specification of a specific order in which items will appear in the menu, regardless of the order in which
 * they are actually added in the code.
 * 
 * Also provides some i18n support: automatically looks up resources and sets menu mnemonics in a locale-independent way
 * 
 * @author rodneykinney
 * 
 */
public class OrderedMenu extends JMenu {
  private List<String> items;

  private OrderedMenu(String title, List<String> items) {
    super(title);
    setMnemonic(title.charAt(0));
    this.items = items;
  }

  // Insert the menu item at the correct index so that the menu maintains the same order specified in the items list
  public JMenuItem add(JMenuItem menu) {
    menu.setMnemonic(menu.getText().charAt(0));
    int targetPosition = items.indexOf(menu.getText());
    if (targetPosition < 0) {
      return super.add(menu);
    }
    int index = 0;
    int prev = targetPosition;
    outer: while (--prev >= 0) {
      // Look for the preceding item in the existing menu
      String previous = items.get(prev);
      if (previous != null) {
        for (int i = 0; i < getMenuComponentCount(); ++i) {
          if (getMenuComponent(i) instanceof JMenuItem) {
            if (previous.equals(((JMenuItem) getMenuComponent(i)).getText())) {
              index = i + 1;
              while (index < getMenuComponentCount() && getMenuComponent(index) instanceof JPopupMenu.Separator) {
                index++;
              }
              break outer;
            }
          }
        }
      }
    }
    if (targetPosition < items.size() - 1 && items.get(targetPosition + 1) == null) {
      insertSeparator(index);
    }
    return super.insert(menu, index);
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
