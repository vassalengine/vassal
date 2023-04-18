/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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

package VASSAL.tools.menu;

import javax.swing.Action;
import javax.swing.JMenuItem;
import java.lang.ref.WeakReference;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class MenuItemProxy extends AbstractProxy<JMenuItem> {
  protected Action action;
  private boolean hideIfBlank = false;

  public MenuItemProxy() {
    this(null);
  }

  public MenuItemProxy(Action action) {
    this.action = action;
  }

  public MenuItemProxy(Action action, boolean hideIfBlank) {
    this.action = action;
    this.hideIfBlank = hideIfBlank;
  }

  public void setHideIfBlank(boolean hideIfBlank) {
    this.hideIfBlank = hideIfBlank;
  }
  
  public Action getAction() {
    return action;
  }

  public void setAction(final Action action) {
    this.action = action;

    forEachPeer(item -> {
      item.setAction(action);
      if (action == null) {
        item.setVisible(false);
      }
      else {
        final String name = (String)action.getValue(Action.NAME);
        item.setVisible(!hideIfBlank || ((name != null) && !name.isEmpty()));
      }
    });
  }

  @Override
  public JMenuItem createPeer() {
    final JMenuItem item = new JMenuItem(action);

    if (action == null) {
      item.setVisible(false);
    }
    else {
      final String name = (String) action.getValue(Action.NAME);
      item.setVisible(!hideIfBlank || ((name != null) && !name.isEmpty()));
    }

    peers.add(new WeakReference<>(item, queue));
    return item;
  }
}
