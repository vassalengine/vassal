/*
 * $Id$
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

public abstract class MenuManager {
  protected static MenuManager instance;

  public static MenuManager getInstance() {
    if (instance == null) throw new IllegalStateException();
    return instance;
  }

  public MenuManager() {
    if (instance != null) throw new IllegalStateException();
    instance = this;
  }

  public abstract JMenuBar getMenuBarFor(JFrame fc);

  public abstract MenuBarProxy getMenuBarProxyFor(JFrame fc);

  private Map<String,List<MenuItemProxy>> actionLocations =
    new HashMap<String,List<MenuItemProxy>>();

  public MenuItemProxy addKey(String key) {
    List<MenuItemProxy> items = actionLocations.get(key);
    if (items == null) {
      items = new ArrayList<MenuItemProxy>();
      actionLocations.put(key, items);
    }

    final MenuItemProxy item = new MenuItemProxy();
    items.add(item);    
    return item;
  }

  public List<MenuItemProxy> getItems(String key) {
    return actionLocations.get(key);
  }

  public void addAction(String key, Action a) {
    final List<MenuItemProxy> items = actionLocations.get(key);
    if (items != null) {
      for (MenuItemProxy i : items) {
        i.setAction(a);
      }
    }
  }

  public void removeAction(String key) {
    addAction(key, null);
  }

  public MenuItemProxy addMarker(String key) {
    List<MenuItemProxy> items = actionLocations.get(key);
    if (items == null) {
      items = new ArrayList<MenuItemProxy>();
      actionLocations.put(key, items);
    }

    final MenuItemProxy item = new Marker();
    items.add(item);    
    return item;
  }
}
