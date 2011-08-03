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

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
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

  private Map<String,MenuMarker> markers = new HashMap<String,MenuMarker>();

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

  public MenuMarker getMarker(String key) {
    return markers.get(key);
  }

  public MenuMarker addMarker(String key) {
    final MenuMarker marker = new MenuMarker();
    markers.put(key, marker);
    return marker;
  }

  public void addToSection(String key, ChildProxy<?> item) {
    final MenuMarker start = getMarker(key + ".start");
    final MenuMarker end = getMarker(key + ".end");
    final ParentProxy parent = end.getParent();

    final int startPos = parent.getIndex(start);
    final int endPos = parent.getIndex(end);

    // do separator check if this is the first item in the group
    if (parent instanceof MenuProxy && startPos + 1 == endPos) {
      // check from the end of the group to the next visible item
      // and insert a separator if that item is not one
      if (nextVisibleItemNotASeparator(end)) {
        ((MenuProxy) parent).insertSeparator(endPos+1);
      }

      // check from the start of the group to the previous visible
      // item and insert a separator if that item is not one
      if (prevVisibleItemNotASeparator(start)) {
        ((MenuProxy) parent).insertSeparator(startPos);
      }
    }

    // insert the item between the markers
    parent.insert(item, endPos);
  }

  public void removeFromSection(String key, ChildProxy<?> item) {
    final MenuMarker start = getMarker(key + ".start");
    final MenuMarker end = getMarker(key + ".end");
    final ParentProxy parent = end.getParent();

    // remove the item
    parent.remove(item);

    // do separator check if this was the last item in the group
    if (parent instanceof MenuProxy) {
      final int startPos = parent.getIndex(start);
      final int endPos = parent.getIndex(end);
      if (startPos + 1 == endPos) {
        if (visibleItemBefore(start)) {
          // if we have a group on each side, or before but not after;
          // remove our top separator
          parent.remove(startPos-1);
        }
        else if (visibleItemAfter(end)) {
          // we have a group after, but none before;
          // remove our bottom separator
          parent.remove(endPos+1);
        }
        // otherwise, we were the sole group, our parent has no visible
        // items now
      }
    }
  }

  private boolean visibleItemAfter(ChildProxy<?> child) {
    final ParentProxy parent = child.getParent();
    final int count = parent.getChildCount();
    for (int i = parent.getIndex(child) + 1; i < count; i++) {
      final ChildProxy<?> c = parent.getChild(i);
      if (!(c instanceof MenuMarker)) return true;
    }
    return false;
  }

  private boolean visibleItemBefore(ChildProxy<?> child) {
    final ParentProxy parent = child.getParent();
    for (int i = parent.getIndex(child) - 1; i >= 0; i++) {
      final ChildProxy<?> c = parent.getChild(i);
      if (!(c instanceof MenuMarker)) return true;
    }
    return false;
  }

  private boolean nextVisibleItemNotASeparator(ChildProxy<?> child) {
    final ParentProxy parent = child.getParent();
    final int count = parent.getChildCount();
    for (int i = parent.getIndex(child) + 1; i < count; i++) {
      final ChildProxy<?> c = parent.getChild(i);
      if (c instanceof MenuMarker) continue;

      return !(c instanceof SeparatorProxy);
    }
    return false;
  }

  private boolean prevVisibleItemNotASeparator(ChildProxy<?> child) {
    final ParentProxy parent = child.getParent();
    for (int i = parent.getIndex(child) - 1; i >= 0; i--) {
      final ChildProxy<?> c = parent.getChild(i);
      if (c instanceof MenuMarker) continue;

      return !(c instanceof SeparatorProxy);
    }
    return false;
  }
}
