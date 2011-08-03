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

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.lang.ref.WeakReference;

import javax.swing.Action;
import javax.swing.JCheckBoxMenuItem;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class CheckBoxMenuItemProxy
       extends AbstractProxy<JCheckBoxMenuItem>
       implements ItemListener {

  private Action action;
  private boolean state;

  public CheckBoxMenuItemProxy() {
    this(null, false);
  }

  public CheckBoxMenuItemProxy(Action action) {
    this(action, false);
  }

  public CheckBoxMenuItemProxy(Action action, boolean state) {
    this.action = action;
    this.state = state;
  }

  public Action getAction() {
    return action;
  }

  public void setAction(final Action action)  {
    this.action = action;

    forEachPeer(new Functor<JCheckBoxMenuItem>() {
      public void apply(JCheckBoxMenuItem item) {
        item.setAction(action);
      }
    });
  }

  public boolean isSelected() {
    return state;
  }

  public void setSelected(final boolean state) {
    this.state = state;

    forEachPeer(new Functor<JCheckBoxMenuItem>() {
      public void apply(JCheckBoxMenuItem item) {
        item.setSelected(state);
      }
    });
  }

  @Override
  public JCheckBoxMenuItem createPeer() {
    final JCheckBoxMenuItem item = new JCheckBoxMenuItem(action);
    item.setSelected(state);
    item.addItemListener(this);

    peers.add(new WeakReference<JCheckBoxMenuItem>(item, queue));
    return item;
  }

  public void itemStateChanged(ItemEvent e) {
    state = e.getStateChange() == ItemEvent.SELECTED;

    forEachPeer(new Functor<JCheckBoxMenuItem>() {
      public void apply(JCheckBoxMenuItem item) {
        item.setSelected(state);
      }
    });
  }
}
