/*
 *
 * Copyright (c) 2026 Christian Holm Christensen
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
import javax.swing.ButtonGroup;
import javax.swing.JRadioButtonMenuItem;

/**
 * @author Christian Holm Christensen
 * @since 3.8.0
 */
public class RadioButtonMenuItemProxy
       extends AbstractProxy<JRadioButtonMenuItem>
       implements ItemListener {

  private Action action;
  private boolean state;
  private ButtonGroup group;
  
  public RadioButtonMenuItemProxy() {
    this(null, false, null);
  }

  public RadioButtonMenuItemProxy(ButtonGroup group) {
    this(null, false, group);
  }

  public RadioButtonMenuItemProxy(Action action) {
    this(action, false, null);
  }
  
  public RadioButtonMenuItemProxy(Action action, ButtonGroup group) {
    this(action, false, group);
  }

  public RadioButtonMenuItemProxy(Action action, boolean state) {
    this.action = action;
    this.state = state;
  }

  public RadioButtonMenuItemProxy(Action action,
                                  boolean state,
                                  ButtonGroup group) {
    this.action = action;
    this.state = state;
    this.group = group;
  }
  
  public Action getAction() {
    return action;
  }

  public void setAction(final Action action)  {
    this.action = action;

    forEachPeer(item -> item.setAction(action));
  }

  public boolean isSelected() {
    return state;
  }

  public void setSelected(final boolean state) {
    this.state = state;

    forEachPeer(item -> item.setSelected(state));
  }

  @Override
  public JRadioButtonMenuItem createPeer() {
    final JRadioButtonMenuItem item = new JRadioButtonMenuItem(action);
    item.setSelected(state);
    item.addItemListener(this);

    peers.add(new WeakReference<>(item, queue));
    group.add(item);
    
    return item;
  }

  @Override
  public void itemStateChanged(ItemEvent e) {
    state = e.getStateChange() == ItemEvent.SELECTED;

    forEachPeer(item -> item.setSelected(state));
  }
}
