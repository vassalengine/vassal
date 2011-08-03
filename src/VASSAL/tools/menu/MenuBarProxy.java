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

import java.lang.ref.WeakReference;

import javax.swing.JComponent;
import javax.swing.JMenuBar;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class MenuBarProxy extends AbstractParent<JMenuBar> {

/*
  private final List<ButtonGroupProxy> groups =
    new ArrayList<ButtonGroupProxy>();

  public ButtonGroupProxy addButtonGroup(final ButtonGroupProxy group) {
    groups.add(group);

    forEachPeer(new Functor<JMenuBar>() {
      public void apply(JMenuBar mb) {
        group.createPeer().setOwner(mb);
      }
    });

    return group;
  }

  public void removeButtonGroup(final ButtonGroupProxy group) {
    groups.remove(group);
  }
*/

  @Override
  public JMenuBar createPeer() {
    final JMenuBar mb = new JMenuBar();

    for (ChildProxy<?> child : children) {
      final JComponent peer = child.createPeer();
      if (peer != null) mb.add(peer);
    }

/*
    for (ButtonGroupProxy group : groups) {
      group.createPeer().setOwner(mb);
    }
*/

    peers.add(new WeakReference<JMenuBar>(mb, queue));
    return mb;
  }
}
