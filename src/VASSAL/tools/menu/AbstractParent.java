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
import java.util.List;

import javax.swing.JComponent;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class AbstractParent<T extends JComponent>
                extends AbstractProxy<T>
                implements ParentProxy {

  protected final List<ChildProxy<?>> children = new ArrayList<ChildProxy<?>>();

  public void add(final ChildProxy<?> child) {
    children.add(child);
    child.setParent(this);

    if (child instanceof MenuMarker) return;

    forEachPeer(new Functor<T>() {
      public void apply(T peer) {
        peer.add(child.createPeer());
      }
    });
  }

  protected int proxyIndexToRealIndex(int pos) {
    // find the true position, neglecting markers
    int j = -1;
    for (int i = 0; i <= pos; i++) {
      if (!(children.get(i) instanceof MenuMarker)) j++;
    }
    return j;
  }

  public void insert(final ChildProxy<?> child, int pos) {
    children.add(pos, child);
    child.setParent(this);

    if (child instanceof MenuMarker) return;

    final int rpos = proxyIndexToRealIndex(pos);

    forEachPeer(new Functor<T>() {
      public void apply(T peer) {
        peer.add(child.createPeer(), rpos);
      }
    });
  }

  public void remove(ChildProxy<?> child) {
    if (children.remove(child)) {
      child.setParent(null);
    }
  }

  public void remove(int pos) {
    final ChildProxy<?> child = children.remove(pos);
    child.setParent(null);
  }

  public int getChildCount() {
    return children.size();
  }

  public ChildProxy<?>[] getChildren() {
    return children.toArray(new ChildProxy<?>[children.size()]);
  }

  public ChildProxy<?> getChild(int pos) {
    return children.get(pos);
  }

  public int getIndex(ChildProxy<?> child) {
    return children.indexOf(child);
  }
}
