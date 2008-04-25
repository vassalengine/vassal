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
import javax.swing.JMenu;

public class MenuProxy extends AbstractParent<JMenu> {
  private String text;

  public MenuProxy() { }

  public MenuProxy(String text) {
    this.text = text;
  }

  public SeparatorProxy addSeparator() {
    final SeparatorProxy sep = new SeparatorProxy();
    add(sep);
    return sep;
  }

  public SeparatorProxy insertSeparator(int pos) {
    final SeparatorProxy sep = new SeparatorProxy();
    insert(sep, pos);
    return sep;
  }

  public void setText(final String text) {
    this.text = text;

    forEachPeer(new Functor<JMenu>() {
      public void apply(JMenu menu) {
        menu.setText(text);
      }
    });
  }

  @Override
  public JMenu createPeer() {
    final JMenu menu = new JMenu(text);

    for (ChildProxy<?> child : children) {
      if (child instanceof Marker) continue;
      menu.add(child.createPeer());
    }
    
    peers.add(new WeakReference<JMenu>(menu, queue));
    return menu;
  }
}
