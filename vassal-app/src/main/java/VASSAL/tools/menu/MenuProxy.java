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

import javax.swing.JComponent;
import javax.swing.JMenu;
import java.lang.ref.WeakReference;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class MenuProxy extends AbstractParent<JMenu> {
  private String text;
  private char mnemonic = 0;
  private boolean hideIfBlank = false;

  public MenuProxy() {
  }

  public MenuProxy(boolean hideIfBlank) {
    this.hideIfBlank = hideIfBlank;
  }

  public MenuProxy(String text) {
    this.text = text;
  }

  public boolean isHideIfBlank() {
    return hideIfBlank;
  }

  public void setHideIfBlank(boolean hideIfBlank) {
    this.hideIfBlank = hideIfBlank;
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

  public String getText() {
    return text;
  }

  public void setText(final String text) {
    this.text = text;

    forEachPeer(menu -> menu.setText(text));

    if (isHideIfBlank()) {
      forEachPeer(menu -> menu.setVisible((text != null) && !text.isEmpty()));
    }
  }

  @Override
  public JMenu createPeer() {
    final JMenu menu = new JMenu(text);
    if (mnemonic != 0) {
      menu.setMnemonic(mnemonic);
    }

    for (final ChildProxy<?> child : children) {
      final JComponent peer = child.createPeer();
      if (peer != null) menu.add(peer);
    }

    peers.add(new WeakReference<>(menu, queue));
    return menu;
  }

  public void setMnemonic(final char mnemonic) {
    this.mnemonic = mnemonic;
    forEachPeer(menu -> menu.setMnemonic(mnemonic));
  }
}
