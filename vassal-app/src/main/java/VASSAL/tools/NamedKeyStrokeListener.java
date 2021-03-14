/*
 *
 * Copyright (c) 2009 by Brent Easton
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

import java.awt.event.ActionListener;

import javax.swing.KeyStroke;
/**
 * Extension of KeyStrokeListener to support NamedKeyStroke's and
 * store the supplied NamedKeyStroke. Some components use KeyStrokeListeners
 * to store KeyStroke values.
 *
 */
public class NamedKeyStrokeListener extends KeyStrokeListener {

  private NamedKeyStroke namedKeyStroke;

  public NamedKeyStrokeListener(ActionListener l) {
    super(l);
  }

  public NamedKeyStrokeListener(ActionListener l, NamedKeyStroke key) {
    this(l);
    setKeyStroke(key);
  }

  @Override
  public void setKeyStroke(KeyStroke newKey) {
    super.setKeyStroke(newKey);
    namedKeyStroke = NamedKeyStroke.of(newKey);
  }

  public void setKeyStroke(NamedKeyStroke newKey) {
    super.setKeyStroke(newKey == null ? null : newKey.getKeyStroke());
    namedKeyStroke = newKey;
  }

  public NamedKeyStroke getNamedKeyStroke() {
    return namedKeyStroke;
  }
}
