/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
 * Utility class for associating an Action with a keystroke from multiple different component sources
 * @see VASSAL.build.GameModule#addKeyStrokeListener
 * @see VASSAL.build.GameModule#addKeyStrokeSource
 */
public class KeyStrokeListener {
  private ActionListener l;
  private KeyStroke key;
  private java.util.Vector sources = new java.util.Vector();

  public KeyStrokeListener(ActionListener l, KeyStroke key) {
    this.l = l;
    setKeyStroke(key);
  }

  public KeyStrokeListener(ActionListener l) {
    this(l, null);
  }

  public void setKeyStroke(KeyStroke newKey) {
    if (newKey != null && newKey.getKeyCode() == 0) {
      newKey = null;
    }
    if (key != null) {
      for (int i = 0; i < sources.size(); ++i) {
        ((KeyStrokeSource) sources.elementAt(i))
          .getComponent().unregisterKeyboardAction(key);
      }
    }
    key = newKey;
    for (int i = 0; i < sources.size(); ++i) {
      addKeyStrokeSource((KeyStrokeSource) sources.elementAt(i));
    }
  }

  public KeyStroke getKeyStroke() {
    return key;
  }

  public void addKeyStrokeSource(KeyStrokeSource src) {
    if (!sources.contains(src)) {
      sources.addElement(src);
    }
    if (key != null) {
      src.getComponent().registerKeyboardAction(l, key, src.getMode());
    }
  }
}
