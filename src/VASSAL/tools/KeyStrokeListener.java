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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.KeyStroke;

/**
 * Utility class for associating an Action with a keystroke from multiple
 * different component sources
 *
 * @see VASSAL.build.GameModule#addKeyStrokeListener
 * @see VASSAL.build.GameModule#addKeyStrokeSource
 */
public class KeyStrokeListener {
  private ActionListener l;
  private KeyStroke key;
  private List<KeyStrokeSource> sources = new ArrayList<KeyStrokeSource>();

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
      for (KeyStrokeSource s : sources) {
        s.getComponent().unregisterKeyboardAction(key);
      }
    }
    key = newKey;
    for (KeyStrokeSource s : sources) {
      addKeyStrokeSource(s);
    }
  }

  public KeyStroke getKeyStroke() {
    return key;
  }

  public void keyPressed(KeyStroke stroke) {
    if (stroke != null && stroke.equals(key)) {
      l.actionPerformed(new ActionEvent(this,0,"Direct Invocation"));
    }
  }

  public void addKeyStrokeSource(KeyStrokeSource src) {
    if (!sources.contains(src)) {
      sources.add(src);
    }
    if (key != null) {
      src.getComponent().registerKeyboardAction(l, key, src.getMode());
    }
  }
}
