/*
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

import VASSAL.tools.swing.SwingUtils;

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
  private final List<KeyStrokeSource> sources = new ArrayList<>();

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
      for (final KeyStrokeSource s : sources) {
        //BR// We are registering/unregistering events directly with components, so we perform our special Mac keyboard translations.
        s.getComponent().unregisterKeyboardAction(SwingUtils.genericToSystem(key));
      }
    }
    key = newKey;
    for (final KeyStrokeSource s : sources) {
      addKeyStrokeSource(s);
    }
  }

  public KeyStroke getKeyStroke() {
    return key;
  }

  public void keyPressed(KeyStroke stroke) {
    //BR// We are receiving events directly from components, so we perform our special Mac keyboard translations.
    if ((stroke != null) && (key != null) && stroke.equals(SwingUtils.genericToSystem(key))) {
      l.actionPerformed(new ActionEvent(this, 0, "Direct Invocation")); //NON-NLS
    }
  }

  public void addKeyStrokeSource(KeyStrokeSource src) {
    if (!sources.contains(src)) {
      sources.add(src);
    }
    if (key != null) {
      //BR// We are registering with components directly, so we perform our special Mac keyboard translations.
      src.getComponent().registerKeyboardAction(l, SwingUtils.genericToSystem(key), src.getMode());
    }
  }

  public void removeKeyStrokeSource(KeyStrokeSource src) {
    if (sources.contains(src)) {
      sources.remove(src);
    }
    if (key != null) {
      //BR// We are registering/unregistering events directly with components, so we perform our special Mac keyboard translations.
      src.getComponent().unregisterKeyboardAction(SwingUtils.genericToSystem(key));
    }
  }
}
