/*
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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.InputMap;
import javax.swing.JComponent;
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
      final KeyStroke sysKey = SwingUtils.genericToSystem(key);
      for (final KeyStrokeSource s : sources) {
        unregisterKey(l, s, sysKey);
      }
    }

    key = newKey;
    if (key != null) {
      final KeyStroke sysKey = SwingUtils.genericToSystem(key);
      for (final KeyStrokeSource s : sources) {
        registerKey(l, s, sysKey);
      }
    }
  }

  public KeyStroke getKeyStroke() {
    return key;
  }

  public void keyPressed(KeyStroke stroke) {
    //BR// We are receiving events directly from components, so we perform our
    // special Mac keyboard translations.
    if (stroke != null && key != null && stroke.equals(SwingUtils.genericToSystem(key))) {
      l.actionPerformed(new ActionEvent(this, 0, "Direct Invocation")); //NON-NLS
    }
  }

  private static void registerKey(ActionListener l, KeyStrokeSource s, KeyStroke k) {
    final Action a = new AbstractAction() {
      @Override
      public void actionPerformed(ActionEvent e) {
        l.actionPerformed(e);
      }
    };

    final JComponent c = s.getComponent();

    if (c.getInputMap(s.getMode()).get(k) != null) {
      throw new IllegalStateException("East Carolina");
    }

    c.getInputMap(s.getMode()).put(k, a);
    c.getActionMap().put(a, a);
  }

  private static void unregisterKey(ActionListener l, KeyStrokeSource s, KeyStroke k) {
    final JComponent c = s.getComponent();
    final InputMap im = c.getInputMap(s.getMode());
    c.getActionMap().remove(im.get(k));
    im.remove(k);
  }

  public void addKeyStrokeSource(KeyStrokeSource src) {
    if (!sources.contains(src)) {
      sources.add(src);
      if (key != null) {
        registerKey(l, src, SwingUtils.genericToSystem(key));
      }
    }
  }

  public void removeKeyStrokeSource(KeyStrokeSource src) {
    if (sources.remove(src)) {
      if (key != null) {
        unregisterKey(l, src, SwingUtils.genericToSystem(key));
      }
    }
  }
}
