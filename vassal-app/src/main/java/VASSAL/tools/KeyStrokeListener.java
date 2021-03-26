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

import java.awt.AWTEventMulticaster;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
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

    if (key != null && !NamedKeyManager.isNamed(key)) {
      final KeyStroke sysKey = SwingUtils.genericToSystem(key);
      for (final KeyStrokeSource s : sources) {
        unregisterKey(l, s, sysKey);
      }
    }

    key = newKey;
    if (key != null && !NamedKeyManager.isNamed(key)) {
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

  private static class ActionChain extends AbstractAction {
    private static final long serialVersionUID = 1L;

    private ActionListener l;

    public ActionChain(ActionListener l) {
      this.l = l;
    }

    public static ActionChain add(ActionListener l, ActionListener r) {
      final ActionChain c = l instanceof ActionChain ?
        (ActionChain) l : new ActionChain(l);
      c.l = AWTEventMulticaster.add(c.l, r);
      return c;
    }

    public static ActionChain remove(ActionListener l, ActionListener r) {
      ActionChain c;
      if (l instanceof ActionChain) {
        c = (ActionChain) l;
        c.l = AWTEventMulticaster.remove(c.l, r);
        return c.l == null ? null : c;
      }
      else {
        l = AWTEventMulticaster.remove(l, r);
        return l == null ? null : new ActionChain(l);
      }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      l.actionPerformed(e);
    }
  }

  private static void registerKey(ActionListener l, KeyStrokeSource s, KeyStroke k) {
    final JComponent c = s.getComponent();
    final InputMap imap = c.getInputMap(s.getMode());
    final ActionMap amap = c.getActionMap();

    Object o = imap.get(k);
    if (o == null) {
      o = new Object();
      imap.put(k, o);
    }

    amap.put(o, ActionChain.add(amap.get(o), l));
  }

  private static void unregisterKey(ActionListener l, KeyStrokeSource s, KeyStroke k) {
    final JComponent c = s.getComponent();
    final InputMap imap = c.getInputMap(s.getMode());

    final Object o = imap.get(k);
    if (o != null) {
      final ActionMap amap = c.getActionMap();
      final Action a = ActionChain.remove(amap.get(o), l);
      if (a == null) {
        imap.remove(k);
      }
      amap.put(o, a);
    }
  }

  public void addKeyStrokeSource(KeyStrokeSource src) {
    if (!sources.contains(src)) {
      sources.add(src);
      if (key != null && !NamedKeyManager.isNamed(key)) {
        registerKey(l, src, SwingUtils.genericToSystem(key));
      }
    }
  }

  public void removeKeyStrokeSource(KeyStrokeSource src) {
    if (sources.remove(src)) {
      if (key != null && !NamedKeyManager.isNamed(key)) {
        unregisterKey(l, src, SwingUtils.genericToSystem(key));
      }
    }
  }
}
