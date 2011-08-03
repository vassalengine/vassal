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

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JTextField;
import javax.swing.KeyStroke;

/**
 * Text component for specifying a hot key
 */
public class HotkeySpecifier extends JTextField implements KeyListener {
  private static final long serialVersionUID = 1L;

  private int key;
  private KeyStroke stroke;

  public HotkeySpecifier() {
    this(0);
  }

  public HotkeySpecifier(int key) {
    this.key = key;
    setText(key == 0 ? "" : KeyEvent.getKeyText(key));
    stroke = KeyStroke.getKeyStroke((char) key);
    addKeyListener(this);
  }

  public int getKey() {
    return key;
  }

  public KeyStroke getKeyStroke() {
    return stroke;
  }

  public void keyTyped(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    stroke = KeyStroke.getKeyStrokeForEvent(e);
  }

  public void keyReleased(KeyEvent e) {
    setText(getString(stroke));
  }

  public static String getString(KeyStroke k) {
    String s = KeyEvent.getKeyText(k.getKeyCode());
    if ((k.getModifiers() & KeyEvent.SHIFT_MASK) > 0) {
      s = KeyEvent.getKeyText(KeyEvent.VK_SHIFT) + "+" + s;
    }
    if ((k.getModifiers() & KeyEvent.CTRL_MASK) > 0) {
      s = KeyEvent.getKeyText(KeyEvent.VK_CONTROL) + "+" + s;
    }
    if ((k.getModifiers() & KeyEvent.META_MASK) > 0) {
      s = KeyEvent.getKeyText(KeyEvent.VK_META) + "+" + s;
    }
    if ((k.getModifiers() & KeyEvent.ALT_MASK) > 0) {
      s = KeyEvent.getKeyText(KeyEvent.VK_ALT) + "+" + s;
    }
    return s;
  }

  public static KeyStroke getStrokeForString(String s) {
    int index = s.indexOf(',');
    return KeyStroke.getKeyStroke
      (Integer.parseInt(s.substring(0, index)),
       Integer.parseInt(s.substring(index + 1)));

  }

  public static String getStringForStroke(KeyStroke stroke) {
    return stroke.getKeyCode() + "," + stroke.getModifiers();
  }

}
