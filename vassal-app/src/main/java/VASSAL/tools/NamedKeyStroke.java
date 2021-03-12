/*
 *
 * Copyright (c) 2008-2009 by Brent Easton
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

import javax.swing.KeyStroke;

import VASSAL.tools.swing.SwingUtils;

/**
 * A NamedKeyStroke is a KeyStroke with a name given by the module developer.
 * An actual KeyStroke is allocated from a pool of KeyStrokes at run-time and
 * associated with the name.
 *
 * KeyStrokes have been replaced by NamedKeyStrokes within Vassal wherever
 * the Stroke is saved. A standard KeyStroke is represented as a NamedKeyStroke
 * with a null or zero-length name.
 *
 * NamedKeyStroke variables should never be null, use NULL_KEYSTROKE to
 * represent a NamedKeyStroke with no value.
 */
public class NamedKeyStroke {
  public static final NamedKeyStroke NULL_KEYSTROKE = new NamedKeyStroke();

  protected KeyStroke stroke;
  protected String name;

  public NamedKeyStroke(int code, int modifiers) {
    this(code, modifiers, null);
  }

  public NamedKeyStroke(int code, int modifiers, String s) {
    this(KeyStroke.getKeyStroke(code, modifiers), s);
  }

  public NamedKeyStroke(KeyStroke k, String s) {
    stroke = k;
    name = s;
  }

  public NamedKeyStroke(KeyStroke k) {
    this(k, null);
  }

  public NamedKeyStroke(String s) {
    this(NamedKeyManager.getMarkerKeyStroke(), s);
  }

  public NamedKeyStroke() {
    this(null, null);
  }

  /**
   * Is there a name associated with this KeyStroke? No name means
   * it is a standard KeyStroke.
   * @return True if a name associated with this Keystroke
   */
  public boolean isNamed() {
    return ! (name == null || name.length() == 0);
  }

  public String getName() {
    return name;
  }

  public boolean isNull() {
    return (stroke == null && name == null) || (stroke != null && stroke.getKeyCode() == 0 && stroke.getModifiers() == 0);
  }

  /**
   * Return the raw KeyStroke stored in this NamedKeyStroke
   * @return KeyStroke
   */
  public KeyStroke getStroke() {
    return stroke;
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof NamedKeyStroke) {
      if (getKeyStroke() == null) {
        return ((NamedKeyStroke) o).getKeyStroke() == null;
      }
      else {
        return getKeyStroke().equals(((NamedKeyStroke) o).getKeyStroke());
      }
    }
    // checking for parameter being a completely unrelated class to this class
    // deliberate misuse of equals()
    else if (o instanceof KeyStroke) {
      final KeyStroke a = getKeyStroke();
      if (a == null) {
        return false;
      }
      return a.equals(o);
    }
    return false;
  }

  /**
   * Return the allocated KeyStroke associated with this KeyStroke
   */
  public KeyStroke getKeyStroke() {
    if (isNamed() && stroke == null) {
      stroke = NamedKeyManager.getInstance().getKeyStroke(this);
    }
    return getStroke();
  }

  public static NamedKeyStroke getNamedKeyStroke(char c) {
    return getNamedKeyStroke(c, 0);
  }

  public static NamedKeyStroke getNamedKeyStroke(char c, int mod) {
    return new NamedKeyStroke(KeyStroke.getKeyStroke(c, mod));
  }

  public static NamedKeyStroke getNamedKeyStroke(int c, int mod) {
    return new NamedKeyStroke(KeyStroke.getKeyStroke(c, mod));
  }

  public static NamedKeyStroke getKeyStrokeForEvent(KeyEvent e) {
    return new NamedKeyStroke(SwingUtils.getKeyStrokeForEvent(e));
  }

}