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
import java.util.Map;
import java.util.Objects;

import javax.swing.KeyStroke;

import org.apache.commons.lang3.tuple.Pair;

import VASSAL.tools.concurrent.ConcurrentSoftHashMap;
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
  private static final Map<Pair<KeyStroke, String>, NamedKeyStroke> CACHE = new ConcurrentSoftHashMap<>();

  public static final NamedKeyStroke NULL_KEYSTROKE = new NamedKeyStroke();

  protected final KeyStroke stroke;
  protected final String name;

  public NamedKeyStroke(int code, int modifiers) {
    this(code, modifiers, null);
  }

  public NamedKeyStroke(int code, int modifiers, String s) {
    this(KeyStroke.getKeyStroke(code, modifiers), s);
  }

  public NamedKeyStroke(KeyStroke k) {
    this(k, null);
  }

  public NamedKeyStroke(String s) {
    this(null, s);
  }

  public NamedKeyStroke() {
    this(null, null);
  }

  public NamedKeyStroke(KeyStroke k, String s) {
    name = s != null ? s.intern() : null;
    stroke = NamedKeyManager.getInstance().getKeyStroke(name, k);
  }

  /**
   * Is there a name associated with this KeyStroke? No name means
   * it is a standard KeyStroke.
   * @return True if a name associated with this Keystroke
   */
  public boolean isNamed() {
    return name != null && !name.isEmpty();
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
    if (this == o) {
      return true;
    }

    if (o instanceof NamedKeyStroke) {
      return Objects.equals(stroke, ((NamedKeyStroke) o).stroke);
    }
    // checking for parameter being a completely unrelated class to this class
    // deliberate misuse of equals()
    else if (o instanceof KeyStroke) {
      return o.equals(stroke);
    }

    return false;
  }

  /**
   * Return the allocated KeyStroke associated with this KeyStroke
   */
  public KeyStroke getKeyStroke() {
    return stroke;
  }

  @Deprecated
  public static NamedKeyStroke getNamedKeyStroke(char c) {
    return of(c);
  }
  @Deprecated
  public static NamedKeyStroke getNamedKeyStroke(char c, int mod) {
    return of(c, mod);
  }

  @Deprecated
  public static NamedKeyStroke getNamedKeyStroke(int c, int mod) {
    return of(c, mod);
  }

  @Deprecated
  public static NamedKeyStroke getKeyStrokeForEvent(KeyEvent e) {
    return of(e);
  }

  public static NamedKeyStroke of(char c) {
    return of(c, 0);
  }

  public static NamedKeyStroke of(KeyEvent e) {
    return of(SwingUtils.getKeyStrokeForEvent(e));
  }

  public static NamedKeyStroke of(char c, int modifiers) {
    return of(KeyStroke.getKeyStroke(c, modifiers));
  }

  public static NamedKeyStroke of(int code, int modifiers) {
    return of(KeyStroke.getKeyStroke(code, modifiers));
  }

  public static NamedKeyStroke of(int code, int modifiers, String s) {
    return of(KeyStroke.getKeyStroke(code, modifiers), s);
  }

  public static NamedKeyStroke of(KeyStroke k) {
    return of(k, null);
  }

  public static NamedKeyStroke of(String s) {
    return of(NamedKeyManager.getInstance().getKeyStroke(s, null), s);
  }

  public static NamedKeyStroke of(KeyStroke k, String s) {
    return CACHE.computeIfAbsent(
      Pair.of(k, s),
      p -> new NamedKeyStroke(p.getLeft(), p.getRight())
    );
  }
}
