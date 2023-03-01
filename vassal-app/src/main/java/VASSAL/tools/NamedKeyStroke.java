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

import VASSAL.build.module.KeyNamer;
import VASSAL.tools.concurrent.ConcurrentSoftHashMap;
import VASSAL.tools.swing.SwingUtils;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.apache.commons.lang3.tuple.Pair;

import javax.swing.KeyStroke;
import java.awt.event.KeyEvent;
import java.util.Map;
import java.util.Objects;

import static java.awt.event.KeyEvent.VK_BACK_SPACE;
import static java.awt.event.KeyEvent.VK_DELETE;

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

  /**
   * @return If a Named keystroke, returns the name string; otherwise if a "real" keystroke returns a string naming the keystroke (e.g. "Ctrl+C")
   */
  public String getDesc() {
    if (isNamed()) {
      return name;
    }
    else if (stroke != null) {
      return KeyNamer.getKeyString(stroke);
    }
    else {
      return "";
    }
  }

  @SuppressFBWarnings(value = "EQ_CHECK_FOR_OPERAND_NOT_COMPATIBLE_WITH_THIS")
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }

    if (o instanceof NamedKeyStroke) {
      return Objects.equals(stroke, ((NamedKeyStroke) o).stroke);
    }
    if (stroke == null) {
      return false;
    }
    // checking for parameter being a completely unrelated class to this class
    // deliberate misuse of equals()
    else if (o instanceof KeyStroke) {
      final int code = stroke.getKeyCode();
      // Either DEL or BACKSPACE matches to either
      if ((code == VK_DELETE) || (code == VK_BACK_SPACE)) {
        final KeyStroke k = (KeyStroke) o;
        if ((k.getKeyCode() == VK_DELETE) || (k.getKeyCode() == VK_BACK_SPACE)) {
          return o.equals(KeyStroke.getKeyStroke(VK_DELETE, stroke.getModifiers(), stroke.isOnKeyRelease())) ||
                 o.equals(KeyStroke.getKeyStroke(VK_BACK_SPACE, stroke.getModifiers(), stroke.isOnKeyRelease()));
        }
      }
      return o.equals(stroke);
    }

    return false;
  }

  @Override
  public int hashCode() {
    return stroke.hashCode();
  }

  /**
   * Return the allocated KeyStroke associated with this KeyStroke
   */
  public KeyStroke getKeyStroke() {
    return stroke;
  }

  @Deprecated(since = "2021-12-01", forRemoval = true)
  public static NamedKeyStroke getNamedKeyStroke(char c) {
    return of(c);
  }
  @Deprecated(since = "2021-12-01", forRemoval = true)
  public static NamedKeyStroke getNamedKeyStroke(char c, int mod) {
    return of(c, mod);
  }

  @Deprecated(since = "2021-12-01", forRemoval = true)
  public static NamedKeyStroke getNamedKeyStroke(int c, int mod) {
    return of(c, mod);
  }

  @Deprecated(since = "2021-12-01", forRemoval = true)
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
