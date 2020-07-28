/*
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
package VASSAL.build.module;

import java.awt.event.KeyEvent;

import javax.swing.KeyStroke;

import org.apache.commons.lang3.StringUtils;

import VASSAL.build.Buildable;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.SwingUtils;


/**
 * Translates key codes into human-readable strings (e.g. "Backspace"). 
 * 
 * Exists as a buildable (buildFile element) so that it can be overridden by custom classes
 */
public class KeyNamer implements Buildable {
  @Override
  public void build(org.w3c.dom.Element e) {
  }

  @Override
  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  @Override
  public void addTo(Buildable b) {
  }

  @Override
  public void add(Buildable b) {
  }  

  /**
   * Implements VASSAL's plain text representation of a KeyStroke.
   * @param k - a keystroke (e.g. VK_BACKSPACE) to be rendered as a string
   * @return  - a human-readable name for the keystroke (e.g. "Backspace")
   */
  public static String getKeyString(KeyStroke k) {
    if (k == null) {
      return null;
    }

    String s;
    int code = k.getKeyCode();
    switch (code) {
    // The addition of underscores screws up these names
    case KeyEvent.VK_ADD:
      s = Resources.getString("Keys.numplus"); //$NON-NLS-1$ //$NON-NLS-2$
      break;
    case KeyEvent.VK_SUBTRACT:
      s = Resources.getString("Keys.numminus"); //$NON-NLS-1$ //$NON-NLS-2$
      break;
    // More compact name, also commonly printed keys  
    case KeyEvent.VK_PAGE_UP:
      s = Resources.getString("Keys.pgup"); //$NON-NLS-1$ //$NON-NLS-2$
      break;
    case KeyEvent.VK_PAGE_DOWN:
      s = Resources.getString("Keys.pgdn"); //$NON-NLS-1$ //$NON-NLS-2$
      break;
    // Non-Americans were (rightly) commenting about this
    case KeyEvent.VK_OPEN_BRACKET:
      s = Resources.getString("Keys.bropen"); //$NON-NLS-1$ //$NON-NLS-2$
      break;
    case KeyEvent.VK_CLOSE_BRACKET:
      s = Resources.getString("Keys.brclose"); //$NON-NLS-1$ //$NON-NLS-2$
      break;
    default:
      s = "";
      break;
    }
    if (s.isEmpty() || s.contains("Keys.")) { //$NON-NLS-1$
      s = KeyEvent.getKeyText(code); 
    }
    s = StringUtils.capitalize(s);
    s = s.replace(' ', '_');        

    final int mods = SwingUtils.genericToSystem(k).getModifiers();
    if ((mods & KeyEvent.SHIFT_DOWN_MASK) > 0) {
      s = Resources.getString("Keys.shift") + "+" + s; //$NON-NLS-1$ //$NON-NLS-2$
    }
    if ((mods & KeyEvent.CTRL_DOWN_MASK) > 0) {
      s = Resources.getString("Keys.ctrl") + "+" + s; //$NON-NLS-1$ //$NON-NLS-2$
    }
    if ((mods & KeyEvent.META_DOWN_MASK) > 0) {  // This is "Command" key on Mac
      s = Resources.getString("Keys.meta") + "+" + s; //$NON-NLS-1$ //$NON-NLS-2$
    }
    if ((mods & KeyEvent.ALT_DOWN_MASK) > 0) {
      s = Resources.getString("Keys.alt") + "+" + s; //$NON-NLS-1$ //$NON-NLS-2$
    }
    return s;
  }
}
