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
package VASSAL.build.module.map;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.i18n.Resources;

/**
 * This component listens to key events on a Map window and
 * scrolls the map.  Depending on the USE_ARROWS attribute,
 * will use number keypad or arrow keys, or will offer a
 * preferences setting for the user to choose
 */
// I18n: Partial
public class Scroller extends AbstractBuildable implements KeyListener {
  /**
   * The attribute name for whether to use arrow keys
   * instead of number keypad.  Should be one of ALWAYS, NEVER, or PROMPT
   */
  public static final String USE_ARROWS = "useArrows";
  public static final String ALWAYS = "always";
  public static final String NEVER = "never";
  public static final String PROMPT = "prompt";

  protected String usingArrows = PROMPT;

  private Map map;
  private char noEcho = 0;

  public void addTo(Buildable parent) {
    map = (Map) parent;
    map.getView().addKeyListener(this);

    BooleanConfigurer c = new BooleanConfigurer(USE_ARROWS, Resources.getString("Scroller.use_arrow_keys_preference"), Boolean.FALSE);
    if (ALWAYS.equals(usingArrows)) {
      GameModule.getGameModule().getPrefs().addOption(null, c);
      c.setValue(Boolean.TRUE);
    }
    else if (PROMPT.equals(usingArrows)) {
      GameModule.getGameModule().getPrefs().addOption(c);
    }
    else if (NEVER.equals(usingArrows)) {
      GameModule.getGameModule().getPrefs().addOption(null, c);
      c.setValue(Boolean.FALSE);
    }

  }

  public void add(Buildable b) {
  }

  public String[] getAttributeNames() {
    return new String[]{PROMPT};
  }

  public void setAttribute(String name, Object value) {
    if (USE_ARROWS.equals(name)) {
      usingArrows = (String) value;
    }
  }

  public String getAttributeValueString(String name) {
    if (USE_ARROWS.equals(name)) {
      return usingArrows;
    }
    else {
      return null;
    }
  }

  protected int xStep = 100;
  protected int yStep = 100;

  public void keyPressed(KeyEvent e) {
    if (e.isConsumed()) {
      return;
    }
    if (Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(USE_ARROWS))) {
      switch (e.getKeyCode()) {
        case KeyEvent.VK_UP:
          map.scroll(0, -yStep);
          e.consume();
          break;
        case KeyEvent.VK_DOWN:
          map.scroll(0, yStep);
          e.consume();
          break;
        case KeyEvent.VK_RIGHT:
          map.scroll(xStep, 0);
          e.consume();
          break;
        case KeyEvent.VK_LEFT:
          map.scroll(-xStep, 0);
          e.consume();
          break;
      }
    }
    else {
      switch (e.getKeyCode()) {
        case KeyEvent.VK_NUMPAD1:
          noEcho = '1';
          map.scroll(-xStep, yStep);
          e.consume();
          break;
        case KeyEvent.VK_NUMPAD2:
          noEcho = '2';
          map.scroll(0, yStep);
          e.consume();
          break;
        case KeyEvent.VK_NUMPAD3:
          noEcho = '3';
          map.scroll(xStep, yStep);
          e.consume();
          break;
        case KeyEvent.VK_NUMPAD4:
          noEcho = '4';
          map.scroll(-xStep, 0);
          e.consume();
          break;
        case KeyEvent.VK_NUMPAD6:
          noEcho = '6';
          map.scroll(xStep, 0);
          e.consume();
          break;
        case KeyEvent.VK_NUMPAD7:
          noEcho = '7';
          map.scroll(-xStep, -yStep);
          e.consume();
          break;
        case KeyEvent.VK_NUMPAD8:
          noEcho = '8';
          map.scroll(0, -yStep);
          e.consume();
          break;
        case KeyEvent.VK_NUMPAD9:
          noEcho = '9';
          map.scroll(xStep, -yStep);
          e.consume();
          break;
      }
    }
  }

  public void keyReleased(KeyEvent e) {
  }

  public void keyTyped(KeyEvent e) {
    if (e.isConsumed()) {
      return;
    }
    if (e.getKeyChar() == noEcho) {
      e.consume();
      noEcho = 0;
    }
  }
}
