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

  @Override
  public void addTo(Buildable parent) {
    map = (Map) parent;
    map.getView().addKeyListener(this);

    final BooleanConfigurer c = new BooleanConfigurer(
      USE_ARROWS,
      Resources.getString("Scroller.use_arrow_keys_preference"),
      Boolean.FALSE
    );

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

  @Override
  public void add(Buildable b) {
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{PROMPT};
  }

  @Override
  public void setAttribute(String name, Object value) {
    if (USE_ARROWS.equals(name)) {
      usingArrows = (String) value;
    }
  }

  @Override
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

  @Override
  public void keyPressed(KeyEvent e) {
    if (e.isConsumed()) return;

    int dx;
    int dy;

    if (Boolean.TRUE.equals(
          GameModule.getGameModule().getPrefs().getValue(USE_ARROWS))) {
      switch (e.getKeyCode()) {
      case KeyEvent.VK_UP:    dx =  0; dy = -1; break;
      case KeyEvent.VK_DOWN:  dx =  0; dy =  1; break;
      case KeyEvent.VK_RIGHT: dx =  1; dy =  0; break;
      case KeyEvent.VK_LEFT:  dx = -1; dy =  0; break;
      default: return;
      }
    }
    else {
      switch (e.getKeyCode()) {
      case KeyEvent.VK_NUMPAD1: dx = -1; dy =  1; noEcho = '1'; break;
      case KeyEvent.VK_NUMPAD2: dx =  0; dy =  1; noEcho = '2'; break;
      case KeyEvent.VK_NUMPAD3: dx =  1; dy =  1; noEcho = '3'; break;
      case KeyEvent.VK_NUMPAD4: dx = -1; dy =  0; noEcho = '4'; break;
      case KeyEvent.VK_NUMPAD6: dx =  1; dy =  0; noEcho = '6'; break;
      case KeyEvent.VK_NUMPAD7: dx = -1; dy = -1; noEcho = '7'; break;
      case KeyEvent.VK_NUMPAD8: dx =  0; dy = -1; noEcho = '8'; break;
      case KeyEvent.VK_NUMPAD9: dx =  1; dy = -1; noEcho = '9'; break;
      default: return;
      }
    }

    map.scroll(dx * xStep, dy * yStep);
    e.consume();
  }

  @Override
  public void keyReleased(KeyEvent e) {
  }

  @Override
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
