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

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.KeyStroke;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;

/**
 * This KeyListener forwards key event from a {@link Map} to the
 * {@link VASSAL.build.module.Chatter} The event is forwarded only if
 * not consumed
 *
 * @see VASSAL.build.module.Chatter#keyCommand
 * @see InputEvent#isConsumed */
public class ForwardToChatter implements Buildable, KeyListener {
  public void build(org.w3c.dom.Element e) {
  }

  public void addTo(Buildable parent) {
    Map map = (Map) parent;
    map.getView().addKeyListener(this);
  }

  public void add(Buildable b) {
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  public void keyPressed(KeyEvent e) {
    process(e);
  }

  public void keyReleased(KeyEvent e) {
    process(e);
  }

  public void keyTyped(KeyEvent e) {
    process(e);
  }

  private void process(KeyEvent e) {
    if (!e.isConsumed()) {
      GameModule.getGameModule().getChatter().keyCommand(KeyStroke.getKeyStrokeForEvent(e));
    }
  }
}
