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

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.counters.KeyBuffer;
import VASSAL.tools.swing.SwingUtils;

/**
 * This KeyListener forwards key event from a {@link Map} to the
 * {@link KeyBuffer}, where it is given to selected GamePieces to
 * interpret.  The event is forwarded only if not consumed
 *
 * @see KeyBuffer
 * @see VASSAL.counters.GamePiece#keyEvent
 * @see InputEvent#isConsumed */
public class ForwardToKeyBuffer implements Buildable, KeyListener {
  private KeyEvent lastConsumedEvent;

  @Override
  public void build(org.w3c.dom.Element e) {
  }

  @Override
  public void addTo(Buildable parent) {
    Map map = (Map) parent;
    map.getView().addKeyListener(this);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  @Override
  public void keyPressed(KeyEvent e) {
    process(e);
  }

  @Override
  public void keyReleased(KeyEvent e) {
    process(e);
  }

  @Override
  public void keyTyped(KeyEvent e) {
    process(e);
  }

  protected void process(KeyEvent e) {
    // If we've consumed a KeyPressed event,
    // then automatically consume any following KeyTyped event
    // resulting from the same keypress
    // This prevents echoing characters to the Chat area if they're keycommand for selected pieces
    if (lastConsumedEvent != null
        && lastConsumedEvent.getWhen() == e.getWhen()) {
      e.consume();
    }
    else {
      lastConsumedEvent = null;
    }
    final int c = e.getKeyCode();
    // Don't pass modifier keys alone to counters
    final boolean onlyModifierKeys = (c == KeyEvent.VK_SHIFT || c == KeyEvent.VK_CONTROL || c == KeyEvent.VK_ALT || c == KeyEvent.VK_META);
    if (!e.isConsumed() && !onlyModifierKeys) { 
      Command comm = KeyBuffer.getBuffer().keyCommand
          (SwingUtils.getKeyStrokeForEvent(e));
      if (comm != null && !comm.isNull()) {
        GameModule.getGameModule().sendAndLog(comm);
        e.consume();
        lastConsumedEvent = e;
      }
    }
  }
}
