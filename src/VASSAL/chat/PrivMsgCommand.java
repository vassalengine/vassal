/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.chat;

import java.awt.Component;
import java.awt.KeyboardFocusManager;
import java.awt.Window;

import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import VASSAL.command.Command;

/**
 * A Command that encapsulates a private chat message from another
 * {@link VASSAL.chat.SimplePlayer} */
public class PrivMsgCommand extends Command {
  private PrivateChatManager mgr;
  private String msg;
  private Player p;

  public PrivMsgCommand(PrivateChatManager mgr, Player sender, String msg) {
    this.mgr = mgr;
    this.msg = msg;
    p = sender;
  }

  public void executeCommand() {
    PrivateChatter chat = mgr.getChatterFor(p);
    if (chat == null) {
      return;
    }

    Window f = SwingUtilities.getWindowAncestor(chat);
    if (!f.isVisible()) {
      f.setVisible(true);
      Component c = KeyboardFocusManager.getCurrentKeyboardFocusManager()
                                        .getFocusOwner();
      if (c == null || !SwingUtilities.isDescendingFrom(c, f)) {
        java.awt.Toolkit.getDefaultToolkit().beep();
        for (int i = 0,j = chat.getComponentCount(); i < j; ++i) {
          if (chat.getComponent(i) instanceof JTextField) {
            (chat.getComponent(i)).requestFocus();
            break;
          }
        }
      }
    }
    else {
      f.toFront();
    }
    chat.show(msg);
  }

  public Command myUndoCommand() {
    return null;
  }

  /**
   * Return true, as this command should not be logged
   */
  public boolean isLoggable() {
    return false;
  }

  public Player getSender() {
    return p;
  }

  public String getMessage() {
    return msg;
  }
}
