/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;

/**
 * A {@link Command} that, when executed, sends game synchronization
 * information to a given {@link VASSAL.chat.SimplePlayer}
 *
 */
public class SynchCommand extends Command {
  private Player recipient;
  private ChatServerConnection client;

  public SynchCommand(Player p, ChatServerConnection client) {
    recipient = p;
    this.client = client;
  }

  public Player getPlayer() {
    return recipient;
  }

  protected void executeCommand() {
    if (recipient != null) {
      GameModule.getGameModule().warn(Resources.getString("Server.sending_game_info", recipient.getName())); //$NON-NLS-1$
      Command synch = GameModule.getGameModule().getGameState().getRestoreCommand();
      if (synch != null) {
        client.sendTo(recipient, synch);
      }
    }
  }

  protected Command myUndoCommand() {
    return null;
  }

  /**
   * Don't log synchronization requests */
  public boolean isLoggable() {
    return false;
  }
}
