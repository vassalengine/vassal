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

import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

/**
 * Date: Mar 16, 2003
 */
public class SynchEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "SYNC"; //$NON-NLS-1$
  private PlayerEncoder playerEncoder;
  private ChatServerConnection client;

  public SynchEncoder(PlayerEncoder playerEncoder, ChatServerConnection client) {
    this.playerEncoder = playerEncoder;
    this.client = client;
  }

  public Command decode(String s) {
    if (s.startsWith(COMMAND_PREFIX)) {
      Player p = playerEncoder.stringToPlayer(s.substring(COMMAND_PREFIX.length()));
      return new SynchCommand(p,client);
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof SynchCommand) {
      SynchCommand cmd = (SynchCommand) c;
      return COMMAND_PREFIX + playerEncoder.playerToString(cmd.getPlayer());
    }
    else {
      return null;
    }
  }

}
