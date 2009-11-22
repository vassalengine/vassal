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
public class InviteEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "INVITE\t"; //$NON-NLS-1$
  private ChatServerConnection client;

  public InviteEncoder(ChatServerConnection client) {
    this.client = client;
  }

  public Command decode(String s) {
    if (s.startsWith(COMMAND_PREFIX)) {
      String[] info = s.split("\\t"); //$NON-NLS-1$
      if (info.length == 4) {
        return new InviteCommand(info[1], info[2], info[3], client);
      }
    }
    return null;
  }

  public String encode(Command c) {
    if (c instanceof InviteCommand) {
      InviteCommand cmd = (InviteCommand) c;
      return COMMAND_PREFIX + cmd.getPlayer() + "\t" + cmd.getPlayerId() + "\t" + cmd.getRoom(); //$NON-NLS-1$ //$NON-NLS-2$
    }
    else {
      return null;
    }
  }

}
