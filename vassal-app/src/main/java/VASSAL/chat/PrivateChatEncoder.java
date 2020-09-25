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
import VASSAL.tools.SequenceEncoder;

/**
 *  Date: Mar 16, 2003
 */
public class PrivateChatEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "PRIV_CHAT"; //$NON-NLS-1$

  private final PlayerEncoder playerEncoder;
  private final PrivateChatManager pChatMgr;

  public PrivateChatEncoder(PlayerEncoder playerEncoder, PrivateChatManager pChatMgr) {
    this.playerEncoder = playerEncoder;
    this.pChatMgr = pChatMgr;
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof PrivMsgCommand)) {
      return null;
    }
    final PrivMsgCommand cmd = (PrivMsgCommand) c;
    final SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, '/');
    se
      .append(playerEncoder.playerToString(cmd.getSender()))
      .append(cmd.getMessage());
    return se.getValue();
  }

  @Override
  public Command decode(String s) {
    if (!s.startsWith(COMMAND_PREFIX)) {
      return null;
    }
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '/');
    st.nextToken();
    final Player sender = playerEncoder.stringToPlayer(st.nextToken());
    return new PrivMsgCommand(pChatMgr, sender, st.nextToken());
  }
}
