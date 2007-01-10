package VASSAL.chat;

import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.tools.SequenceEncoder;

/**
 * Date: Mar 16, 2003
 */
public class PrivateChatEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "PRIV_CHAT";
  private PlayerEncoder playerEncoder;

  private PrivateChatManager pChatMgr;

  public PrivateChatEncoder(PlayerEncoder playerEncoder, PrivateChatManager pChatMgr) {
    this.playerEncoder = playerEncoder;
    this.pChatMgr = pChatMgr;
  }

  public String encode(Command c) {
    if (c instanceof PrivMsgCommand) {
      PrivMsgCommand cmd = (PrivMsgCommand) c;
      SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, '/');
      se.append(playerEncoder.playerToString(cmd.getSender()));
      se.append(cmd.getMessage());
      return se.getValue();
    }
    else {
      return null;
    }
  }

  public Command decode(String s) {
    if (s.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '/');
      st.nextToken();
      Player sender = playerEncoder.stringToPlayer(st.nextToken());
      return new PrivMsgCommand(pChatMgr, sender, st.nextToken());
    }
    else {
      return null;
    }
  }

}
