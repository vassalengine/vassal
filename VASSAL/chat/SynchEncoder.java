package VASSAL.chat;

import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

/**
 * Date: Mar 16, 2003
 */
public class SynchEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "SYNC";
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
