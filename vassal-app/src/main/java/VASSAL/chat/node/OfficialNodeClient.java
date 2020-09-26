package VASSAL.chat.node;

import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.command.CommandEncoder;

public class OfficialNodeClient extends NodeClient {

  public OfficialNodeClient(String moduleName, String playerId, CommandEncoder encoder, String host, int port, MessageBoard msgSvr, WelcomeMessageServer welcomer) {
    super(moduleName, playerId, encoder, host, port, msgSvr, welcomer);
  }
}
