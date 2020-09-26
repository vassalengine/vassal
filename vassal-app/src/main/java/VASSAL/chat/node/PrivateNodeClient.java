package VASSAL.chat.node;

import VASSAL.chat.DummyMessageServer;
import VASSAL.command.CommandEncoder;

public class PrivateNodeClient extends NodeClient {
  public PrivateNodeClient(String moduleName, String playerId, CommandEncoder encoder, String host, int port) {
    super(moduleName, playerId, encoder, host, port, new DummyMessageServer());
  }
}
