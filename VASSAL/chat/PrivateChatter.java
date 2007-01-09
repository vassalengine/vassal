package VASSAL.chat;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.ServerConnection;

/**
 * A window for private messages between the user and another player;
 */
public class PrivateChatter extends Chatter {
  private ChatServerConnection client;
  private Player other;

  public PrivateChatter(Player other, ChatServerConnection client) {
    this.other = other;
    this.client = client;
  }

  /**
   * @deprecated
   */
  public String getHandle() {
    return GameModule.getGameModule().getChatter().getHandle();
  }

  public void send(String msg) {
    if (msg != null
      && msg.length() > 0) {
      show(msg);
      PrivMsgCommand c = new PrivMsgCommand(null, client.getUserInfo(), msg);
      client.sendTo(other, c);
    }
  }

  public Player getPlayer() {
    return other;
  }

  public ServerConnection getClient() {
    return client;
  }
}
