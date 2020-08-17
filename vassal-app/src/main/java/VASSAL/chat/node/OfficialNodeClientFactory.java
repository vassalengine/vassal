package VASSAL.chat.node;

import java.util.Properties;

import VASSAL.chat.ChatServerConnection;

public class OfficialNodeClientFactory extends NodeClientFactory {
  public static final String OFFICIAL_TYPE = "official"; //$NON-NLS-1$

  public static final String OFFICIAL_HOST = "game.vassalengine.org";
  public static final String OFFICIAL_PORT = "5050";

  @Override
  public ChatServerConnection buildServer(Properties param) {
    param.setProperty(NodeClientFactory.NODE_HOST, OFFICIAL_HOST);
    param.setProperty(NodeClientFactory.NODE_PORT, OFFICIAL_PORT);
    return super.buildServer(param);
  }
}
