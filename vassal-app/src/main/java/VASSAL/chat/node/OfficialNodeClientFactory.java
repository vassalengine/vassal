package VASSAL.chat.node;

import java.util.Properties;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.HttpMessageServer;
import VASSAL.chat.peer2peer.PeerPoolInfo;
import VASSAL.i18n.Resources;

public class OfficialNodeClientFactory extends NodeClientFactory {
  public static final String OFFICIAL_TYPE = "official"; //$NON-NLS-1$
  public static final String OFFICIAL_HOST = "game.vassalengine.org";
  public static final String OFFICIAL_PORT = "5050";

  private static final String UNNAMED_MODULE = Resources.getString("Chat.unknown_module");  //$NON-NLS-1$
  private static final String UNKNOWN_USER = Resources.getString("Chat.unknown_user");  //$NON-NLS-1$

  @Override
  protected ChatServerConnection buildServerImpl(Properties param) {
    final String host = param.getProperty(NODE_HOST, OFFICIAL_HOST);  //$NON-NLS-1$
    final int port = Integer.parseInt(param.getProperty(NODE_PORT, OFFICIAL_PORT));  //$NON-NLS-1$

    final PeerPoolInfo publicInfo = new PeerPoolInfo() {
      @Override
      public String getModuleName() {
        final GameModule g = GameModule.getGameModule();
        return g == null ? UNNAMED_MODULE : g.getGameName();
      }

      @Override
      public String getUserName() {
        final GameModule g = GameModule.getGameModule();
        return g == null ? UNKNOWN_USER : (String) g.getPrefs().getValue(GameModule.REAL_NAME);
      }
    };

    final HttpMessageServer httpMessageServer = new HttpMessageServer(publicInfo);
    final GameModule g = GameModule.getGameModule();

    return new OfficialNodeClient(
      g.getGameName(),
      GameModule.getUserId() + "." + System.currentTimeMillis(),
      g,
      host,
      port,
      httpMessageServer,
      httpMessageServer
    );
  }
}
