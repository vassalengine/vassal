package VASSAL.chat.node;

import java.util.Properties;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.DummyMessageServer;
import VASSAL.chat.ui.ChatServerControls;

public class PrivateNodeClientFactory extends NodeClientFactory {
  public static final String PRIVATE_TYPE = "private"; //$NON-NLS-1$
  public static final String PRIVATE_HOST = "localhost";
  public static final String PRIVATE_PORT = "5050";

  @Override
  protected ChatServerConnection buildServerImpl(Properties param) {
    final String host = param.getProperty(NODE_HOST, PRIVATE_HOST);
    final int port = Integer.parseInt(param.getProperty(NODE_PORT, PRIVATE_PORT));

    final DummyMessageServer msgServer = new DummyMessageServer();

    final GameModule g = GameModule.getGameModule();

    return new PrivateNodeClient(
      g.getGameName(),
      GameModule.getUserId() + "." + System.currentTimeMillis(),
      g,
      host,
      port,
      msgServer,
      msgServer
    ) {
      @Override
      public void initializeControls(ChatServerControls controls) {
        super.initializeControls(controls);
        messageBoardControls.uninitializeControls(controls);
      }
    };
  }
}
