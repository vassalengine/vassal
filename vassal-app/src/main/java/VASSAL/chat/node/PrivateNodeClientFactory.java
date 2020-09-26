package VASSAL.chat.node;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.CommandDecoder;
import VASSAL.chat.DummyMessageServer;
import VASSAL.chat.ui.ChatServerControls;

public class PrivateNodeClientFactory extends NodeClientFactory {
  public static final String PRIVATE_TYPE = "private"; //$NON-NLS-1$
  public static final String PRIVATE_HOST = "localhost";
  public static final String PRIVATE_PORT = "5050";


  private static final Logger logger =
    LoggerFactory.getLogger(PrivateNodeClientFactory.class);

  @Override
  public ChatServerConnection buildServer(Properties param) {
    final String host = param.getProperty(NODE_HOST, PRIVATE_HOST);
    final int port = Integer.parseInt(param.getProperty(NODE_PORT, PRIVATE_PORT));

    final DummyMessageServer msgServer = new DummyMessageServer();

    final GameModule g = GameModule.getGameModule();

    final NodeClient server = new NodeClient(
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
        serverStatusControls.uninitializeControls(controls);
      }
    };

    g.getPrefs().getOption(GameModule.REAL_NAME).fireUpdate();
    g.getPrefs().getOption(GameModule.PERSONAL_INFO).fireUpdate();

    server.addPropertyChangeListener(ChatServerConnection.STATUS, e -> {
      final String mess = (String) e.getNewValue();
      GameModule.getGameModule().warn(mess);
      logger.error("", mess);
    });

    server.addPropertyChangeListener(ChatServerConnection.INCOMING_MSG, new CommandDecoder());

    return server;
  }
}
