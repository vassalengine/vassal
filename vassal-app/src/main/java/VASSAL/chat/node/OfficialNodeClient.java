package VASSAL.chat.node;

import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.MessageBoardControlsInitializer;
import VASSAL.chat.ui.ServerStatusControlsInitializer;
import VASSAL.command.CommandEncoder;
import VASSAL.i18n.Resources;

public class OfficialNodeClient extends NodeClient {
  private final MessageBoardControlsInitializer messageBoardControls;
  private final ServerStatusControlsInitializer serverStatusControls;

  public OfficialNodeClient(String moduleName, String playerId, CommandEncoder encoder, String host, int port, MessageBoard msgSvr, WelcomeMessageServer welcomer) {
    super(moduleName, playerId, encoder, host, port, welcomer);

    messageBoardControls = new MessageBoardControlsInitializer(
      Resources.getString("Chat.messages"), msgSvr //$NON-NLS-1$
    );
    serverStatusControls = new ServerStatusControlsInitializer(new CgiServerStatus());
  }

  @Override
  public void initializeControls(ChatServerControls controls) {
    super.initializeControls(controls);
    messageBoardControls.initializeControls(controls);
    serverStatusControls.initializeControls(controls);
  }

  @Override
  public void uninitializeControls(ChatServerControls controls) {
    super.uninitializeControls(controls);
    messageBoardControls.uninitializeControls(controls);
    serverStatusControls.uninitializeControls(controls);
  }
}
