package VASSAL.chat.node;

import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.ServerStatusControlsInitializer;
import VASSAL.command.CommandEncoder;

public class OfficialNodeClient extends NodeClient {
  private ServerStatusControlsInitializer serverStatusControls;

  public OfficialNodeClient(String moduleName, String playerId, CommandEncoder encoder, String host, int port, MessageBoard msgSvr, WelcomeMessageServer welcomer) {
    super(moduleName, playerId, encoder, host, port, msgSvr, welcomer);
    serverStatusControls = new ServerStatusControlsInitializer(new CgiServerStatus());
  }

  @Override
  public void initializeControls(ChatServerControls controls) {
    super.initializeControls(controls);
    serverStatusControls.initializeControls(controls);
  }

  @Override
  public void uninitializeControls(ChatServerControls controls) {
    super.uninitializeControls(controls);
    serverStatusControls.uninitializeControls(controls);
  }
}
