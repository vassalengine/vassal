package VASSAL.chat.ui;

import javax.swing.JButton;
import VASSAL.chat.ChatServerConnection;

public class ServerStatusControlsInitializer implements ChatControlsInitializer {
  private JButton showStatusButton;
  private ChatServerConnection client;

  public ServerStatusControlsInitializer(ChatServerConnection client) {
    super();
    this.client = client;
  }

  public void initializeControls(ChatServerControls controls) {
    showStatusButton = controls.getToolbar().add(new ShowServerStatusAction(client, getClass().getResource("/images/status.gif")));
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(showStatusButton);
  }
}
