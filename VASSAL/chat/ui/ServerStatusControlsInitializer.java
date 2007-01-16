package VASSAL.chat.ui;

import javax.swing.JButton;
import VASSAL.chat.ServerStatus;

public class ServerStatusControlsInitializer implements ChatControlsInitializer {
  private JButton showStatusButton;
  private ServerStatus status;

  public ServerStatusControlsInitializer(ServerStatus status) {
    super();
    this.status = status;
  }

  public void initializeControls(ChatServerControls controls) {
    showStatusButton = controls.getToolbar().add(new ShowServerStatusAction(status, getClass().getResource("/images/status.gif")));
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(showStatusButton);
  }
}
