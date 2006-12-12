package VASSAL.chat.ui;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Room;
import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * When invoked, will join a game room on the server
 */
public class JoinRoomAction extends AbstractAction {
  private Room r;
  private ChatServerConnection client;

  public JoinRoomAction(Room r, ChatServerConnection client) {
    super("Join Room");
    this.r = r;
    this.client = client;
    setEnabled(r != null && !r.equals(client.getRoom()));
  }

  public void actionPerformed(ActionEvent evt) {
    client.setRoom(r);
  }
}
