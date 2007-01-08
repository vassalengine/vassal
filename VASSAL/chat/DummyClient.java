package VASSAL.chat;

import java.beans.PropertyChangeListener;
import VASSAL.command.Command;

/** 
 * Empty server
 * @author rkinney
 *
 */
public class DummyClient implements ChatServerConnection {
  private Player playerInfo = new SimplePlayer("<nobody>");


  public DummyClient() {
  }

  public Room[] getAvailableRooms() {
    return new Room[0];
  }

  public Room getRoom() {
    return null;
  }

  public ServerStatus getStatusServer() {
    return null;
  }

  public void sendTo(Player recipient, Command c) {
  }

  public void setRoom(Room r) {
  }

  public void addPropertyChangeListener(String propertyName, PropertyChangeListener l) {
  }

  public boolean isConnected() {
    return false;
  }

  public void sendToOthers(Command c) {
  }

  public void setConnected(boolean connect) {
  }

  public Player getUserInfo() {
    return playerInfo;
  }

  public void setUserInfo(Player playerInfo) {
    this.playerInfo = playerInfo;
  }

}
