package VASSAL.chat;

import VASSAL.build.module.ServerConnection;
import VASSAL.command.Command;

 /**
  * Specialized interface for chat-room server connections 
  * @author rkinney
  *
  */
public interface ChatServerConnection extends ServerConnection {
  /** Property representing the currently-occupied room */
  public static final String ROOM = "Room";
  /** Property representing the list of all rooms */
  public static final String AVAILABLE_ROOMS = "AvailableRooms";
  /** Property representing an informational message (e.g. "Connection succeeded")*/
  public static final String STATUS = "Status";
  /** Property representing the current player's information */
  public static final String PLAYER_INFO = "Player";
  /** Property representing a message received from the remove server */
  public static final String INCOMING_MSG = "Msg";
  /** Property representing the StatusServer implementation */
  public static final String STATUS_SERVER = "StatusServer";


  /** Return the room currently occupied by the player */
  Room getRoom();

  /** Join the given room */
  void setRoom(Room r);

  /** Return an array of all rooms on the server */
  Room[] getAvailableRooms();

  /** Return a Player instance representing the current player */
  Player getUserInfo();

  /** Set the Player instance representing this player */
  void setUserInfo(Player p);

  /** Send a Command to a particular player */
  void sendTo(Player recipient, Command c);

}
