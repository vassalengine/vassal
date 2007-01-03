package VASSAL.chat.peer2peer;

import org.litesoft.p2pchat.PeerInfo;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.Player;

import java.util.Enumeration;
import java.util.Vector;

public class RoomManager {
  private Vector rooms = new Vector();
  private SimpleRoom defaultRoom = new SimpleRoom("Main Room");

  public RoomManager() {
    rooms.addElement(defaultRoom);
  }

  public VASSAL.chat.Room[] update(PeerInfo info) {
    P2PPlayer p = new P2PPlayer(info);
    VASSAL.chat.Room oldRoom = getRoomContaining(p);
    VASSAL.chat.Room newRoom = new SimpleRoom(p.getRoom());
    if (rooms.contains(newRoom)) {
      newRoom = (VASSAL.chat.Room) rooms.elementAt(rooms.indexOf(newRoom));
    }
    else {
      rooms.addElement(newRoom);
    }
    newRoom.addPlayer(p);
    if (oldRoom != null
      && !oldRoom.equals(newRoom)) {
      oldRoom.removePlayer(p);
      if (oldRoom.getPlayerList().size() == 0
        && !oldRoom.equals(defaultRoom)) {
        rooms.removeElement(oldRoom);
      }
    }
    return getRooms();
  }

  public VASSAL.chat.Room[] remove(PeerInfo info) {
    P2PPlayer p = new P2PPlayer(info);
    for (int i = 0; i < rooms.size(); ++i) {
      VASSAL.chat.Room r = (VASSAL.chat.Room) rooms.elementAt(i);
      r.removePlayer(p);
      if (r.getPlayerList().size() == 0
        && !r.equals(defaultRoom)) {
        rooms.removeElementAt(i--);
      }
    }
    return getRooms();
  }

  public P2PPlayer getPlayerById(String id) {
    for (int i=0;i<rooms.size();++i){
      VASSAL.chat.Room r = (VASSAL.chat.Room)rooms.elementAt(i);
      VASSAL.chat.Player[] playerArray = (VASSAL.chat.Player[]) r.getPlayerList().toArray();
      for (int j=0;j<playerArray.length;++j) {
        P2PPlayer p = (P2PPlayer) playerArray[j];
        if (id.equals(p.getId())) {
          return p;
        }
      }
    }
    return null;
  }

  public VASSAL.chat.Room[] getRooms() {
//	System.err.println("--------");
    VASSAL.chat.Room[] r = new VASSAL.chat.Room[rooms.size()];
    for (int i = 0; i < r.length; ++i) {
      r[i] = (VASSAL.chat.Room) rooms.elementAt(i);
      //	    System.err.println("Room "+r[i]);
      //	    for (int j=0;j<r[i].players.length;++j) {
      //		System.err.println("  "+((P2PPlayer)r[i].players[j]).summary());
      //	    }
    }
    return r;
  }

  public SimpleRoom getRoomContaining(Player p) {
    for (Enumeration e = rooms.elements();
         e.hasMoreElements();) {
      SimpleRoom r = (SimpleRoom) e.nextElement();
      if (r.contains(p)) {
        return r;
      }
    }
    return null;
  }

  public void setDefaultRoomName(String name) {
    defaultRoom.setName(name);
  }

  public VASSAL.chat.Room getDefaultRoom() {
    return defaultRoom;
  }

  public void clear() {
    rooms.removeAllElements();
    defaultRoom.setPlayers(new Player[0]);
    rooms.addElement(defaultRoom);
  }
}
