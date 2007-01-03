package VASSAL.chat.peer2peer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import VASSAL.chat.Player;

/**
 * Tracks players entering/exiting a room
 */
public class RoomTracker {
  private List joinedPlayers;
  private List leftPlayers;

  public RoomTracker() {
    joinedPlayers = new ArrayList();
    leftPlayers = new ArrayList();
  }

  public void init(VASSAL.chat.Room r) {
    joinedPlayers.clear();
    leftPlayers.clear();
    Player[] p = (Player[]) r.getPlayerList().toArray();
    for (int i = 0; i < p.length; ++i) {
      leftPlayers.add(p[i]);
    }
  }

  public void finalize(VASSAL.chat.Room r) {
    Player[] players = (Player[]) r.getPlayerList().toArray();
    for (int i = 0; i < players.length; ++i) {
      if (!leftPlayers.contains(players[i])) {
        joinedPlayers.add(players[i]);
      }
      leftPlayers.remove(players[i]);
    }
  }

  public Enumeration getJoinedPlayers() {
    return Collections.enumeration(joinedPlayers);
  }

  public Enumeration getLeftPlayers() {
    return Collections.enumeration(leftPlayers);
  }
}
