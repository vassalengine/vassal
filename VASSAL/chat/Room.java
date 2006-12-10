package VASSAL.chat;

import java.util.List;

/**
 * Represents a game room on the server
 * @author rkinney
 *
 */
public interface Room {
  /** The display name of this room */
  String getName();
  /** Return an (unmodifiable) list of players */
  List getPlayerList();
  /** Add a player to this room */
  void addPlayer(Player p);
  /** Remove a player from this room */
  void removePlayer(Player p);
}
