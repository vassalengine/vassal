package VASSAL.chat;

/**
 * Represents an occupant of a chat room, i.e. a VASSAL user connected to the server
 * @author rkinney
 *
 */
public interface Player {
  /** Unique identifier for this player */
  String getId();
  /** Display name (nickname) for the player */
  String getName();
  /** Return the current status of the player */
  PlayerStatus getStatus();
}
