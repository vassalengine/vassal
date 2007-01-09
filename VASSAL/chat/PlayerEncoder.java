package VASSAL.chat;



/**
 * Date: Mar 16, 2003
 */
public interface PlayerEncoder {
  public Player stringToPlayer(String s);
  public String playerToString(Player p);
}
