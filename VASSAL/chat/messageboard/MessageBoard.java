package VASSAL.chat.messageboard;


/**
 * Simple abstraction of a message board
 * Date: Mar 11, 2003
 */
public interface MessageBoard extends VASSAL.chat.MessageServer {
  public Message[] getMessages();

  public void postMessage(String msg);
}
