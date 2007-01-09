/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 14, 2003
 */
package VASSAL.chat.node;

/** Generic interface for listening to a socket */
public interface SocketWatcher {
  public void handleMessage(String msg);
  public void socketClosed(SocketHandler handler);
}
