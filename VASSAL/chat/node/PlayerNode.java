/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 9, 2003
 */
package VASSAL.chat.node;

import java.io.IOException;
import java.net.Socket;
import java.util.Properties;
import java.util.Iterator;

/**
 * Node representing a single player.
 * A leaf node in a hierarchical server.
 * Reads and writes directly to a socket
 * {@link #getInfo} returns an encoded {@link java.util.Properties} object with real name, profile, etc.
 */
public class PlayerNode extends Node implements SocketWatcher {
  private SocketHandler input;
  protected String id;
  protected String info;
  private AsynchronousServerNode server;
  private static ConnectionLimiter connLimiter = new ConnectionLimiter();

  public PlayerNode(Socket socket, AsynchronousServerNode server) throws IOException {
    super(null,null,null);
    this.server = server;
    this.input = new BufferedSocketHandler(socket,this);
    input.start();
  }

  public String getId() {
    return id;
  }

  public boolean isLeaf() {
    return true;
  }

  public void send(String msg) {
    input.writeLine(msg);
  }

  public String getInfo() {
    return info;
  }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof PlayerNode)) return false;

    final PlayerNode player = (PlayerNode) o;

    if (!id.equals(player.id)) return false;

    return true;
  }

  public int hashCode() {
    return id.hashCode();
  }

  public void handleMessage(String line) {
    String[] info;
    Properties p;
    String cmd;
    if ((info = Protocol.decodeRegisterCommand(line)) != null) {
      id = info[0];
      this.info = info[2];
      server.registerNode(info[1],this);
    }
    else if ((info = Protocol.decodeJoinCommand(line)) != null) {
      server.move(this,info[0]);
    }
    else if ((info = Protocol.decodeForwardCommand(line)) != null) {
      server.forward(info[0],info[1]);
    }
    else if ((info = Protocol.decodeStatsCommand(line)) != null) {
      this.info = info[0];
      server.updateInfo(this);
    }
    else if ((p = Protocol.decodeRoomsInfo(line)) != null) {
      for (Iterator it = p.keySet().iterator(); it.hasNext();) {
        String roomName = (String) it.next();
        Node target = server.getModule(this).getDescendant(roomName);
        if (target != null) {
          target.setInfo(p.getProperty(roomName));
          server.updateInfo(target);
        }
      }
    }
    else if ((cmd = Protocol.decodeLoginCommand(line)) != null) {
      connLimiter.register(cmd,input);
    }
  }

  public void socketClosed(SocketHandler handler) {
    server.disconnect(this);
  }
}
