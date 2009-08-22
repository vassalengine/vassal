/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 9, 2003
 */
package VASSAL.chat.node;

import java.io.IOException;
import java.net.Socket;
import java.util.Iterator;
import java.util.Properties;

import VASSAL.tools.SequenceEncoder;

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

  // Always update IP on client info in case client 'forgets' their IP
  public String getInfo() {
    String ip = input.sock.getInetAddress().getHostAddress();
    return info + (ip.length() > 0 ? "|ip=" + ip : "");
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
      // Requests to move to a locked room must be accompanied by the rooms 'password' which
      // is the owner of the room. This allows 'Invited' clients to join Locked rooms.
      // Rooms are reused, so clients are allowed to enter an empty locked room without a password.
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(info[0], '/');
      sd.nextToken("");
      final String joinRoomName = sd.nextToken("");
      final Node room = server.getModule(this).getDescendant(joinRoomName);
      if (room != null) {
        final boolean locked = "true".equals(room.getInfoProperty(NodeRoom.LOCKED));
        if (locked && room.getChildren().length > 0) {
          final String owner = room.getInfoProperty(NodeRoom.OWNER);
          if (info.length < 2 || !owner.equals(info[1])) {
            return;
          }
        }
      }
      server.move(this,info[0]);
    }
    else if ((info = Protocol.decodeForwardCommand(line)) != null) {
      server.forward(info[0],info[1]);
    }
    else if ((info = Protocol.decodeStatsCommand(line)) != null) {
      this.info = info[0];
      server.updateInfo(this);
    }
    else if ((info = Protocol.decodeKickCommand(line)) != null) {
      server.kick(this, info[0]);
    }
    else if ((p = Protocol.decodeRoomsInfo(line)) != null) {
// FIXME: Use Properties.stringPropertyNames() in 1.6+.
      for (Iterator<Object> it = p.keySet().iterator(); it.hasNext();) {
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
