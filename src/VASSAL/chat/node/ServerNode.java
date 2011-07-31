/*
 * $Id$
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
package VASSAL.chat.node;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Logger;

import VASSAL.tools.ArrayUtils;
import VASSAL.tools.PropertiesEncoder;
import VASSAL.tools.SequenceEncoder;

public class ServerNode extends Node {
  private static final Logger logger = Logger.getLogger(ServerNode.class.getName());
  private SendContentsTask sendContents;

  public ServerNode() {
    super(null, null, null);
    sendContents = new SendContentsTask();
    Timer t = new Timer();
    t.schedule(sendContents, 0, 1000);
  }

  public synchronized void forward(String senderPath, String msg) {
    MsgSender target = getMsgSender(senderPath);
    target.send(msg);
  }

  public synchronized MsgSender getMsgSender(String path) {
    Node[] target = new Node[]{this};
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(path, '/');
    while (st.hasMoreTokens()) {
      String childId = st.nextToken();
      if ("*".equals(childId)) { //$NON-NLS-1$
        ArrayList<Node> l = new ArrayList<Node>();
        for (Node node : target) {
          l.addAll(Arrays.asList(node.getChildren()));
        }
        target = l.toArray(new Node[l.size()]);
      }
      else if (childId.startsWith("~")) { //$NON-NLS-1$
        childId = childId.substring(1);
        ArrayList<Node> l = new ArrayList<Node>();
        for (Node node : target) {
          for (Node child : node.getChildren()) {
            if (!childId.equals(child.getId())) {
              l.add(child);
            }
          }
        }
        target = l.toArray(new Node[l.size()]);
      }
      else {
        int i = 0;
        int n = target.length;
        for (; i < n; ++i) {
          Node child = target[i].getChild(childId);
          if (child != null) {
            target = new Node[]{child};
            break;
          }
        }
        if (i == n) {
          target = new Node[0];
        }
      }
    }

    final MsgSender[] senders = ArrayUtils.copyOf(target);

    return new MsgSender() {
      public void send(String msg) {
        for (int i = 0; i < senders.length; ++i) {
          senders[i].send(msg);
        }
      }
    };
  }

  public synchronized void disconnect(Node target) {
    Node mod = getModule(target);
    if (mod != null) {
      Node room = target.getParent();
      room.remove(target);
      if (room.getChildren().length == 0) {
        room.getParent().remove(room);
      }
      if (mod.getChildren().length == 0) {
        remove(mod);
      }
      sendContents(mod);
    }
  }

  protected synchronized void sendContents(Node module) {
    sendContents.markChanged(module);
  }

  public synchronized void registerNode(String parentPath, Node newNode) {
    Node newParent = Node.build(this, parentPath);
    newParent.add(newNode);
    Node module = getModule(newParent);
    if (module != null) {
      sendContents(module);
    }
  }

  public Node getModule(Node n) {
    Node module = n;
    while (module != null && module.getParent() != this) {
      module = module.getParent();
    }
    return module;
  }

  public synchronized void move(Node target, String newParentPath) {
    Node oldMod = getModule(target);
    Node newParent = Node.build(this, newParentPath);
    newParent.add(target);
    Node mod = getModule(newParent);
    if (mod != null) {
      sendContents(mod);
    }
    if (oldMod != mod && oldMod != null) {
      sendContents(oldMod);
    }
  }

  public synchronized void updateInfo(Node target) {
    Node mod = getModule(target);
    if (mod != null) {
      sendContents(mod);
    }
  }

  /**
   * One client has requested to kick another out of a room. Validate that - Both players are in the same room - Kicker
   * is the owner of the room
   *
   * @param kickerId
   *          Id of Kicking player
   * @param kickeeId
   *          Id of Player to be kicked
   */
  public synchronized void kick(PlayerNode kicker, String kickeeId) {
    // Check the kicker owns the room he is in
    final Node roomNode = kicker.getParent();
    String roomOwnerId;
    try {
      roomOwnerId = new PropertiesEncoder(roomNode.getInfo()).getProperties().getProperty(NodeRoom.OWNER);
    }
    catch (IOException e) {
      e.printStackTrace();
      return;
    }
    if (roomOwnerId == null || !roomOwnerId.equals(kicker.getId())) {
      return;
    }
    // Check the kickee belongs to the same room
    final Node kickeeNode = roomNode.getChild(kickeeId);
    if (kickeeNode == null) {
      return;
    }
    // Kick to the default room and tell them they have been kicked
    final Node defaultRoomNode = roomNode.getParent().getChildren()[0];
    move(kickeeNode, defaultRoomNode.getPath());
  }
  private static class SendContentsTask extends TimerTask {
    // FIXME: should modules be wrapped by Collections.synchronizedMap()?
    private Set<Node> modules = new HashSet<Node>();

    public void markChanged(Node module) {
      synchronized (modules) {
        modules.add(module);
      }
    }

    public void run() {
      HashSet<Node> s = new HashSet<Node>();
      synchronized (modules) {
        s.addAll(modules);
      }
      for (Node module : s) {
        logger.fine("Sending contents of " + module.getId()); //$NON-NLS-1$
        Node[] players = module.getLeafDescendants();
        Node[] rooms = module.getChildren();
        String listCommand = Protocol.encodeListCommand(players);
        module.send(listCommand);
        logger.finer(listCommand);
        String roomInfo = Protocol.encodeRoomsInfo(rooms);
        module.send(roomInfo);
        logger.finer(roomInfo);
      }
      synchronized (modules) {
        modules.clear();
      }
    }
  }
}
