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
 * Date: May 14, 2003
 */
package VASSAL.chat.node;

import java.io.IOException;
import java.util.Properties;

import VASSAL.tools.PropertiesEncoder;
import VASSAL.tools.SequenceEncoder;

/**
 * Utility method for encoding server-related commands into strings. Messages
 * sent or interpreted by the server are encoded here. Messages sent from one
 * client to another are simply forwarded as strings without being decoded.
 */
public class Protocol {
  public static final String REGISTER = "REG\t"; //$NON-NLS-1$
  public static final String REG_REQUEST = "REG_REQUEST\t"; //$NON-NLS-1$
  public static final String JOIN = "JOIN\t"; //$NON-NLS-1$
  public static final String FORWARD = "FWD\t"; //$NON-NLS-1$
  public static final String STATS = "STATS\t"; //$NON-NLS-1$
  public static final String LIST = "LIST\t"; //$NON-NLS-1$
  public static final String CONTENTS = "SERVER_CONTENTS\t"; //$NON-NLS-1$
  public static final String NODE_INFO = "NODE_INFO\t"; //$NON-NLS-1$
  public static final String ROOM_INFO = "ROOM_INFO\t"; //$NON-NLS-1$
  public static final String LOGIN = "LOGIN\t"; //$NON-NLS-1$
  public static final String KICK = "KICK\t"; //$NON-NLS-1$

  /**
   * Contains registration information sent when a client initially connects to
   * the server
   *
   * @param id
   * @param initialPath
   * @param info
   * @return
   */
  public static String encodeRegisterCommand(String id, String initialPath, String info) {
    String msg = new SequenceEncoder(id, '\t').append(initialPath).append(info).getValue();
    return REGISTER + msg;
  }

  /**
   * @see #encodeRegisterCommand
   * @return senderId, newParentPath, senderInfo
   */
  public static String[] decodeRegisterCommand(String cmd) {
    String[] info = null;
    if (cmd.startsWith(REGISTER)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(cmd.substring(REGISTER.length()), '\t');
      info = new String[] {st.nextToken(), st.nextToken(), st.nextToken()};
    }
    return info;
  }

  /**
   * Sent when a player wishes to join a room
   *
   * @param newParentPath
   * @return
   */
  public static String encodeJoinCommand(String newParentPath) {
    return JOIN + newParentPath;
  }

  /**
   * Sent when a player is invited to join a locked room
   *
   * @param newParentPath
   * @param password
   * @return
   */
  public static String encodeJoinCommand(String newParentPath, String password) {
    return JOIN + newParentPath + "\t" + password;
  }

  /**
   * @see #encodeJoinCommand
   * @return newParentPath
   */
  public static String[] decodeJoinCommand(String cmd) {
    String[] info = null;
    if (cmd.startsWith(JOIN)) {
      final String[] parts = cmd.split("\\t");
      if (parts.length == 2) {
        info = new String[] {parts[1]};
      }
      else if (parts.length == 3) {
        info = new String[] {parts[1], parts[2]};
      }
    }
    return info;
  }

  /**
   * Forward a message to other client nodes
   *
   * @param recipientPath
   *          a path name specifying the indented recipients of the message
   * @param message
   * @return
   */
  public static String encodeForwardCommand(String recipientPath, String message) {
    String msg = new SequenceEncoder(recipientPath, '\t').append(message).getValue();
    return FORWARD + msg;
  }

  /**
   * @see #encodeForwardCommand
   * @return recipientPath, message
   */
  public static String[] decodeForwardCommand(String cmd) {
    String[] info = null;
    if (cmd.startsWith(FORWARD)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(cmd.substring(FORWARD.length()), '\t');
      info = new String[] {st.nextToken(), st.nextToken()};
    }
    return info;
  }

  /**
   * Sent when a player updates his personal information
   *
   * @param info
   *          the encoded properties of the {@link PlayerNode} corresponding to
   *          the player
   * @see Node#setInfo
   * @return
   */
  public static String encodeStatsCommand(String info) {
    return STATS + info;
  }

  /**
   * @see #encodeStatsCommand
   * @return path, playerProperties
   */
  public static String[] decodeStatsCommand(String cmd) {
    String[] info = null;
    if (cmd.startsWith(STATS)) {
      info = new String[] {cmd.substring(STATS.length())};
    }
    return info;
  }

  /**
   * Sent when the info for a particular (non-player) node is updated
   *
   * @param n
   * @return
   */
  public static String encodeNodeInfoCommand(Node n) {
    String info = n.getInfo();
    if (info == null) {
      info = ""; //$NON-NLS-1$
    }
    return NODE_INFO + new SequenceEncoder(n.getPath(), '=').append(info).getValue();
  }

  /**
   *
   * @param cmd
   * @return path, info
   * @see #encodeNodeInfoCommand
   */
  public static String[] decodeNodeInfoCommand(String cmd) {
    String s[] = null;
    if (cmd.startsWith(NODE_INFO)) {
      s = new String[2];
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(cmd.substring(NODE_INFO.length()), '=');
      s[0] = st.nextToken();
      s[1] = st.nextToken();
    }
    return s;
  }

  /**
   * Sent when a room's info changes, or to update all rooms' info at once
   *
   * @param rooms
   * @return
   */
  public static String encodeRoomsInfo(Node[] rooms) {
    Properties p = new Properties();
    for (int i = 0; i < rooms.length; ++i) {
      if (rooms[i].getInfo() != null && rooms[i].getInfo().length() > 0)
        p.setProperty(rooms[i].getId(), rooms[i].getInfo());
    }
    String value = new PropertiesEncoder(p).getStringValue();
    return value == null ? ROOM_INFO : ROOM_INFO + value;
  }

  public static Properties decodeRoomsInfo(String cmd) {
    Properties p = null;
    if (cmd.startsWith(ROOM_INFO)) {
      try {
        p = new PropertiesEncoder(cmd.substring(ROOM_INFO.length())).getProperties();
      }
      // FIXME: review error message
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    return p;
  }

  /**
   * A dump of the current connections to the server. Includes a path name and
   * info for each player node
   *
   * @param nodes
   * @return
   */
  public static String encodeListCommand(Node[] nodes) {
    SequenceEncoder list = new SequenceEncoder('\t');
    for (int i = 0; i < nodes.length; ++i) {
      if (nodes[i].getPath() != null && nodes[i].getInfo() != null) {
        SequenceEncoder info = new SequenceEncoder('=');
        info.append(nodes[i].getPath()).append(nodes[i].getInfo());
        list.append(info.getValue());
      }
    }
    String value = list.getValue();
    return value == null ? LIST : LIST + value;
  }

  /**
   * @see #encodeListCommand
   * @param cmd
   * @return
   */
  public static Node decodeListCommand(String cmd) {
    Node node = null;
    if (cmd.startsWith(LIST)) {
      Node root = new Node(null, null, null);
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(cmd.substring(LIST.length()), '\t');
      while (st.hasMoreTokens()) {
        String nodeInfo = st.nextToken();
        SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(nodeInfo, '=');
        String path = st2.nextToken();
        String info = st2.nextToken();
        Node.build(root, path).setInfo(info);
      }
      node = root;
    }
    return node;
  }

  public static boolean decodeRegisterRequest(String cmd) {
    return cmd.startsWith(REG_REQUEST);
  }

  public static String encodeRegisterRequest() {
    return REG_REQUEST;
  }

  /**
   * Sent when associating a connection with a given username
   *
   * @param username
   * @see ConnectionLimiter
   */
  public static String encodeLoginCommand(String username) {
    return LOGIN + username;
  }

  public static String decodeLoginCommand(String cmd) {
    String username = null;
    if (cmd.startsWith(LOGIN)) {
      username = cmd.substring(LOGIN.length());
    }
    return username;
  }

  /**
   * Sent by the owner of a room to kick another player back to the Main Room
   *
   * @param p
   * @return
   */
  public static String encodeKickCommand(String kickeeId) {
    return KICK + kickeeId;
  }

  public static String[] decodeKickCommand(String cmd) {
    String[] player = null;
    if (cmd.startsWith(KICK)) {
      player = new String[] {cmd.substring(KICK.length())};
    }
    return player;
  }

  /**
   * A dump of the current connections to the server. Includes a path name and
   * info for each player node, and info for each room node as well
   *
   * @param nodes
   * @return public static String encodeContentsCommand(Node[] nodes) {
   *         SequenceEncoder list = new SequenceEncoder('\t'); for (int i = 0; i <
   *         nodes.length; ++i) { list.append(nodes[i].getPathAndInfo()); }
   *         return CONTENTS + list.getValue(); }
   */

  /**
   * @see #encodeContentsCommand
   * @param cmd
   * @return public static Node decodeContentsCommand(String cmd) { Node node =
   *         null; if (cmd.startsWith(CONTENTS)) { Node root = new Node(null,
   *         null, null); SequenceEncoder.Decoder st = new
   *         SequenceEncoder.Decoder(cmd.substring(CONTENTS.length()), '\t');
   *         while (st.hasMoreTokens()) { String pathAndInfo = st.nextToken();
   *         Node.build(root, pathAndInfo); } node = root; } return node; }
   */
}
