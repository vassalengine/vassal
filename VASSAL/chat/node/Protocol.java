/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 14, 2003
 */
package VASSAL.chat.node;

import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.PropertiesEncoder;

import java.util.Properties;
import java.io.IOException;

/**
 * Utility method for encoding server-related commands into strings. Messages
 * sent or interpreted by the server are encoded here. Messages sent from one
 * client to another are simply forwarded as strings without being decoded.
 */
public class Protocol {
  public static final String REGISTER = "REG\t";
  public static final String REG_REQUEST = "REG_REQUEST\t";
  public static final String JOIN = "JOIN\t";
  public static final String FORWARD = "FWD\t";
  public static final String STATS = "STATS\t";
  public static final String LIST = "LIST\t";
  public static final String CONTENTS = "SERVER_CONTENTS\t";
  public static final String NODE_INFO = "NODE_INFO\t";
  public static final String ROOM_INFO = "ROOM_INFO\t";
  public static final String LOGIN = "LOGIN\t";

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
   * @see #encodeJoinCommand
   * @return newParentPath
   */
  public static String[] decodeJoinCommand(String cmd) {
    String[] info = null;
    if (cmd.startsWith(JOIN)) {
      info = new String[] {cmd.substring(JOIN.length())};
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
      info = "";
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
