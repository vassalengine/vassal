package org.litesoft.p2pchat;

import java.io.IOException;

// Copyright Status:
//
// All Software available from LiteSoft.org (including this file) is
// hereby released into the public domain.
//
// It is free!  As in, you may use it freely in both commercial and
// non-commercial applications, bundle it with your software
// distribution, include it on a CD-ROM, list the source code in a book,
// mirror the documentation at your own web site, or use it in any other
// way you see fit.
//
// NO Warranty!
//
// All software is provided "as is".
//
// There is ABSOLUTELY NO WARRANTY OF ANY KIND: not for the design, fitness
// (for a particular purpose), level of errors (or lack thereof), or
// applicability of this software.  The entire risk as to the quality
// and performance of this software is with you.  Should this software
// prove defective, you assume the cost of all necessary servicing, repair
// or correction.
//
// In no event unless required by applicable law or agreed to in writing
// will any party who created or may modify and/or redistribute this
// software, be liable to you for damages, including any general,
// special, incidental or consequential damages arising out of the use or
// inability to use this software (including but not limited to loss of
// data or data being rendered inaccurate or losses sustained by you or
// third parties or a failure of this software to operate with any
// other programs), even if such holder or other party has been advised
// of the possibility of such damages.
//
// NOTE: Should you discover a bug, have a recogmendation for a change, wish
// to submit modifications, or wish to add new classes/functionality,
// please email them to:
//
//        changes@litesoft.org
//

/**
 * @author  Devin Smith and George Smith
 * @version 0.4 04/07/13 Added support for Network Password
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */
public class PeerInfo {
  private String zID = "";
  private String zChatName;
  private String zAddresses;
  private Integer zPort = null;
  private String[] zOldChatNames = null;  
  private static final String NO_CHATNAME = "(?)";
  private int failureCount = 0;
  private String zNetworkPw = "";
  public PeerInfo(String pChatName, String pAddresses) {
    zChatName = (pChatName != null) ? pChatName : NO_CHATNAME;
    IllegalArgument.ifNull("Addresses", zAddresses = pAddresses);
  }

  public PeerInfo(String pChatName, String pAddresses, int pPort) {
    this(pChatName, pAddresses);
    zPort = pPort;
  }
  
  public PeerInfo(String pChatName, String pAddresses, int pPort, String pNetworkPw) {
    this (pChatName, pAddresses, pPort);
    zNetworkPw = pNetworkPw;
  }

  public void updateWith(PeerInfo pPeerInfo) {
    if (pPeerInfo != null) {
      setChatName(pPeerInfo.getChatName());
      setAddresses(pPeerInfo.getAddresses());
      setPort(pPeerInfo.getPort());
      setNetworkPw(pPeerInfo.getNetworkPw());
    }
  }

  public static PeerInfo deFormat(String pFormatted) {
    IllegalArgument.ifNull("Formatted", pFormatted);
    int colonAt = pFormatted.indexOf(':');
    int spaceAt = pFormatted.indexOf(' ');
    if (colonAt == -1)
      return null;
    String chatName = null;
    if (spaceAt == -1) {
      spaceAt = pFormatted.length();
    }
    else {
      chatName = pFormatted.substring(spaceAt + 1);
    }
    String addresses = pFormatted.substring(0, colonAt);
    
//    int port = 0;
//    try {
//      port = Integer.parseInt(pFormatted.substring(colonAt + 1, spaceAt));
//    }
//    catch (NumberFormatException shouldNotHappenSoWeIgnoreThisMessage) {
//      return null;
//    }
//    return new PeerInfo(chatName, addresses, port);
    
    final String rest = pFormatted.substring(colonAt+1, spaceAt);
    final int slashAt = rest.indexOf('/');
    String portStr, networkPw;
    if (slashAt == -1) {
      portStr = rest;
      networkPw = "";
    }
    else {
      portStr = rest.substring(0, slashAt);
      networkPw = rest.substring(slashAt+1);
    }
    
    int port = 0;
    try {
      port = Integer.parseInt(portStr);
    }
    catch (NumberFormatException shouldNotHappenSoWeIgnoreThisMessage) {
      return null;
    }
    return new PeerInfo(chatName, addresses, port, networkPw);
    
  }

  public boolean isAddressable() {
    return ((zAddresses != null) && (zPort != null));
  }

  public String getID() {
    return zID;
  }

  public void setID(String pID) {
    IllegalArgument.ifNull("ID", zID = pID);
  }

  public String getChatName() {
    return zChatName;
  }

  public String[] getOldChatNames() {
    return zOldChatNames;
  }

  public synchronized String getPrevChatName() {
    return (zOldChatNames == null) ? null : zOldChatNames[0];
  }

  public synchronized void setChatName(String pChatName) {
    if ((pChatName != null) && !pChatName.equals(zChatName)) {
      if ((zChatName != null) && !zChatName.equals(NO_CHATNAME)) {
        if (zOldChatNames == null)
          zOldChatNames = new String[1];
        else {
          String[] temp = new String[zOldChatNames.length + 1];
          System.arraycopy(zOldChatNames, 0, temp, 1, zOldChatNames.length);
          zOldChatNames = temp;
        }
        zOldChatNames[0] = zChatName;
      }
      zChatName = pChatName;
    }
  }

  public String getAddresses() {
    return zAddresses;
  }

  public void setAddresses(String pAddresses) {
    IllegalArgument.ifNull("Addresses", zAddresses = pAddresses);
  }

  public int getPort() {
    return (zPort == null) ? 0 : zPort.intValue();
  }

  public void setPort(int pPort) {
    zPort = pPort;
  }

  public String format() {
    return zAddresses + ":" + zPort + "/" + zNetworkPw + " " + zChatName;
  }
  
  public void setNetworkPw(String pw) {
    zNetworkPw = pw;
  }
  
  public String getNetworkPw () {
    return zNetworkPw;
  }

/*
    public String getAddress() {
        return getAddresses() + ":" + getPort();
    }
*/

  public int hashCode() {
    int retval = 0;
    if (zAddresses != null)
      retval += zAddresses.hashCode();
    if (zPort != null)
      retval += zPort.hashCode();
    return retval;
  }

  public boolean equals(String pAddresses, Integer pPort) {
    if (zAddresses != pAddresses) {
      if (zAddresses == null)
        return false;
      if (zAddresses.equals(pAddresses) == false)
        return false;
    }
    if (zPort != pPort) {
      if (zPort == null)
        return false;
      if (zPort.equals(pPort) == false)
        return false;
    }
    return true;
  }

  public boolean equals(PeerInfo pOther) {
    return (pOther == null) ? false : equals(pOther.zAddresses, pOther.zPort);
  }

  public boolean equals(Object obj) {
    if (obj instanceof PeerInfo) // Note: Use of instanceof is so that MyInfo can be compared.
      return equals((PeerInfo) obj);
    return false;
  }

  public String toString() {
    String retval = zID + " " + zChatName + " " + zAddresses + ":" + ((zPort != null) ? zPort.toString() : "?");

    String[] oldnames = getOldChatNames();
    if (oldnames != null)
      for (int i = 0; i < oldnames.length; i++)
        retval += " -> " + oldnames[i];

    return retval;
  }

  public int incrementFailureCount() {
    return ++failureCount;
  }

  public void setFailureReason(IOException failureReason) {
  }
}
