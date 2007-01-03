package org.litesoft.p2pchat;

import java.io.*;

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
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */
public class UserDialogConsole extends Thread implements UserDialog {
  private MyInfo zMyInfo;
  private ActivePeerManager zActivePeerManager = null;
  private BufferedReader zReader;

  public UserDialogConsole(String pTitle, MyInfo pMyInfo) {
    IllegalArgument.ifNull("Title", pTitle);
    IllegalArgument.ifNull("MyInfo", zMyInfo = pMyInfo);
    System.out.println(pTitle);
    zReader = new BufferedReader(new InputStreamReader(System.in));
    start();
  }

  public void setActivePeerManager(ActivePeerManager pActivePeerManager) {
    if (pActivePeerManager != null)
      zActivePeerManager = pActivePeerManager;
  }

  public void setPendingPeerManager(PendingPeerManager pPendingPeerManager) {
    if (pPendingPeerManager != null) {
    }
  }

  public void run() {
    System.out.println("Listening At: " + zMyInfo.getAddresses() + ":" + zMyInfo.getPort());

    try {
      while (true)
        processCommand(zReader.readLine());
    }
    catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

  private void processCommand(String pLine) {
    pLine = pLine.trim();

    if (pLine.length() == 0)
      return;

    if (pLine.equalsIgnoreCase("WHO")) {
      showWho();
      return;
    }
    //                                    01234
    if (pLine.toUpperCase().startsWith("I AM ") && pLine.length() > 5) {
      handleNAMEchange(pLine.substring(5).trim());
      send("Name Change from (" + zMyInfo.getPrevChatName() + ") to: " + zMyInfo.getChatName());
      return;
    }
    //                                    0123
    if (pLine.toUpperCase().startsWith("MSG ") && pLine.length() > 4) {
      String lineRest = pLine.substring(4).trim();
      int spaceAt = lineRest.indexOf(' ');
      if (spaceAt != -1) {
        handlePrivateMessage(lineRest.substring(0, spaceAt), lineRest.substring(spaceAt + 1).trim());
        return;
      }
    }
    // Chat message
    handleCHAT(pLine);
  }

  private void send(String pMessage) {
    System.out.println(pMessage);
  }

  private void showWho() {
    send("Currently Connected:");
    send("  " + zMyInfo.toString());
    PeerInfo[] peers = getPeerInfos();
    for (int i = 0; i < peers.length; i++)
      send("  " + peers[i].toString());
  }

  public void showUnrecognized(PeerInfo pPeerInfo, String pBadMessage) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    IllegalArgument.ifNull("BadMessage", pBadMessage);
    send("Unrecognized Command from (" + pPeerInfo.getID() + " " + pPeerInfo.getChatName() + "): " + pBadMessage);
  }

  public void showStreamsFailed(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    send("Unable to Set up I/O Streams with: " + pPeerInfo.toString());
  }

  public void showConnectFailed(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    send("Unable to Connect to: " + pPeerInfo.toString());
  }

  public void showConnect(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    send("Connection From: " + pPeerInfo.toString());
  }

  public void showDisconnect(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    send("Disconnect: " + pPeerInfo.getID() + " " + pPeerInfo.getChatName());
  }

  public void showCHAT(PeerInfo pPeerInfo, String pMessage) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    IllegalArgument.ifNull("Message", pMessage);
    send(pPeerInfo.getID() + " " + pPeerInfo.getChatName() + ": " + pMessage);
  }

  public void showPMSG(PeerInfo pPeerInfo, String pMessage) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    IllegalArgument.ifNull("Message", pMessage);
    send("Private Message From (" + pPeerInfo.getID() + " " + pPeerInfo.getChatName() + "): " + pMessage);
  }

  public void showNAME(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    send("Name Change: " + pPeerInfo.toString());
  }

  public void showHELO(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    send("HELO From: " + pPeerInfo.toString());
  }

  private void handleCHAT(String pLine) {
    IllegalArgument.ifNull("Line", pLine);
    if (zActivePeerManager == null)     // builder pattern
      send("No Peer Manager!");
    else {
      zActivePeerManager.sendToAllCHAT(pLine);
      send(zMyInfo.getChatName() + ": " + pLine);
    }
  }

  private void handleNAMEchange(String pNewName) {
    IllegalArgument.ifNull("NewName", pNewName);
    if (zActivePeerManager == null)     // builder pattern
      send("No Peer Manager!");
    else {
      zMyInfo.setChatName(pNewName);
      zActivePeerManager.sendToAllNAME();
    }
  }

  private PeerInfo[] getPeerInfos() {
    return (zActivePeerManager != null) ? zActivePeerManager.getPeerInfos() : new PeerInfo[0]; // builder pattern
  }

  private void handlePrivateMessage(String id, String msg) {
    if (zActivePeerManager == null)     // builder pattern
      send("No Peer Manager!");
    else {
      ActivePeer other = zActivePeerManager.getPeerListenerByID(id);
      if (other == null)
        send("Unrecognized Peer ID: " + id);
      else {
        other.sendPMSG(msg);
        send("Private Message sent to: " + other.getPeerInfo().toString());
      }
    }
  }
}
