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
 * @version 0.4 04/07/13 Added name for thread
 *                       Close PeerReader & PeerWriter on close
 *                       Add DONE message type
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */
public class ActivePeer extends Thread {
  public static final String CHAT = "CHAT ";
  public static final String HELO = "HELO ";
  public static final String PMSG = "PMSG ";
  public static final String NAME = "NAME ";
  public static final String PEER = "PEER ";
  public static final String DONE = "DONE ";

  private MyInfo zMyInfo;
  private UserDialog zUserDialog;
  private ActivePeersSupport zActivePeersSupport;
  private NewPeersSupport zNewPeersSupport;
  private PeerInfo zPeerInfo;
  private PeerReader zReader;
  private PeerWriter zWriter;

  public ActivePeer(MyInfo pMyInfo, UserDialog pUserDialog,
                    ActivePeersSupport pActivePeersSupport,
                    NewPeersSupport pNewPeersSupport,
                    PeerInfo pPeerInfo, InputStream pIs, OutputStream pOs) {
    this(pMyInfo, pUserDialog, pActivePeersSupport, pNewPeersSupport, pPeerInfo, new PeerReader(pIs), new PeerWriter(pOs));
  }

  public ActivePeer(MyInfo pMyInfo, UserDialog pUserDialog,
                    ActivePeersSupport pActivePeersSupport,
                    NewPeersSupport pNewPeersSupport,
                    PeerInfo pPeerInfo, PeerReader reader, PeerWriter writer) {
    IllegalArgument.ifNull("MyInfo", zMyInfo = pMyInfo);
    IllegalArgument.ifNull("UserDialog", zUserDialog = pUserDialog);
    IllegalArgument.ifNull("ActivePeersSupport", zActivePeersSupport = pActivePeersSupport);
    IllegalArgument.ifNull("NewPeersSupport", zNewPeersSupport = pNewPeersSupport);
    IllegalArgument.ifNull("PeerInfo", zPeerInfo = pPeerInfo);
    zReader = reader;
    zWriter = writer;
    setName("Active Peer Thread");
    start();
  }

  public PeerInfo getPeerInfo() {
    return zPeerInfo;
  }

  public void sendNAME() {
    send(formatNAME());
  }

  public void sendPMSG(String pMessage) {
    IllegalArgument.ifNull("Message", pMessage);
    send(formatPMSG(pMessage));
  }

  public void sendCHAT(String pMessage) {
    IllegalArgument.ifNull("Message", pMessage);
    send(formatCHAT(pMessage));
  }
  
  public void finish() {
    send(DONE);
  }
  
  public void run() {
    send(formatHELO());

    for (String line; null != (line = zReader.readLine());) {
      processCommand(line);
      if (line.equals(DONE)) {
        break;
      }
    }
    
    close();
    zActivePeersSupport.removeActivePeer(this);
    zUserDialog.showDisconnect(zPeerInfo);
  }

  public synchronized void close() {
    zReader.close();
    zWriter.close();
  }

  private void processCommand(String pLine) {
    if (pLine.trim().length() == 0)
      return;
    String cmdParams;

    try {
      if (null != (cmdParams = checkForCommand(pLine, CHAT))) {
        processCHAT(cmdParams);
        return;
      }
      if (null != (cmdParams = checkForCommand(pLine, HELO))) {
        processHELO(cmdParams);
        return;
      }
      if (null != (cmdParams = checkForCommand(pLine, PMSG))) {
        processPMSG(cmdParams);
        return;
      }
      if (null != (cmdParams = checkForCommand(pLine, NAME))) {
        processNAME(cmdParams);
        return;
      }
      if (null != (cmdParams = checkForCommand(pLine, PEER))) {
        processPEER(cmdParams);
        return;
      }
    }
    catch (Exception e) {
      System.err.println("Caught "+e.getClass().getName()+" processing command "+pLine);
      e.printStackTrace();
      return;
    }
    if ("BYE".equals(pLine)) {
      zWriter.close();
      return;
    }
    zUserDialog.showUnrecognized(zPeerInfo, pLine);
  }

  private String checkForCommand(String pLine, String pCmd) {
    return pLine.startsWith(pCmd) ? pLine.substring(pCmd.length()).trim() : null;
  }

  private void send(String pMessage) {
    zWriter.writeLine(pMessage);
  }

  private String formatCHAT(String pMessage) {
    return CHAT + pMessage;
  }

  private void processCHAT(String pCmdParams) {
    zUserDialog.showCHAT(zPeerInfo, pCmdParams);
  }

  private String formatPMSG(String pMessage) {
    return PMSG + pMessage;
  }

  private void processPMSG(String pCmdParams) {
    zUserDialog.showPMSG(zPeerInfo, pCmdParams);
  }

  private String formatNAME() {
    return "NAME " + zMyInfo.getChatName();
  }

  private void processNAME(String pCmdParams) {
    if (!pCmdParams.equals(zPeerInfo.getChatName())) {
      zPeerInfo.setChatName(pCmdParams);
      zUserDialog.showNAME(zPeerInfo);
    }
  }

  private String formatHELO() {
    return "HELO " + zMyInfo.format();
  }

  private void processHELO(String pCmdParams) {
    PeerInfo pi = PeerInfo.deFormat(pCmdParams);
    if (pi != null) {
      zPeerInfo.updateWith(pi);
      PeerInfo[] peers = zActivePeersSupport.getPeerInfos();
      for (int i = peers.length; i-- > 0;)
        if (!zPeerInfo.equals(peers[i]))
          send(formatPEER(peers[i]));
      zUserDialog.showHELO(zPeerInfo);
    }
  }

  private String formatPEER(PeerInfo pPeerInfo) {
    return PEER + pPeerInfo.format();
  }

  private void processPEER(String pCmdParams) {
    PeerInfo pi = PeerInfo.deFormat(pCmdParams);
    if (pi != null) {
      zNewPeersSupport.addNewPeer(pi);
    }
  }
}
