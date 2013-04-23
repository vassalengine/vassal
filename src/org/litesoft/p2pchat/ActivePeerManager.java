package org.litesoft.p2pchat;

import java.io.*;
import java.util.*;


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
 * @version 0.4 08/04/13 Close Peer on removal
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */
public class ActivePeerManager
  implements ActivePeersSupport, PendingPeersSupport {
  private MyInfo zMyInfo;
  private UserDialog zUserDialog;
  private PendingPeerManager zPendingPeerManager;
  private Vector<ActivePeer> zListofPeers = new Vector<ActivePeer>();
  private int zPeerCnt = 0;

  public ActivePeerManager(MyInfo pMyInfo, UserDialog pUserDialog, PendingPeerManager pPendingPeerManager) {
    IllegalArgument.ifNull("MyInfo", zMyInfo = pMyInfo);
    IllegalArgument.ifNull("UserDialog", zUserDialog = pUserDialog);
    IllegalArgument.ifNull("PendingPeerManager", zPendingPeerManager = pPendingPeerManager);
    zUserDialog.setActivePeerManager(this);
    zPendingPeerManager.start(this);
  }

  public synchronized void removeActivePeer(ActivePeer pRequester) {
    IllegalArgument.ifNull("Requester", pRequester);
    zListofPeers.removeElement(pRequester);
    pRequester.close();
  }

  public synchronized void clear() {
    while (!zListofPeers.isEmpty()) {
      ActivePeer p = (ActivePeer) zListofPeers.lastElement();
      zListofPeers.removeElement(p);
      p.close();
    }
  }

  public synchronized PeerInfo[] getPeerInfos() {
    PeerInfo[] retval = new PeerInfo[zListofPeers.size()];
    for (int i = 0; i < retval.length; i++)
      retval[i] = ((ActivePeer) (zListofPeers.elementAt(i))).getPeerInfo();
    return retval;
  }

  public synchronized void sendToAllCHAT(String pMessage) {
    IllegalArgument.ifNull("Message", pMessage);
    for (Enumeration<ActivePeer> it = zListofPeers.elements(); it.hasMoreElements();)
      ((ActivePeer) it.nextElement()).sendCHAT(pMessage);
  }

  public synchronized void sendToAllNAME() {
    for (Enumeration<ActivePeer> it = zListofPeers.elements(); it.hasMoreElements();)
      ((ActivePeer) it.nextElement()).sendNAME();
  }

  public synchronized ActivePeer getPeerListenerByID(String pID) {
    if (pID != null)
      for (Enumeration<ActivePeer> it = zListofPeers.elements(); it.hasMoreElements();) {
        ActivePeer peer = (ActivePeer) it.nextElement();
        if (pID.equals(peer.getPeerInfo().getID()))
          return peer;
      }
    return null;
  }

  public synchronized ActivePeer getPeerListenerByInfo(PeerInfo info) {
    if (info != null) {
      for (Enumeration<ActivePeer> it = zListofPeers.elements(); it.hasMoreElements();) {
        ActivePeer peer = (ActivePeer) it.nextElement();
        if (info.equals(peer.getPeerInfo())) {
          return peer;
        }
      }
    }
    return null;
  }

  public synchronized boolean isAlreadyConnected(PeerInfo pPeerInfo) {
    if (pPeerInfo != null)
      for (Enumeration<ActivePeer> it = zListofPeers.elements(); it.hasMoreElements();)
        if (((ActivePeer) it.nextElement()).getPeerInfo().equals(pPeerInfo))
          return true;

    return false;
  }
  
  public synchronized void addActivePeer(PeerInfo pPeerInfo, InputStream pInputStream, OutputStream pOutputStream) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    IllegalArgument.ifNull("InputStream", pInputStream);
    IllegalArgument.ifNull("OutputStream", pOutputStream);
    pPeerInfo.setID(String.valueOf(++zPeerCnt));
    zListofPeers.addElement(
      new ActivePeer(zMyInfo, zUserDialog, this, zPendingPeerManager,
                     pPeerInfo, pInputStream, pOutputStream));
  }

  public synchronized void addActivePeer(PeerInfo pPeerInfo, PeerReader reader, PeerWriter writer) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    IllegalArgument.ifNull("PeerReader", reader);
    IllegalArgument.ifNull("PeerWriter", writer);
    pPeerInfo.setID(String.valueOf(++zPeerCnt));
    zListofPeers.addElement(
      new ActivePeer(zMyInfo, zUserDialog, this, zPendingPeerManager,
                     pPeerInfo, reader, writer));
  }
}
