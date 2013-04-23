package org.litesoft.p2pchat;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;


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
 * @version 0.4 04/06/13 Add finish() to cleanly stop the PPM thread
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */

public class PendingPeerManager extends Thread implements NewPeersSupport {
  private UserDialog zUserDialog;
  private PendingPeerLinkedList zPendingPeers = new PendingPeerLinkedList();
  private PendingPeersSupport zPendingPeersSupport = null;
  private int maxRetries = 3;
  private boolean finish = false;

  public PendingPeerManager(UserDialog pUserDialog) {
    IllegalArgument.ifNull("UserDialog", zUserDialog = pUserDialog);
    zUserDialog.setPendingPeerManager(this);
  }

  public void start(PendingPeersSupport pPendingPeersSupport) {
    IllegalArgument.ifNull("PendingPeersSupport", zPendingPeersSupport = pPendingPeersSupport);
    start();
  }

  public void addNewPeer(PeerInfo pInfo) {
    IllegalArgument.ifNull("Info", pInfo);
    zPendingPeers.add(null, pInfo);
  }

  public void addNewPeer(Socket pSocket) {
    IllegalArgument.ifNull("Socket", pSocket);
    InetAddress theirAddress = pSocket.getInetAddress();
    String theirName = theirAddress.getHostName();
    String theirIP = theirAddress.getHostAddress();
    String name = theirIP.equals(theirName) ? null : "(Host: " + theirName + ")";
    zPendingPeers.add(pSocket, new PeerInfo(name, theirIP));
  }

  public void addNewPeer(PeerInfo info, PeerReader reader, PeerWriter writer) {
    zPendingPeersSupport.addActivePeer(info, reader, writer);
  }

  public void run() {
    while (true) {
      final PendingPeerNode next = zPendingPeers.next();
      if (finish) {
        return;
      }
      Runnable runnable = new Runnable() {
        public void run() {
          handleNewPeerClient(next);
        }
      };
      new Thread(runnable).start();
    }
  }
  
  public void finish() {
    finish = true;
    interrupt();
  }
  
  private void handleNewPeerClient(PendingPeerNode pPendingPeerNode) {
    PeerInfo peerInfo = pPendingPeerNode.getPeerInfo();
    if (zPendingPeersSupport.isAlreadyConnected(peerInfo)) {
      return;
    }
    if ("true".equals(System.getProperty("debug"))) {
      System.err.println("Attempting to contact " + peerInfo.format());
    }

    Socket socket = pPendingPeerNode.getSocket();
    if (socket == null) {
      try {
        socket = getPeerClientSocketFromAddresses(peerInfo);
      }
      catch (IOException e) {
        if ("true".equals(System.getProperty("debug"))) {
          System.err.println("Failed to establish connection to " + peerInfo.format() + ":  " + e.getMessage());
        }
        // If we're under the maximum number of retries, send this peer to the end of the queue
        if (peerInfo.incrementFailureCount() <= maxRetries) {
          zPendingPeers.add(socket, peerInfo);
        }
        else {
          if ("true".equals(System.getProperty("debug"))) {
            System.err.println("Maximum retries reached for " + peerInfo.format());
          }
          peerInfo.setFailureReason(e);
          zUserDialog.showConnectFailed(peerInfo);
        }
        return;
      }
    }

    InputStream inputStream = null;
    OutputStream outputStream = null;
    try {
      inputStream = socket.getInputStream();
      outputStream = socket.getOutputStream();
    }
    catch (IOException e) {
      zUserDialog.showStreamsFailed(peerInfo);
      try {
        socket.close();
      }
      catch (IOException ignore) {
      }
      return;
    }
    zPendingPeersSupport.addActivePeer(peerInfo, inputStream, outputStream);
    zUserDialog.showConnect(peerInfo);
  }

  private Socket getPeerClientSocketFromAddresses(PeerInfo pPeerInfo) throws IOException {
    String pPeerAddresses = pPeerInfo.getAddresses();
    int peerPort = pPeerInfo.getPort();
    for (int i; -1 != (i = pPeerAddresses.indexOf(',')); pPeerAddresses = pPeerAddresses.substring(i + 1)) {
      try {
        return getPeerClientSocket(pPeerAddresses.substring(0, i), peerPort);
      }
      catch (IOException e) {
        if ("true".equals(System.getProperty("debug"))) {
          System.err.println("Failed to establish connection to " + pPeerAddresses.substring(0, i)
                             + ":" + peerPort + ".  " + e.getMessage());
        }
      }
    }
    return getPeerClientSocket(pPeerAddresses, peerPort);
  }

  private Socket getPeerClientSocket(String pPeerAddress, int pPeerPort) throws IOException {
    Socket retval = new Socket(pPeerAddress, pPeerPort);
    if ("true".equals(System.getProperty("debug"))) {
      System.err.println("Established connection to " + pPeerAddress + ":" + pPeerPort);
    }
    return retval;
  }

  public void setMaxRetries(int maxRetries) {
    this.maxRetries = maxRetries;
  }

  public void setSocketConnectTimeout(long socketConnectTimeout) {
  }
}
