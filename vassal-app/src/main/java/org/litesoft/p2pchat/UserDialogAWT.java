package org.litesoft.p2pchat;

import java.awt.*;
import java.awt.List;
import java.awt.event.*;
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
 * @version 0.3 02/02/02 Added IllegalArgument.ifNull for all public params that may not be null
 * @version 0.2 01/28/02 Refactored and Added Licence
 * @version 0.1 12/27/01 Initial Version
 */
@SuppressWarnings("unchecked")
public class UserDialogAWT extends Frame implements UserDialog {
  private static final long serialVersionUID = 1L;
  private MyInfo zMyInfo;
  private ActivePeerManager zActivePeerManager = null;
  private TextField zChatText;
  private TextField zNameText;
  private TextArea zMessagesTextArea;
  private List zPeersList;
  private Hashtable zPrivateMessagersMap = new Hashtable();

  public UserDialogAWT(String pTitle, MyInfo pMyInfo) {
    super(pTitle);
    IllegalArgument.ifNull("Title", pTitle);
    IllegalArgument.ifNull("MyInfo", zMyInfo = pMyInfo);

    setLayout(new BorderLayout());

    add("North", layoutNamePanel());
    add("Center", layoutReceivedMessagesPanel());
    add("East", layoutWhoPanel());
    add("South", layoutChatEntryPanel());
    pack();
    addWindowListener(
      new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          System.exit(0);
        }
      }
    );
    setVisible(true);
    showWho();
    zChatText.requestFocus();
  }

  public void setActivePeerManager(ActivePeerManager pActivePeerManager) {
    if (pActivePeerManager != null)
      zActivePeerManager = pActivePeerManager;
  }

  public void setPendingPeerManager(PendingPeerManager pPendingPeerManager) {
    if (pPendingPeerManager != null) {
    }
  }

  private Panel layoutChatEntryPanel() {
    Panel panel = new Panel();
    panel.setLayout(new BorderLayout());
    panel.add("West", new Label("Message to Send:"));
    panel.add("Center", zChatText = new TextField());
    zChatText.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          handleCHAT(e.getActionCommand());
          zChatText.setText("");
        }
      }
    );
    return panel;
  }

  private Panel layoutReceivedMessagesPanel() {
    Panel panel = new Panel();
    panel.setLayout(new BorderLayout());
    panel.add("North", new Label("Received Messages:"));
    panel.add("Center", zMessagesTextArea = new TextArea());
    zMessagesTextArea.setEnabled(true);
    zMessagesTextArea.setEditable(false);
    return panel;
  }

  private Panel layoutWhoPanel() {
    Panel panel = new Panel();
    panel.setLayout(new BorderLayout());
    panel.add("North", new Label("Who's On:"));
    panel.add("Center", zPeersList = new List(25, false));
    zPeersList.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          String lineRest = e.getActionCommand();
          int spaceAt = lineRest.indexOf(' ');
          if (spaceAt != -1)
            handlePrivateMessageWindowRequest(lineRest.substring(0, spaceAt));
          zChatText.requestFocus();
        }
      }
    );
    return panel;
  }

  private Panel layoutNamePanel() {
    Panel panel = new Panel();
    panel.setLayout(new FlowLayout(FlowLayout.LEFT));
    panel.add(new Label("Name:", Label.RIGHT));
    panel.add(zNameText = new TextField("", 15));
    zNameText.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          handleNAMEchange(e.getActionCommand().trim());
          zNameText.setText("");
          showWho();
          zChatText.requestFocus();
        }
      }
    );
    return panel;
  }

  private void send(String pMessage) {
    String current = zMessagesTextArea.getText();
    current += pMessage + "\n";
    zMessagesTextArea.setText(current);
  }

  private void showWho() {
    zPeersList.removeAll();
    zPeersList.add(zMyInfo.toString(), 0);
    PeerInfo[] peers = getPeerInfos();
    for (int i = 0; i < peers.length; i++)
      zPeersList.add(peers[i].toString(), i + 1);
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
    showWho();
  }

  public void showDisconnect(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    UserDialogPrivMsgAWT subWindow = getPrivateMessageWindow(pPeerInfo);
    if (subWindow != null) {
      unregisterPrivateMessager(pPeerInfo);
      subWindow.dispose();
    }
    showWho();
  }

  public void showCHAT(PeerInfo pPeerInfo, String pMessage) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    IllegalArgument.ifNull("Message", pMessage);
    send(pPeerInfo.getID() + " " + pPeerInfo.getChatName() + ": " + pMessage);
  }

  public void showPMSG(PeerInfo pPeerInfo, String pMessage) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    IllegalArgument.ifNull("Message", pMessage);
    UserDialogPrivMsgAWT subWindow = getPrivateMessageWindow(pPeerInfo);
    if (subWindow != null)
      subWindow.send(pPeerInfo.getChatName() + ": " + pMessage);
    else
      send("Private Message From (" + pPeerInfo.getID() + " " + pPeerInfo.getChatName() + "): " + pMessage);
  }

  public void showNAME(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    showWho();
  }

  public void showHELO(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    showWho();
  }

  private UserDialogPrivMsgAWT getPrivateMessageWindow(PeerInfo pPeerInfo) {
    return (UserDialogPrivMsgAWT) zPrivateMessagersMap.get(pPeerInfo);
  }

  public void unregisterPrivateMessager(PeerInfo pPeerInfo) {
    IllegalArgument.ifNull("PeerInfo", pPeerInfo);
    zPrivateMessagersMap.remove(pPeerInfo);
  }

  private void openPrivateMessageWindow(ActivePeer other) {
    zPrivateMessagersMap.put(other.getPeerInfo(), new UserDialogPrivMsgAWT(this, zMyInfo, other));
  }

  private void handleCHAT(String pLine) {
    if (zActivePeerManager == null)     // builder pattern
      send("No Peer Manager!");
    else {
      zActivePeerManager.sendToAllCHAT(pLine);
      send(zMyInfo.getChatName() + ": " + pLine);
    }
  }

  private void handleNAMEchange(String newName) {
    if (zActivePeerManager == null)     // builder pattern
      send("No Peer Manager!");
    else {
      zMyInfo.setChatName(newName);
      zActivePeerManager.sendToAllNAME();
    }
  }

  private PeerInfo[] getPeerInfos() {
    return (zActivePeerManager != null) ? zActivePeerManager.getPeerInfos() : new PeerInfo[0]; // builder pattern
  }

  private void handlePrivateMessageWindowRequest(String id) {
    if (zActivePeerManager == null)     // builder pattern
      send("No Peer Manager!");
    else {
      ActivePeer other = zActivePeerManager.getPeerListenerByID(id);
      if (other == null)
        send("Unrecognized Peer ID: " + id);
      else
        openPrivateMessageWindow(other);
    }
  }
}
