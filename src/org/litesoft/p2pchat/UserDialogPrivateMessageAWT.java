package org.litesoft.p2pchat;

import java.awt.*;
import java.awt.event.*;

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
public class UserDialogPrivateMessageAWT extends Frame {
  private static final long serialVersionUID = 1L;
  private UserDialogAWT zUserDialog;
  private MyInfo zMyInfo;
  private ActivePeer zPeerListener;

  private TextField zChatText;
  private TextArea zMessagesTextArea;

  public UserDialogPrivateMessageAWT(UserDialogAWT pUserDialog, MyInfo pMyInfo, ActivePeer pPeerListener) {
    super("P2P Chat Private Messaging with " + pPeerListener.getPeerInfo().toString());

    IllegalArgument.ifNull("UserDialog", zUserDialog = pUserDialog);
    IllegalArgument.ifNull("MyInfo", zMyInfo = pMyInfo);
    IllegalArgument.ifNull("PeerListener", zPeerListener = pPeerListener);

    setLayout(new BorderLayout());

    add("Center", layoutReceivedMessagesPanel());
    add("South", layoutChatEntryPanel());
    pack();
    addWindowListener(
      new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          zUserDialog.unregisterPrivateMessager(zPeerListener.getPeerInfo());
          dispose();
        }
      }
    );
    setVisible(true);
  }

  private Panel layoutChatEntryPanel() {
    Panel panel = new Panel();
    panel.setLayout(new BorderLayout());
    panel.add("West", new Label("Message to Send:"));
    panel.add("Center", zChatText = new TextField());
    zChatText.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          String pLine = e.getActionCommand();
          zChatText.setText("");
          zPeerListener.sendPMSG(pLine);
          send(zMyInfo.getChatName() + ": " + pLine);
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

  public void send(String pMessage) {
    IllegalArgument.ifNull("Message", pMessage);
    String current = zMessagesTextArea.getText();
    current += pMessage + "\n";
    zMessagesTextArea.setText(current);
  }
}
