package VASSAL.chat.ui;

import javax.swing.JButton;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.messageboard.MessageBoardControls;

/** Adds controls to post/retrieve message from a message board */
public class MessageBoardControlsInitializer implements ChatControlsInitializer {
  
  private MessageBoardControls msgMgr;
  private JButton checkMsgButton;
  private JButton postMsgButton;

  public MessageBoardControlsInitializer(String name, MessageBoard board) {
    super();
    msgMgr = new MessageBoardControls();
    msgMgr.setServer(board, name);
  }

  public void initializeControls(ChatServerControls controls) {
    checkMsgButton = controls.getToolbar().add(msgMgr.getCheckMessagesAction());
    postMsgButton = controls.getToolbar().add(msgMgr.getPostMessageAction());
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(checkMsgButton);
    controls.getToolbar().remove(postMsgButton);
    controls.getToolbar().repaint();
  }
}
