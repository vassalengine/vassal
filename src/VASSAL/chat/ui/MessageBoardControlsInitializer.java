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
