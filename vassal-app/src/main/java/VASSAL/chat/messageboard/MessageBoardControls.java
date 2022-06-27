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
package VASSAL.chat.messageboard;

import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.menu.MenuManager;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingWorker;
import javax.swing.WindowConstants;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.ExecutionException;

/**
 * UI controls for posting and retrieving messages from a
 * message board.
 */
public class MessageBoardControls {
  private MessageBoard server;
  private String serverName;
  private Action checkMessagesAction;
  private Action postMessageAction;
  private MessageViewer viewer;
  private JFrame msgFrame;
  private JFrame msgComposer;

  public MessageBoardControls() {
    initComponents();
    setServer(null, null);
  }

  public void setServer(MessageBoard msgSvr, String name) {
    server = msgSvr;
    serverName = name;
    checkMessagesAction.setEnabled(server != null);
    postMessageAction.setEnabled(server != null);
  }

  public void showMessages(Message[] m) {
    viewer.setMessages(
      Collections.enumeration(
        Collections.unmodifiableList((Arrays.asList(m)))));
  }

  private void initComponents() {
    viewer = new MessageViewer();

    msgFrame = new JFrame(Resources.getString("Chat.message_board"));  //$NON-NLS-1$
    msgFrame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
    msgFrame.setLayout(
      new BoxLayout(msgFrame.getContentPane(), BoxLayout.Y_AXIS));
    msgFrame.setJMenuBar(MenuManager.getInstance().getMenuBarFor(msgFrame));
    msgFrame.add(viewer);

    final Box box = Box.createHorizontalBox();
    final JButton refresh = new JButton(
      Resources.getString("Chat.refresh"));  //$NON-NLS-1$
    refresh.addActionListener(evt -> {
      refresh.setEnabled(false);

      new SwingWorker<Message[], Void>() {
        @Override
        protected Message[] doInBackground() {
          return server.getMessages();
        }

        @Override
        protected void done() {
          try {
            showMessages(get());
          }
          catch (final InterruptedException | ExecutionException e) {
            ErrorDialog.bug(e);
          }

          msgFrame.setTitle(serverName != null ?
            Resources.getString("Chat.message_board_title", serverName) :
            Resources.getString("Chat.message_board"));   //$NON-NLS-1$

          refresh.setEnabled(true);
        }
      }.execute();
    });
    box.add(refresh);

    final JButton b = new JButton(Resources.getString(Resources.CLOSE));
    b.addActionListener(e -> msgFrame.setVisible(false));
    box.add(b);
    msgFrame.add(box);

    msgComposer = new Comp();

    checkMessagesAction = new AbstractAction(
                  Resources.getString("Chat.check_messages")) {  //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent evt) {
        setEnabled(false);

        new SwingWorker<Message[], Void>() {
          @Override
          protected Message[] doInBackground() {
            return server.getMessages();
          }

          @Override
          protected void done() {
            try {
              showMessages(get());
            }
            catch (final InterruptedException | ExecutionException e) {
              ErrorDialog.bug(e);
            }

            msgFrame.setTitle(serverName != null ?
              Resources.getString("Chat.message_board_title", serverName) :
              Resources.getString("Chat.message_board"));   //$NON-NLS-1$

            msgFrame.setVisible(true);
            setEnabled(true);
          }
        }.execute();
      }
    };

    URL imageURL = getClass().getResource("/images/getMessages.gif");  //$NON-NLS-1$
    if (imageURL != null) {
      checkMessagesAction.putValue(Action.SHORT_DESCRIPTION, checkMessagesAction.getValue(Action.NAME));
      checkMessagesAction.putValue(Action.NAME, "");  //$NON-NLS-1$
      checkMessagesAction.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
    }

    postMessageAction = new AbstractAction(
                      Resources.getString("Chat.post_message")) {  //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent evt) {
        msgComposer.setVisible(true);
      }
    };

    imageURL = getClass().getResource("/images/postMessage.gif");  //$NON-NLS-1$
    if (imageURL != null) {
      postMessageAction.putValue(Action.SHORT_DESCRIPTION, postMessageAction.getValue(Action.NAME));
      postMessageAction.putValue(Action.NAME, "");  //$NON-NLS-1$
      postMessageAction.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
    }

    msgFrame.pack();
    msgFrame.setLocation(java.awt.Toolkit.getDefaultToolkit().getScreenSize().width / 2 - msgFrame.getSize().width / 2, 0);
  }

  public Action getCheckMessagesAction() {
    return checkMessagesAction;
  }

  public Action getPostMessageAction() {
    return postMessageAction;
  }

  private class Comp extends JFrame {
    private static final long serialVersionUID = 1L;

    private Comp() {
      super(Resources.getString("Chat.message_composer"));  //$NON-NLS-1$
      setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
      setJMenuBar(MenuManager.getInstance().getMenuBarFor(this));

      final JTextArea msgArea = new JTextArea(15, 60);
      final Box b = Box.createHorizontalBox();

      final JButton okButton =
        new JButton(Resources.getString("Chat.send"));  //$NON-NLS-1$
      okButton.addActionListener(evt -> {
        okButton.setEnabled(false);
        msgArea.setEnabled(false);

        new SwingWorker<Void, Void>() {
          @Override
          protected Void doInBackground() {
            server.postMessage(msgArea.getText());
            return null;
          }

          @Override
          protected void done() {
            setVisible(false);
            okButton.setEnabled(true);
            msgArea.setText("");  //$NON-NLS-1$
            msgArea.setEnabled(true);
          }
        }.execute();
      });

// FIXME: Cancel does not cancel sending a message!
      final JButton cancelButton = new JButton(Resources.getString(Resources.CANCEL));
      cancelButton.addActionListener(evt -> {
        setVisible(false);
        msgArea.setText("");  //$NON-NLS-1$
      });

      b.add(okButton);
      b.add(cancelButton);
      add(new JScrollPane(msgArea));
      add(b);

      // Default actions for Enter/ESC
      getRootPane().setDefaultButton(okButton);
      getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "Cancel"); //$NON-NLS-1$
      getRootPane().getActionMap().put("Cancel", new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
          cancelButton.doClick();
        }
      });

      pack();
      setLocation(java.awt.Toolkit.getDefaultToolkit().getScreenSize().width / 2 - getSize().width / 2, 0);
    }
  }
}
