package VASSAL.chat.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;
import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JToolBar;
import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.HttpMessageServer;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.messageboard.MessageBoardControls;
import VASSAL.chat.peer2peer.PeerPoolInfo;

/**
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Jul 16, 2003
 */
public class ShowServerStatusAction extends AbstractAction {
  private static Window frame;

  public ShowServerStatusAction(ServerStatus status, URL iconURL) {
    if (frame == null) {
      frame = new Window(status);
    }
    if (iconURL == null) {
      putValue(NAME, "Server Status");
    }
    else {
      putValue(SMALL_ICON, new ImageIcon(iconURL));
    }
    putValue(SHORT_DESCRIPTION, "Display server connections for all modules");
  }
  
  public void actionPerformed(ActionEvent e) {
    frame.refresh();
  }

  private static class Window extends JFrame implements PropertyChangeListener {
    private ServerStatusView view;
    private MessageBoardControls messageMgr;

    public Window(ServerStatus status) {
      super("Server Status");
      view = new ServerStatusView(status);
      view.addPropertyChangeListener(ServerStatusView.SELECTION_PROPERTY,this);
      getContentPane().add(view);
      messageMgr = new MessageBoardControls();
      JToolBar toolbar = new JToolBar();
      toolbar.setFloatable(false);
      toolbar.add(messageMgr.getCheckMessagesAction());
      toolbar.add(messageMgr.getPostMessageAction());
      getContentPane().add(toolbar, BorderLayout.NORTH);
      pack();
      setSize(Math.max(getSize().width,400),Math.max(getSize().height,300));
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      setLocation(d.width / 2 - getSize().width / 2, d.height / 2 - getSize().height / 2);
    }

    public void refresh() {
      if (!isVisible()) {
        setVisible(true);
      }
      else {
        toFront();
      }
      view.refresh();
    }

    public void propertyChange(PropertyChangeEvent evt) {
      MessageBoard server = null;
      String name = null;
      if (evt.getNewValue() instanceof ServerStatus.ModuleSummary) {
        final String moduleName = ((ServerStatus.ModuleSummary) evt.getNewValue()).getModuleName();
        server = new HttpMessageServer(new PeerPoolInfo() {
          public String getModuleName() {
            return moduleName;
          }

          public String getUserName() {
            return ((ChatServerConnection) GameModule.getGameModule().getServer()).getUserInfo().getName();
          }
        });
      }
      messageMgr.setServer(server, name);
    }
  }
}
