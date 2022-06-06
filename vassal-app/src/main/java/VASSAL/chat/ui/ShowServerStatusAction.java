/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

import java.awt.BorderLayout;
import java.awt.Dimension;
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
import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;

/**
 * Description?
 */
public class ShowServerStatusAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private static Window frame;

  public ShowServerStatusAction(ServerStatus status, URL iconURL) {
    this(status, iconURL, true);
  }

  public ShowServerStatusAction(ServerStatus status, URL iconURL, boolean includeMessageControls) {
    if (frame == null) {
      frame = new Window(status, includeMessageControls);
    }
    if (iconURL == null) {
      putValue(NAME, Resources.getString("Chat.server_status")); //$NON-NLS-1$
    }
    else {
      putValue(SMALL_ICON, new ImageIcon(iconURL));
    }
    putValue(SHORT_DESCRIPTION, Resources.getString("Chat.display_connections")); //$NON-NLS-1$
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    frame.refresh();
  }

  private static class Window extends JFrame implements PropertyChangeListener {
    private static final long serialVersionUID = 1L;

    private final ServerStatusView view;
    private final MessageBoardControls messageMgr;

    public Window(ServerStatus status, boolean includeMessageControls) {
      super(Resources.getString("Chat.server_status")); //$NON-NLS-1$
      setJMenuBar(MenuManager.getInstance().getMenuBarFor(this));

      view = new ServerStatusView(status);
      view.addPropertyChangeListener(ServerStatusView.SELECTION_PROPERTY, this);
      add(view);
      if (includeMessageControls) {
        messageMgr = new MessageBoardControls();
        final JToolBar toolbar = new JToolBar();
        toolbar.setFloatable(false);
        toolbar.add(messageMgr.getCheckMessagesAction());
        toolbar.add(messageMgr.getPostMessageAction());
        add(toolbar, BorderLayout.NORTH);
      }
      else {
        messageMgr = null;
      }

      pack();
      setSize(Math.max(getSize().width, 400), Math.max(getSize().height, 300));
      final Dimension d = SwingUtils.getScreenSize();
      setLocation(d.width / 2 - getSize().width / 2,
                  d.height / 2 - getSize().height / 2);
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

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
      MessageBoard server = null;
      if (evt.getNewValue() instanceof ServerStatus.ModuleSummary) {
        final String moduleName = ((ServerStatus.ModuleSummary) evt.getNewValue()).getModuleName();
        server = new HttpMessageServer(new PeerPoolInfo() {
          @Override
          public String getModuleName() {
            return moduleName;
          }

          @Override
          public String getUserName() {
            return ((ChatServerConnection) GameModule.getGameModule().getServer()).getUserInfo().getName();
          }
        });
      }
      if (messageMgr != null) {
        messageMgr.setServer(server, null);
      }
    }
  }
}
