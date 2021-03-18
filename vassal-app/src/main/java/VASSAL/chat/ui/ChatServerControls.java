/*
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.tree.TreePath;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.ServerAddressBook;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.VisibilityOption;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SplitPane;
import VASSAL.tools.swing.SwingUtils;

import net.miginfocom.swing.MigLayout;

public class ChatServerControls extends AbstractBuildable {

  protected RoomTree currentRoom;
  protected JTextField newRoom;
  protected JLabel newRoomLabel;
  protected JToolBar toolbar;
  protected RoomTree roomTree;
  protected JButton newRoomButton;

  protected JButton launch;
  protected ChatServerConnection client;
  protected JPanel controlPanel;
  protected ChatControlsInitializer oldClient;
  protected BasicChatControlsInitializer basicControls;
  protected JButton configServerButton;
  protected String configServerText;

  @SuppressWarnings("removal")
  @Deprecated(since = "2020-11-15", forRemoval = true)
  protected VASSAL.tools.ComponentSplitter.SplitPane splitter;

  protected SplitPane splitPane;

  public ChatServerControls() {
    final JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    split.setResizeWeight(0.5);

    final JPanel roomPanel = new JPanel(new MigLayout("fill, nogrid, hidemode 3")); //NON-NLS
    roomPanel.setBorder(BorderFactory.createTitledBorder(
      BorderFactory.createRaisedBevelBorder(),
      Resources.getString("Chat.active_games"))
    );

    newRoom = new JTextField(12);
    newRoomLabel = new JLabel(Resources.getString("Chat.new_game"));
    newRoomLabel.setLabelFor(newRoom);
    roomPanel.add(newRoomLabel, "");
    roomPanel.add(newRoom, "growx, pushx"); //NON-NLS

    newRoomButton = new JButton("..."); //$NON-NLS-1$
    newRoomButton.setPreferredSize(new Dimension(20, 20));
    newRoomButton.setVisible(false);
    roomPanel.add(newRoomButton, "hidemode 3"); //NON-NLS

    roomTree = new RoomTree();
    final JScrollPane roomScroll = new JScrollPane(roomTree);
    roomPanel.add(roomScroll, "newline, spanx, grow, push"); //NON-NLS

    split.setLeftComponent(roomPanel);
    currentRoom = new RoomTree();
    currentRoom.addTreeWillExpandListener(new javax.swing.event.TreeWillExpandListener() {
      @Override
      public void treeWillCollapse(javax.swing.event.TreeExpansionEvent evt) throws javax.swing.tree.ExpandVetoException {
        throw new javax.swing.tree.ExpandVetoException(evt);
      }

      @Override
      public void treeWillExpand(javax.swing.event.TreeExpansionEvent evt) throws javax.swing.tree.ExpandVetoException {
      }
    });
    final JScrollPane scroll = new JScrollPane(currentRoom);
    scroll.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedBevelBorder(), Resources.getString("Chat.current_game")));  //$NON-NLS-1$
    split.setRightComponent(scroll);
    split.setDividerLocation(160);
    split.setPreferredSize(new Dimension(320, 120));
    controlPanel = new JPanel();
    controlPanel.setLayout(new BorderLayout());
    controlPanel.add("Center", split);  //$NON-NLS-1$
    toolbar = new JToolBar();
    controlPanel.add("North", toolbar);  //$NON-NLS-1$
    toolbar.addSeparator();

    configServerButton = new JButton();
    configServerButton.addActionListener(e -> ServerAddressBook.editCurrentServer(!client.isConnected()));
    configServerButton.addMouseListener(new MouseAdapter() {
      private void maybePopup(MouseEvent e) {
        if (!client.isConnected() && e.isPopupTrigger()) {
          showChangeServerMenu();
        }
      }

      @Override
      public void mousePressed(MouseEvent e) {
        maybePopup(e);
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        maybePopup(e);
      }
    });
    toolbar.add(configServerButton);
  }

  private void showChangeServerMenu() {
    ServerAddressBook.changeServerPopup(configServerButton);
  }

  public void updateClientDisplay(Icon icon, String text) {
    configServerButton.setIcon(icon);
    configServerText = text;
    updateConfigServerToolTipText();
  }

  private void updateConfigServerToolTipText() {
    if (client.isConnected()) {
      configServerButton.setToolTipText(configServerText);
    }
    else {
      configServerButton.setToolTipText("<html><center>" + configServerText + "<br>" + Resources.getString("Chat.change_server"));  //NON-NLS
    }
  }

  public Component getExtendedControls() {
    return null;
  }

  @Override
  public void addTo(Buildable b) {
    final GameModule gm = GameModule.getGameModule();
    setClient((ChatServerConnection) gm.getServer());
    launch = new JButton(Resources.getString("Chat.server"));  //$NON-NLS-1$
    launch.setFocusable(false);
    launch.setAlignmentY(0.0F);
    final ActionListener al = evt -> toggleVisible();
    launch.addActionListener(al);
    final NamedKeyStrokeListener l = new NamedKeyStrokeListener(al);
    l.setKeyStroke(NamedKeyStroke.of(KeyEvent.VK_S, InputEvent.ALT_DOWN_MASK));
    final URL iconURL = getClass().getResource("/images/connect.gif");  //$NON-NLS-1$
    if (iconURL != null) {
      launch.setIcon(new ImageIcon(iconURL));
      launch.setText(null);
    }

    final IconConfigurer iconConfig = new IconConfigurer("serverControlsIcon", Resources.getString("Chat.server_controls_button_icon"), "/images/connect.gif"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    iconConfig.setValue("/images/connect.gif");  //$NON-NLS-1$
    GlobalOptions.getInstance().addOption(iconConfig);
    iconConfig.addPropertyChangeListener(evt -> launch.setIcon(iconConfig.getIconValue()));
    iconConfig.fireUpdate();

    final NamedHotKeyConfigurer keyConfig = new NamedHotKeyConfigurer("serverControlsHotKey", Resources.getString("Chat.server_controls_hotkey"), l.getNamedKeyStroke());   //$NON-NLS-1$ //$NON-NLS-2$
    GlobalOptions.getInstance().addOption(keyConfig);
    keyConfig.addPropertyChangeListener(evt -> {
      l.setKeyStroke(keyConfig.getValueNamedKeyStroke());
      launch.setToolTipText(Resources.getString("Chat.server_controls_tooltip", NamedHotKeyConfigurer.getString(l.getKeyStroke())));  //$NON-NLS-1$
    });
    keyConfig.fireUpdate();

    gm.addKeyStrokeListener(l);
    gm.getToolBar().add(launch);
  }

  public void toggleVisible() {
    if (controlPanel.getTopLevelAncestor() == null) {
      if (GlobalOptions.getInstance().isUseSingleWindow()) {
        final Component gmcp = GameModule.getGameModule().getControlPanel();
        final Container gmcppar = gmcp.getParent();
        final int i = SwingUtils.getIndexInParent(gmcp, gmcppar);

        splitPane = new SplitPane(SplitPane.HORIZONTAL_SPLIT, gmcp, controlPanel);
        splitPane.setResizeWeight(1.0);
        gmcppar.add(splitPane, i);
      }
      else {
        final JFrame frame = new JFrame(Resources.getString("Chat.server"));  //$NON-NLS-1$
        frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        frame.add(controlPanel);
        frame.setJMenuBar(MenuManager.getInstance().getMenuBarFor(frame));

        final String key = "BoundsOfClientWindow";  //$NON-NLS-1$
        final PositionOption pos = new VisibilityOption(key, frame);
        GameModule.getGameModule().getPrefs().addOption(pos);
        frame.setVisible(true);
      }
    }
    else if (splitPane != null) {
      splitPane.toggleRight();
    }
    else {
      controlPanel.getTopLevelAncestor().setVisible(!controlPanel.getTopLevelAncestor().isVisible());
    }
  }

  public JPanel getControls() {
    return controlPanel;
  }

  public void setClient(ChatServerConnection c) {
    client = c;
    if (c instanceof ChatControlsInitializer) {
      if (basicControls != null) {
        basicControls.uninitializeControls(this);
      }
      if (oldClient != null) {
        oldClient.uninitializeControls(this);
      }
      basicControls = new BasicChatControlsInitializer(c);
      basicControls.initializeControls(this);
      ((ChatControlsInitializer)c).initializeControls(this);
      oldClient = (ChatControlsInitializer) c;
    }
    final PropertyChangeListener roomUpdater = evt -> {
      final Runnable runnable = () -> roomTree.setRooms((VASSAL.chat.Room[]) evt.getNewValue());
      SwingUtilities.invokeLater(runnable);
    };
    client.addPropertyChangeListener(ChatServerConnection.AVAILABLE_ROOMS, roomUpdater);
    final PropertyChangeListener currentRoomUpdater = evt -> {
      final Runnable runnable = () -> {
        if (evt.getNewValue() == null) {
          currentRoom.setRooms(new VASSAL.chat.Room[0]);
        }
        else {
          currentRoom.setRooms(new VASSAL.chat.Room[]{(VASSAL.chat.Room) evt.getNewValue()});
          final Object root = currentRoom.getModel().getRoot();
          final Object room = currentRoom.getModel().getChild(root, 0);
          currentRoom.expandPath(new TreePath(new Object[]{root, room}));
        }
      };
      SwingUtilities.invokeLater(runnable);
    };
    client.addPropertyChangeListener(ChatServerConnection.ROOM, currentRoomUpdater);
    client.addPropertyChangeListener(ChatServerConnection.CONNECTED, e -> updateConfigServerToolTipText());
  }

  public ChatServerConnection getClient() {
    return client;
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public void setAttribute(String name, Object value) {
  }

  @Override
  public String getAttributeValueString(String name) {
    return null;
  }

  public JToolBar getToolbar() {
    return toolbar;
  }

  public RoomTree getCurrentRoom() {
    return currentRoom;
  }

  public JTextField getNewRoom() {
    return newRoom;
  }

  public void addExtendedNewRoomHandler(ActionListener l) {
    newRoomButton.addActionListener(l);
    newRoomButton.setVisible(true);
  }

  public void removeExtendedNewRoomHandler(ActionListener l) {
    newRoomButton.removeActionListener(l);
    newRoomButton.setVisible(false);
  }

  public void setRoomControlsVisible(boolean visible) {
    newRoom.setVisible(visible);
    newRoomLabel.setVisible(visible);
  }

  public RoomTree getRoomTree() {
    return roomTree;
  }
}
