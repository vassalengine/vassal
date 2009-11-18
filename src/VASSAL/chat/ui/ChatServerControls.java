/*
 * $Id$
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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
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
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.VisibilityOption;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.menu.MenuManager;

public class ChatServerControls extends AbstractBuildable {

  protected RoomTree currentRoom;
  protected JTextField newRoom;
  protected JToolBar toolbar;
  protected RoomTree roomTree;
  protected JButton newRoomButton;
  
  protected JButton launch;
  protected ChatServerConnection client;
  protected JPanel controlPanel;
  protected ComponentSplitter.SplitPane splitter;
  protected ChatControlsInitializer oldClient;
  protected BasicChatControlsInitializer basicControls;

  public ChatServerControls() {
    final JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    roomTree = new RoomTree();
    JScrollPane scroll = new JScrollPane(roomTree);
    final JPanel roomPanel = new JPanel();
    roomPanel.setLayout(new BoxLayout(roomPanel, BoxLayout.Y_AXIS));
    final JPanel b = new JPanel(new BorderLayout());
    b.add(new JLabel(Resources.getString("Chat.new_game")), BorderLayout.LINE_START);  //$NON-NLS-1$
    newRoom = new JTextField(12);
    b.add(newRoom, BorderLayout.CENTER);
    newRoomButton = new JButton("...");
    newRoomButton.setPreferredSize(new Dimension(20, 20));
    b.add(newRoomButton, BorderLayout.LINE_END);
    roomPanel.add(b);
    roomPanel.add(scroll);
    roomPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedBevelBorder(), Resources.getString("Chat.active_games")));  //$NON-NLS-1$
    split.setLeftComponent(roomPanel);
    currentRoom = new RoomTree();
    currentRoom.addTreeWillExpandListener(new javax.swing.event.TreeWillExpandListener() {
      public void treeWillCollapse(javax.swing.event.TreeExpansionEvent evt) throws javax.swing.tree.ExpandVetoException {
        throw new javax.swing.tree.ExpandVetoException(evt);
      }

      public void treeWillExpand(javax.swing.event.TreeExpansionEvent evt) throws javax.swing.tree.ExpandVetoException {
      }
    });
    scroll = new JScrollPane(currentRoom);
    scroll.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedBevelBorder(), Resources.getString("Chat.current_game")));  //$NON-NLS-1$
    split.setRightComponent(scroll);
    split.setDividerLocation(160);
    split.setPreferredSize(new java.awt.Dimension(320, 120));
    controlPanel = new JPanel();
    controlPanel.setLayout(new java.awt.BorderLayout());
    controlPanel.add("Center", split);  //$NON-NLS-1$
    toolbar = new JToolBar();
    controlPanel.add("North", toolbar);  //$NON-NLS-1$
    toolbar.addSeparator();
  }
  
  public Component getExtendedControls() {
    return null;
  }
  
  public void addTo(Buildable b) {
    final GameModule gm = GameModule.getGameModule();
    setClient((ChatServerConnection) gm.getServer());
    launch = new JButton(Resources.getString("Chat.server"));  //$NON-NLS-1$
    launch.setAlignmentY(0.0F);
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        toggleVisible();
      }
    };
    launch.addActionListener(al);
    final NamedKeyStrokeListener l = new NamedKeyStrokeListener(al);
    l.setKeyStroke(NamedKeyStroke.getNamedKeyStroke(KeyEvent.VK_S, InputEvent.ALT_MASK));
    URL iconURL = getClass().getResource("/images/connect.gif");  //$NON-NLS-1$
    if (iconURL != null) {
      launch.setIcon(new ImageIcon(iconURL));
      launch.setText(null);
    }

    final IconConfigurer iconConfig = new IconConfigurer("serverControlsIcon", Resources.getString("Chat.server_controls_button_icon"), "/images/connect.gif"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    iconConfig.setValue("/images/connect.gif");  //$NON-NLS-1$
    GlobalOptions.getInstance().addOption(iconConfig);
    iconConfig.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        launch.setIcon(iconConfig.getIconValue());
      }
    });
    iconConfig.fireUpdate();
    
    final NamedHotKeyConfigurer keyConfig = new NamedHotKeyConfigurer("serverControlsHotKey", Resources.getString("Chat.server_controls_hotkey"), l.getNamedKeyStroke());   //$NON-NLS-1$ //$NON-NLS-2$
    GlobalOptions.getInstance().addOption(keyConfig);
    keyConfig.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        l.setKeyStroke(keyConfig.getValueNamedKeyStroke());
        launch.setToolTipText(Resources.getString("Chat.server_controls_tooltip", NamedHotKeyConfigurer.getString(l.getKeyStroke())));  //$NON-NLS-1$
      }
    });
    keyConfig.fireUpdate();
      
    gm.addKeyStrokeListener(l);
    gm.getToolBar().add(launch);
  }

  public void toggleVisible() {
    if (controlPanel.getTopLevelAncestor() == null) {
      if (GlobalOptions.getInstance().isUseSingleWindow()) {
        splitter = new ComponentSplitter().splitRight(GameModule.getGameModule().getControlPanel(), controlPanel, false);
        splitter.revalidate();
        final Runnable runnable = new Runnable() {
          public void run() {
            splitter.showComponent();
          }
        };
        SwingUtilities.invokeLater(runnable);
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
    else if (splitter != null) {
      splitter.toggleVisibility();
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
    PropertyChangeListener roomUpdater = new PropertyChangeListener() {
      public void propertyChange(final PropertyChangeEvent evt) {
        final Runnable runnable = new Runnable() {
          public void run() {
            roomTree.setRooms((VASSAL.chat.Room[]) evt.getNewValue());
          }
        };
        SwingUtilities.invokeLater(runnable);
      }
    };
    client.addPropertyChangeListener(ChatServerConnection.AVAILABLE_ROOMS, roomUpdater);
    PropertyChangeListener currentRoomUpdater = new PropertyChangeListener() {
      public void propertyChange(final PropertyChangeEvent evt) {
        final Runnable runnable = new Runnable() {
          public void run() {
              if (evt.getNewValue() == null) {
                currentRoom.setRooms(new VASSAL.chat.Room[0]);
              }
              else {
                currentRoom.setRooms(new VASSAL.chat.Room[]{(VASSAL.chat.Room) evt.getNewValue()});
                final Object root = currentRoom.getModel().getRoot();
                final Object room = currentRoom.getModel().getChild(root, 0);
                currentRoom.expandPath(new TreePath(new Object[]{root, room}));
              }
            }
        };
        SwingUtilities.invokeLater(runnable);
      }
    };
    client.addPropertyChangeListener(ChatServerConnection.ROOM, currentRoomUpdater);
  }

  public ChatServerConnection getClient() {
    return client;
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String name, Object value) {
  }

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
  
  public JButton getNewRoomButton() {
    return newRoomButton;
  }

  public RoomTree getRoomTree() {
    return roomTree;
  }
}
