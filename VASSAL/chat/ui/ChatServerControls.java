package VASSAL.chat.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;
import javax.swing.BorderFactory;
import javax.swing.Box;
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
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.tree.TreePath;
import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.ServerConnection;
import VASSAL.chat.ChatServerConnection;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.VisibilityOption;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.KeyStrokeListener;

public class ChatServerControls extends AbstractBuildable {

  protected RoomTree currentRoom;
  protected JTextField newRoom;
  protected JToolBar toolbar;
  protected RoomTree roomTree;
  
  protected JButton launch;
  protected ChatServerConnection client;
  protected JPanel controlPanel;
  protected ComponentSplitter.SplitPane splitter;
  protected ChatControlsInitializer oldClient;
  protected BasicChatControlsInitializer basicControls;

  public ChatServerControls() {
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    roomTree = new RoomTree();
    JScrollPane scroll = new JScrollPane(roomTree);
    JPanel roomPanel = new JPanel();
    roomPanel.setLayout(new BoxLayout(roomPanel, BoxLayout.Y_AXIS));
    Box b = Box.createHorizontalBox();
    b.add(new JLabel("New Game: "));
    newRoom = new JTextField(12);
    newRoom.setMaximumSize(newRoom.getPreferredSize());
    b.add(newRoom);
    roomPanel.add(b);
    roomPanel.add(scroll);
    roomPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedBevelBorder(), "Active Games"));
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
    scroll.setBorder(BorderFactory.createTitledBorder(BorderFactory.createRaisedBevelBorder(), "Current Game"));
    split.setRightComponent(scroll);
    split.setDividerLocation(160);
    split.setPreferredSize(new java.awt.Dimension(320, 120));
    controlPanel = new JPanel();
    controlPanel.setLayout(new java.awt.BorderLayout());
    controlPanel.add("Center", split);
    toolbar = new JToolBar();
    controlPanel.add("North", toolbar);
    toolbar.addSeparator();
  }
  
  public void addTo(Buildable b) {
    final GameModule gm = GameModule.getGameModule();
    setClient((ChatServerConnection) gm.getServer());
    launch = new JButton("Server");
    launch.setAlignmentY(0.0F);
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        toggleVisible();
      }
    };
    launch.addActionListener(al);
    final KeyStrokeListener l = new KeyStrokeListener(al);
    l.setKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.ALT_MASK));
    URL iconURL = getClass().getResource("/images/connect.gif");
    if (iconURL != null) {
      launch.setIcon(new ImageIcon(iconURL));
      launch.setText(null);
    }
    GameModule.getGameModule().getFrame().addComponentListener(new ComponentAdapter() {
      public void componentShown(ComponentEvent e) {
        GameModule.getGameModule().getFrame().removeComponentListener(this);
        final IconConfigurer iconConfig = new IconConfigurer("serverControlsIcon", "Server controls button icon", "/images/connect.gif");
        iconConfig.setValue("/images/connect.gif");
        GlobalOptions.getInstance().addOption(iconConfig);
        iconConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            launch.setIcon(iconConfig.getIconValue());
          }
        });
        iconConfig.fireUpdate();
        final HotKeyConfigurer keyConfig = new HotKeyConfigurer("serverControlsHotKey", "Server controls hotkey", l.getKeyStroke());
        GlobalOptions.getInstance().addOption(keyConfig);
        keyConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            l.setKeyStroke((KeyStroke) keyConfig.getValue());
            launch.setToolTipText("Show/Hide the server controls [" + HotKeyConfigurer.getString(l.getKeyStroke()) + "]");
          }
        });
        keyConfig.fireUpdate();
      }
    });
    gm.addKeyStrokeListener(l);
    gm.getToolBar().add(launch);
  }

  protected void toggleVisible() {
    if (controlPanel.getTopLevelAncestor() == null) {
      if (GlobalOptions.getInstance().isUseSingleWindow()) {
        splitter = new ComponentSplitter().splitRight(GameModule.getGameModule().getControlPanel(), controlPanel, false);
        splitter.revalidate();
        Runnable runnable = new Runnable() {
          public void run() {
            splitter.showComponent();
          }
        };
        SwingUtilities.invokeLater(runnable);
      }
      else {
        JFrame frame = new JFrame("Server");
        frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        frame.getContentPane().add(controlPanel);
        String key = "BoundsOfClientWindow";
        PositionOption pos = new VisibilityOption(key, frame);
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
        Runnable runnable = new Runnable() {
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
        Runnable runnable = new Runnable() {
          public void run() {
              if (evt.getNewValue() == null) {
                currentRoom.setRooms(new VASSAL.chat.Room[0]);
              }
              else {
                currentRoom.setRooms(new VASSAL.chat.Room[]{(VASSAL.chat.Room) evt.getNewValue()});
                Object root = currentRoom.getModel().getRoot();
                Object room = currentRoom.getModel().getChild(root, 0);
                currentRoom.expandPath(new TreePath(new Object[]{root, room}));
              }
            }
        };
        SwingUtilities.invokeLater(runnable);
      }
    };
    client.addPropertyChangeListener(ChatServerConnection.ROOM, currentRoomUpdater);
  }

  public ChatServerConnection getSvrConnection() {
    return client;
  }

  public ServerConnection getClient() {
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

  public RoomTree getRoomTree() {
    return roomTree;
  }
}
