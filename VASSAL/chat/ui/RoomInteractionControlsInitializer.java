package VASSAL.chat.ui;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimpleRoom;

/**
 * Adds mouse listeners to the RoomTree components: double-click to join a room, etc. Builds a popup when right-clicking
 * on a player or room
 * 
 * @author rkinney
 * 
 */
public class RoomInteractionControlsInitializer implements ChatControlsInitializer {
  public static final Font POPUP_MENU_FONT = new Font("Dialog", 0, 10);
  private List playerActionFactories = new ArrayList();
  private List roomActionFactories = new ArrayList();
  protected ChatServerConnection client;
  private MouseAdapter currentRoomPopupBuilder;
  private MouseAdapter roomPopupBuilder;
  private ActionListener roomCreator;

  public RoomInteractionControlsInitializer(ChatServerConnection client) {
    super();
    this.client = client;
  }

  public void initializeControls(final ChatServerControls controls) {
    currentRoomPopupBuilder = new MouseAdapter() {
      public void mouseReleased(MouseEvent evt) {
        JTree tree = (JTree) evt.getSource();
        if (evt.isMetaDown()) {
          TreePath path = tree.getPathForLocation(evt.getX(), evt.getY());
          if (path != null) {
            Object target = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
            if (target instanceof Player) {
              JPopupMenu popup = buildPopupForPlayer((Player) target, tree);
              if (popup != null) {
                for (int i = 0, n = popup.getComponentCount(); i < n; ++i) {
                  popup.getComponent(i).setFont(POPUP_MENU_FONT);
                }
                popup.show(tree, evt.getX(), evt.getY());
              }
            }
          }
        }
      }
    };
    controls.getCurrentRoom().addMouseListener(currentRoomPopupBuilder);
    roomPopupBuilder = new MouseAdapter() {
      public void mouseReleased(MouseEvent evt) {
        JTree tree = (JTree) evt.getSource();
        TreePath path = tree.getPathForLocation(evt.getX(), evt.getY());
        if (path != null) {
          Object target = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
          if (target instanceof Player) {
            if (evt.isMetaDown()) {
              JPopupMenu popup = buildPopupForPlayer((Player) target, tree);
              for (int i = 0, n = popup.getComponentCount(); i < n; ++i) {
                popup.getComponent(i).setFont(POPUP_MENU_FONT);
              }
              popup.show(tree, evt.getX(), evt.getY());
            }
          }
          else if (target instanceof SimpleRoom) {
            if (evt.isMetaDown()) {
              JPopupMenu popup = buildPopupForRoom((VASSAL.chat.Room) target, tree);
              for (int i = 0, n = popup.getComponentCount(); i < n; ++i) {
                popup.getComponent(i).setFont(POPUP_MENU_FONT);
              }
              popup.show(tree, evt.getX(), evt.getY());
            }
            else if (evt.getClickCount() == 2) {
              int row = tree.getRowForLocation(evt.getX(), evt.getY());
              if (tree.isCollapsed(row)) {
                tree.expandRow(row);
              }
              else {
                tree.collapseRow(row);
              }
              doubleClickRoom((VASSAL.chat.Room) target, tree);
            }
          }
        }
      }
    };
    controls.getRoomTree().addMouseListener(roomPopupBuilder);
    roomCreator = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        createRoom(controls.getNewRoom().getText());
        controls.getNewRoom().setText("");
      }
    };
    controls.getNewRoom().addActionListener(roomCreator);
  }

  protected void createRoom(String name) {
    client.setRoom(new SimpleRoom(name));
  }

  public JPopupMenu buildPopupForRoom(Room room, JTree tree) {
    JPopupMenu popup = new JPopupMenu();
    for (Iterator it = roomActionFactories.iterator(); it.hasNext();) {
      RoomActionFactory f = (RoomActionFactory) it.next();
      popup.add(f.getAction(room, tree));
    }
    // popup.add(new JoinRoomAction(target, source.getSvrConnection()));
    return popup.getComponentCount() == 0 ? null : popup;
  }

  public void doubleClickRoom(Room room, JTree tree) {
    if (!room.equals(client.getRoom())) {
      new JoinRoomAction(room, client).actionPerformed(null);
    }
  }

  public void addPlayerActionFactory(PlayerActionFactory f) {
    playerActionFactories.add(f);
  }

  public void addRoomActionFactory(RoomActionFactory f) {
    roomActionFactories.add(f);
  }

  public JPopupMenu buildPopupForPlayer(Player target, JTree tree) {
    JPopupMenu popup = new JPopupMenu();
    for (Iterator it = playerActionFactories.iterator(); it.hasNext();) {
      PlayerActionFactory f = (PlayerActionFactory) it.next();
      popup.add(f.getAction(target, tree));
    }
    // popup.add(new ShowProfileAction(target, (Frame) SwingUtilities.getAncestorOfClass(Frame.class, tree)));
    // popup.add(new PrivateMessageAction(target, source.getSvrConnection(), source.getPrivateChatManager()));
    // popup.add(new SendSoundAction("Send Wake-up", source.getSvrConnection(), WAKE_UP_SOUND, target));
    return popup.getComponentCount() == 0 ? null : popup;
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getRoomTree().removeMouseListener(roomPopupBuilder);
    controls.getCurrentRoom().removeMouseListener(currentRoomPopupBuilder);
    controls.getNewRoom().removeActionListener(roomCreator);
  }
}
