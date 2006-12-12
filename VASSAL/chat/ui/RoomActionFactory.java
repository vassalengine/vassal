package VASSAL.chat.ui;

import javax.swing.Action;
import javax.swing.JTree;
import VASSAL.chat.Room;

public interface RoomActionFactory {
  Action getAction(Room p, JTree tree);
}
