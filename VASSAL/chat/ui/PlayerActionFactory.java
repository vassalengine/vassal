package VASSAL.chat.ui;

import javax.swing.Action;
import javax.swing.JTree;
import VASSAL.chat.Player;

public interface PlayerActionFactory {
  Action getAction(Player p, JTree tree);
}
