package VASSAL.chat.ui;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JTree;
import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.SynchCommand;

/**
 * When invoked, will request synchronization info from another player
 */
public class SynchAction extends AbstractAction {
  private Player p;
  private ChatServerConnection client;

  public SynchAction(Player p, ChatServerConnection client) {
    super("Synchronize");
    this.p = p;
    this.client = client;
    if (p != null
      && GameModule.getGameModule() != null
      && !p.equals(client.getUserInfo())) {
      setEnabled(true);
    }
    else {
      setEnabled(false);
    }
  }

  public void actionPerformed(ActionEvent evt) {
    if (isEnabled()) {
      GameModule.getGameModule().getGameState().setup(false);
      client.sendTo(p, new SynchCommand(client.getUserInfo(),client));
    }
  }
  
  public static PlayerActionFactory factory(final ChatServerConnection client) {
    return new PlayerActionFactory() {
      public Action getAction(Player p, JTree tree) {
        return new SynchAction(p,client);
      }
    };
  }
}
