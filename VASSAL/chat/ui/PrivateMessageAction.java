package VASSAL.chat.ui;


import javax.swing.*;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.PrivateChatManager;
import VASSAL.chat.PrivateChatter;
import java.awt.event.ActionEvent;

/**
 * When invoked, will open a private message window to another player
 */
public class PrivateMessageAction extends AbstractAction {
    private Player p;
    private PrivateChatManager mgr;

    public PrivateMessageAction(Player p, ChatServerConnection client, PrivateChatManager mgr) {
	super("Private Msg");
	this.p = p;
	this.mgr = mgr;
	setEnabled(p != null 
		   && client != null
		   && mgr != null 
		   && !p.equals(client.getUserInfo()));
    }
    public void actionPerformed(ActionEvent evt) {
	PrivateChatter chat = mgr.getChatterFor(p);
	java.awt.Window f = (java.awt.Window)chat.getTopLevelAncestor();
	f.setVisible(true);
	f.toFront();
    }
    
    public static PlayerActionFactory factory(final ChatServerConnection client, final PrivateChatManager chatMgr) {
      return new PlayerActionFactory() {
        public Action getAction(Player p, JTree tree) {
          return new PrivateMessageAction(p, client, chatMgr);
        }
      };
    }
}
