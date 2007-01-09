package VASSAL.chat;


import javax.swing.*;
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
}
