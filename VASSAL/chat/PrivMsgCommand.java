package VASSAL.chat;

import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import VASSAL.command.Command;

/**
 * A Command that encapsulates a private chat message from another
 * {@link VASSAL.chat.SimplePlayer} */
public class PrivMsgCommand extends Command {
  private PrivateChatManager mgr;
  private String msg;
  private Player p;

  public PrivMsgCommand(PrivateChatManager mgr, Player sender, String msg) {
    this.mgr = mgr;
    this.msg = msg;
    p = (Player) sender;
  }

  public void executeCommand() {
    PrivateChatter chat = mgr.getChatterFor(p);
    if (chat == null) {
      return;
    }
    java.awt.Window f = SwingUtilities.getWindowAncestor(chat);
    if (!f.isVisible()) {
      f.setVisible(true);
      if (SwingUtilities.findFocusOwner(f) == null) {
        java.awt.Toolkit.getDefaultToolkit().beep();
        for (int i = 0,j = chat.getComponentCount(); i < j; ++i) {
          if (chat.getComponent(i) instanceof JTextField) {
            (chat.getComponent(i)).requestFocus();
            break;
          }
        }
      }
    }
    else {
      f.toFront();
    }
    chat.show(msg);
  }

  public Command myUndoCommand() {
    return null;
  }

  /**
   * Return true, as this command should not be logged
   */
  public boolean isLoggable() {
    return false;
  }

  public Player getSender() {
    return p;
  }

  public String getMessage() {
    return msg;
  }
}
