package VASSAL.chat.ui;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import VASSAL.chat.Player;
import VASSAL.chat.PlayerInfoWindow;
import VASSAL.chat.SimplePlayer;

/**
 * When invoked, will show profile information about another player
 */
public class ShowProfileAction extends AbstractAction {
  private SimplePlayer p;
  private java.awt.Frame f;

  public ShowProfileAction(SimplePlayer p, java.awt.Frame f) {
    super("Show Profile");
    this.p = p;
    this.f = f;
  }

  public void actionPerformed(ActionEvent evt) {
    new PlayerInfoWindow(f, p).setVisible(true);
  }

  public static PlayerActionFactory factory() {
    return new PlayerActionFactory() {
      public Action getAction(Player p, JTree tree) {
        return new ShowProfileAction((SimplePlayer) p, (java.awt.Frame) SwingUtilities.getAncestorOfClass(Frame.class, tree));
      }
    };
  }
}
