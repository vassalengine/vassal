package VASSAL.chat;

import VASSAL.build.GameModule;
import VASSAL.command.Command;

/**
 * A {@link Command} that, when executed, sends game synchronization
 * information to a given {@link VASSAL.chat.SimplePlayer}
 *
 */
public class SynchCommand extends Command {
  private Player recipient;
  private ChatServerConnection client;

  public SynchCommand(Player p, ChatServerConnection client) {
    recipient = p;
    this.client = client;
  }

  public Player getPlayer() {
    return recipient;
  }

  protected void executeCommand() {
    GameModule.getGameModule().warn("Sending game info ... ");
    Command synch = GameModule.getGameModule().getGameState().getRestoreCommand();
    if (synch != null) {
      client.sendTo(recipient, synch);
    }
  }

  protected Command myUndoCommand() {
    return null;
  }

  /**
   * Don't log synchronization requests */
  public boolean isLoggable() {
    return false;
  }
}
