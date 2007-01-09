package VASSAL.chat;

import VASSAL.build.GameModule;
import VASSAL.command.Command;

public class MainRoomChecker {
  private boolean warnedMain = false;
  private MainRoomFilter filter = new MainRoomFilter();

  public String filter(String input, String mainRoom, String currentRoom) {
    if (GameModule.getGameModule() == null
      || input == null) {
      return input;
    }
    String output;
    if (mainRoom.equals(currentRoom)) {
      Command c = filter.apply(GameModule.getGameModule().decode(input));
      output = GameModule.getGameModule().encode(c);
      if (!warnedMain && !input.equals(output)) {
        javax.swing.JOptionPane.showMessageDialog
          (GameModule.getGameModule().getChatter(), "Chatting only (no games) allowed in " + mainRoom + ".\nCreate a new room to play");
        warnedMain = true;
      }
    }
    else {
      output = input;
    }
    return output;
  }

}
