package VASSAL.chat;

import VASSAL.build.module.Chatter;
import VASSAL.command.Command;
import VASSAL.command.CommandFilter;

/**
 * Only passes commands that are allowed in the Main Room
 */
public class MainRoomFilter extends CommandFilter {
  public MainRoomFilter() {
  }

  protected boolean accept(Command c) {
    return c instanceof Chatter.DisplayText
        || c instanceof PrivMsgCommand
        || c instanceof SoundEncoder.Cmd;
  }
}

