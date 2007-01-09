package VASSAL.chat;

import VASSAL.command.CommandEncoder;
import VASSAL.command.Command;
import VASSAL.configure.SoundConfigurer;
import VASSAL.build.GameModule;

/**
 * Encodes commands that play sounds
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Jul 29, 2003
 */
public class SoundEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "PLAY\t";

  public Command decode(String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      return new Cmd(command.substring(COMMAND_PREFIX.length()));
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof Cmd) {
      Cmd cmd = (Cmd) c;
      s = COMMAND_PREFIX+cmd.soundKey;
    }
    return s;
  }

  public static class Cmd extends Command {
    private String soundKey;

    public Cmd(String soundKey) {
      this.soundKey = soundKey;
    }

    protected void executeCommand() {
      SoundConfigurer c = (SoundConfigurer) GameModule.getGameModule().getGlobalPrefs().getOption(soundKey);
      if (c != null) {
        c.play();
      }
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
