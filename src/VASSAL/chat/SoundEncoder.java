/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.chat;

import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.SoundConfigurer;
import VASSAL.preferences.Prefs;

/**
 * Encodes commands that play sounds
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Jul 29, 2003
 */
public class SoundEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "PLAY\t"; //$NON-NLS-1$

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
      SoundConfigurer c = (SoundConfigurer) Prefs.getGlobalPrefs().getOption(soundKey);
      if (c != null) {
        c.play();
      }
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
