/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
package VASSAL.command;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.tools.ReadErrorDialog;

import java.io.IOException;

public class PlayAudioClipCommand extends Command {
  public static final String COMMAND_PREFIX = "AUDIO\t"; //NON-NLS

  private final String clipName;

  public PlayAudioClipCommand(String clipName) {
    this.clipName = clipName;
  }

  @Override
  protected void executeCommand() {
    try {
      if (!GlobalOptions.getInstance().isSoundGlobalMute() && !GameModule.getGameModule().getGameState().isFastForwarding()) {
        GameModule.getGameModule()
          .getDataArchive()
          .getCachedAudioClip(clipName)
          .play();
      }
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, clipName);
    }
  }

  @Override
  protected Command myUndoCommand() {
    return null;
  }

  public static PlayAudioClipCommand decode(String s) {
    if (s.startsWith(COMMAND_PREFIX)) {
      return new PlayAudioClipCommand(s.substring(COMMAND_PREFIX.length()));
    }
    else {
      return null;
    }
  }

  public String encode() {
    return COMMAND_PREFIX + clipName;
  }
}
