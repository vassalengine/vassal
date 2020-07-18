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

import javax.swing.JOptionPane;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;

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
        JOptionPane.showMessageDialog(
          GameModule.getGameModule().getChatter(), Resources.getString("Chat.chatting_only", mainRoom)); //$NON-NLS-1$
        warnedMain = true;
      }
    }
    else {
      output = input;
    }
    return output;
  }

}
