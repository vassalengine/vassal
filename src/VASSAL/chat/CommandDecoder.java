/*
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;
import VASSAL.command.Command;

/**
 * Listens for incoming messages (PropertyChangeEvents with name {@link ChatServerConnection.INCOMING_MSG}) and
 * interprets the message as a command to be executed
 *
 * @author rodneykinney
 *
 */
public class CommandDecoder implements PropertyChangeListener {
  public void propertyChange(PropertyChangeEvent evt) {
    final Command c = GameModule.getGameModule().decode((String) evt.getNewValue());
    if (c != null) {
      Runnable runnable = new Runnable() {
        public void run() {
          c.execute();
          GameModule.getGameModule().getLogger().log(c);
        }
      };
      SwingUtilities.invokeLater(runnable);
    }
  }
}
