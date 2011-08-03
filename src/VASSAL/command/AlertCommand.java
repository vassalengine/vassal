/*
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

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;

/** This command, when executed, displays a Dialog box with a message */
public class AlertCommand extends Command {
  private String msg;

  public AlertCommand(String msg) {
    this.msg = msg;
  }

  protected void executeCommand() {
    Runnable runnable = new Runnable() {
      public void run() {
        JOptionPane.showMessageDialog(GameModule.getGameModule() == null ? null : GameModule.getGameModule().getFrame(), msg);
      }
    };
    SwingUtilities.invokeLater(runnable);
  }

  protected Command myUndoCommand() {
    return null;
  }

  public String getMessage() {
    return msg;
  }
}
