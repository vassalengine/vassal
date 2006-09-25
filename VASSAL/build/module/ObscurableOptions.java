/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Sep 3, 2002
 * Time: 10:13:28 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.Vector;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;

/**
 * Determines whether players are allowed to un-mask other players pieces.  The module designer may
 * set the option to always on, always off, or let the players determine it with a Preferences setting.
 */
public class ObscurableOptions implements CommandEncoder, GameComponent {
  private static ObscurableOptions instance;
  public static final String COMMAND_ID = "UNMASK\t";
  public static final String PREFS_KEY = "OpponentUnmaskable";
  private Vector allowed = new Vector();
  private Boolean override;

  private ObscurableOptions() {
  }

  public static ObscurableOptions getInstance() {
    if (instance == null) {
      instance = new ObscurableOptions();
    }
    return instance;
  }

  public void allowSome(String preferencesPrompt) {
    Configurer c = new BooleanConfigurer(PREFS_KEY, preferencesPrompt);
    GameModule.getGameModule().getPrefs().addOption(c);
    c.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        if (Boolean.TRUE.equals(evt.getNewValue())) {
          ObscurableOptions.getInstance().allow(GameModule.getUserId());
          String side = PlayerRoster.getMySide();
          if (side != null) {
            ObscurableOptions.getInstance().allow(side);
          }
        }
        else {
          ObscurableOptions.getInstance().disallow(GameModule.getUserId());
          String side = PlayerRoster.getMySide();
          if (side != null) {
            ObscurableOptions.getInstance().disallow(side);
          }
        }
        GameModule.getGameModule().getServer().sendToOthers(new SetAllowed(instance.allowed));
      }
    });
    if (Boolean.TRUE.equals(c.getValue())) {
      allow(GameModule.getUserId());
    }
    else {
      disallow(GameModule.getUserId());
    }
  }

  /**
   * Set the text accompanying the "Allow opponent to unmask" control in the Preferences
   */
  public void setPrompt(String preferencesPrompt) {
    Configurer c = GameModule.getGameModule().getPrefs().getOption(PREFS_KEY);
    if (c != null) {
      c.setName(preferencesPrompt);
    }
  }

  public void allowAll() {
    override = Boolean.TRUE;
  }

  public void allowNone() {
    override = Boolean.FALSE;
  }

  public void allow(String id) {
    if (!allowed.contains(id)) {
      allowed.addElement(id);
    }
  }

  public void disallow(String id) {
    allowed.removeElement(id);
  }

  public Command decode(String command) {
    if (command.startsWith(COMMAND_ID)) {
      command = command.substring(COMMAND_ID.length());
      Vector v = new Vector();
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      while (st.hasMoreTokens()) {
        v.addElement(st.nextToken());
      }
      return new SetAllowed(v);
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof SetAllowed) {
      Vector v = ((SetAllowed) c).getAllowedIds();
      if (v.size() == 0) {
        return COMMAND_ID;
      }
      else {
        SequenceEncoder se = new SequenceEncoder('\t');
        for (Enumeration e = v.elements(); e.hasMoreElements();) {
          se.append((String) e.nextElement());
        }
        return COMMAND_ID + se.getValue();
      }
    }
    else {
      return null;
    }
  }

  public Command getRestoreCommand() {
    return new SetAllowed(allowed);
  }

  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      allowed.removeAllElements();
    }
    else if (Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(PREFS_KEY))) {
      allow(GameModule.getUserId());
    }
  }

  /** Return true if pieces belonging to the given id are un-maskable by other players */
  public boolean isUnmaskable(String id) {
    if (override != null) {
      return override.booleanValue();
    }
    else {
      return allowed.contains(id);
    }
  }

  public static class SetAllowed extends Command {
    private Vector allowed = new Vector();

    public SetAllowed(Vector allowed) {
      this.allowed = allowed;
    }

    public Vector getAllowedIds() {
      return allowed;
    }

    protected void executeCommand() {
      ObscurableOptions.getInstance().override = null;
      ObscurableOptions.getInstance().allowed = this.allowed;
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
