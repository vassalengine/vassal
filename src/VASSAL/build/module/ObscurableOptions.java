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
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;

/**
 * Determines whether players are allowed to unmask other players pieces.  The module designer may
 * set the option to always on, always off, or let the players determine it with a Preferences setting.
 */
public class ObscurableOptions implements CommandEncoder, GameComponent {
  private static final ObscurableOptions instance = new ObscurableOptions();

  public static final String COMMAND_ID = "UNMASK\t"; //$NON-NLS-1$
  public static final String PREFS_KEY = "OpponentUnmaskable"; //$NON-NLS-1$
  private List<String> allowed = new ArrayList<String>();
  private Boolean override;

  private ObscurableOptions() {
  }

  /**
   * Create a private set of ObscurableOptions. If no setting are passed,
   * use the current global settings.
   * @param settings encoded settings
   */
  public ObscurableOptions(String settings) {
    this();
    if (settings != null && settings.length() > 0) {
      decodeOptions(settings);
    }
    else {
      decodeOptions(getInstance().encodeOptions());
    }
  }

  /**
   * Return the current global ObscurableOptions
   * @return global Options
   */
  public static ObscurableOptions getInstance() {
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
        GameModule.getGameModule()
                  .getServer().sendToOthers(new SetAllowed(instance.allowed));
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
      allowed.add(id);
    }
  }

  public void disallow(String id) {
    allowed.remove(id);
  }

  public Command decode(String command) {
    if (command.startsWith(COMMAND_ID)) {
      command = command.substring(COMMAND_ID.length());
      ArrayList<String> l = new ArrayList<String>();
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      while (st.hasMoreTokens()) {
        l.add(st.nextToken());
      }
      return new SetAllowed(l);
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof SetAllowed) {
      List<String> l = ((SetAllowed) c).getAllowedIds();
      if (l.isEmpty()) {
        return COMMAND_ID;
      }
      else {
        SequenceEncoder se = new SequenceEncoder('\t');
        for (String s : l) {
          se.append(s);
        }
        return COMMAND_ID + se.getValue();
      }
    }
    else {
      return null;
    }
  }

  /**
   * Encode the current ObscurableOptions as a String
   * @return encoded options
   */
  public String encodeOptions() {
    final SequenceEncoder se = new SequenceEncoder('|');
    if (override == null) {
      se.append("");
    }
    else {
      se.append(override.booleanValue());
    }
    se.append(allowed.size());
    for (String who : allowed) {
      se.append(who);
    }

    return se.getValue();
  }

  /**
   * Set the current options from an encoded string
   * @param s encoded string
   */
  public void decodeOptions(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    String setting = sd.nextToken("");
    if (setting.length()==0) {
      override = null;
    }
    else {
      override = setting.equals("true");
    }
    final int count = sd.nextInt(0);
    allowed.clear();
    for (int i = 0; i < count; i++) {
      setting = sd.nextToken("");
      if (setting.length() > 0) {
        allowed.add(setting);
      }
    }
  }

  public Command getRestoreCommand() {
    return new SetAllowed(allowed);
  }

  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      allowed.clear();
    }
    else if (Boolean.TRUE.equals(
               GameModule.getGameModule().getPrefs().getValue(PREFS_KEY))) {
      allow(GameModule.getUserId());
    }
  }

  /**
   * @return true if pieces belonging to the given id are unmaskable by
   * other players
   */
  public boolean isUnmaskable(String id) {
    return override != null ? override.booleanValue() : allowed.contains(id);
  }

  public static class SetAllowed extends Command {
    private List<String> allowed;

    public SetAllowed(List<String> allowed) {
      this.allowed = allowed;
    }

    /** @deprecated Use {@link #SetAllowed(List<String>)} instead. */
    @Deprecated
    public SetAllowed(Vector<String> allowed) {
      this.allowed = allowed;
    }

    public List<String> getAllowedIds() {
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
