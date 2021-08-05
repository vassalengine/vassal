/*
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
package VASSAL.build.module;

import VASSAL.tools.ProblemDialog;
import java.util.ArrayList;
import java.util.List;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;
import java.util.Vector;

/**
 * Determines whether players are allowed to unmask other players pieces.  The module designer may
 * set the option to always on, always off, or let the players determine it with a Preferences setting.
 */
public class ObscurableOptions implements CommandEncoder, GameComponent {
  private static final ObscurableOptions instance = new ObscurableOptions();

  public static final String COMMAND_ID = "UNMASK\t"; //$NON-NLS-1$
  public static final String PREFS_KEY = "OpponentUnmaskable"; //$NON-NLS-1$
  private List<String> allowed = new ArrayList<>();
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
    final Configurer c = new BooleanConfigurer(PREFS_KEY, preferencesPrompt);
    GameModule.getGameModule().getPrefs().addOption(c);
    c.addPropertyChangeListener(evt -> {
      if (Boolean.TRUE.equals(evt.getNewValue())) {
        ObscurableOptions.getInstance().allow(GameModule.getActiveUserId());
        final String side = PlayerRoster.getMySide();
        if (side != null) {
          ObscurableOptions.getInstance().allow(side);
        }
      }
      else {
        ObscurableOptions.getInstance().disallow(GameModule.getActiveUserId());
        final String side = PlayerRoster.getMySide();
        if (side != null) {
          ObscurableOptions.getInstance().disallow(side);
        }
      }
      GameModule.getGameModule()
                .getServer().sendToOthers(new SetAllowed(instance.allowed));
    });
    if (Boolean.TRUE.equals(c.getValue())) {
      allow(GameModule.getActiveUserId());
    }
    else {
      disallow(GameModule.getActiveUserId());
    }
  }

  /**
   * Set the text accompanying the "Allow opponent to unmask" control in the Preferences
   * No longer required with new Configurers. Caused double-up label in config display.
   * @deprecated No replacement
   */
  @Deprecated (since = "2020-10-27", forRemoval = true)
  public void setPrompt(String preferencesPrompt) {
    ProblemDialog.showDeprecated("2020-10-27");
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

  @Override
  public Command decode(String command) {
    if (!command.startsWith(COMMAND_ID)) {
      return null;
    }

    command = command.substring(COMMAND_ID.length());
    final List<String> l = new ArrayList<>();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    return new SetAllowed(l);
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof SetAllowed)) {
      return null;
    }

    final List<String> l = ((SetAllowed) c).getAllowedIds();
    final SequenceEncoder se = new SequenceEncoder('\t');
    for (final String s : l) {
      se.append(s);
    }
    return COMMAND_ID + se.getValue();
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
      se.append(override);
    }
    se.append(allowed.size());
    for (final String who : allowed) {
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
    String setting = sd.nextToken(""); //NON-NLS
    if (setting.length() == 0) {
      override = null;
    }
    else {
      override = setting.equals("true"); //NON-NLS
    }
    final int count = sd.nextInt(0);
    allowed.clear();
    for (int i = 0; i < count; i++) {
      setting = sd.nextToken(""); //NON-NLS
      if (setting.length() > 0) {
        allowed.add(setting);
      }
    }
  }

  @Override
  public Command getRestoreCommand() {
    return new SetAllowed(allowed);
  }

  @Override
  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      allowed.clear();
    }
    else if (Boolean.TRUE.equals(
               GameModule.getGameModule().getPrefs().getValue(PREFS_KEY))) {
      allow(GameModule.getActiveUserId());
    }
  }

  /**
   * @return true if pieces belonging to the given id are unmaskable by
   * other players
   */
  public boolean isUnmaskable(String id) {
    return override != null ? override : allowed.contains(id);
  }

  public static class SetAllowed extends Command {
    private final List<String> allowed;

    public SetAllowed(List<String> allowed) {
      this.allowed = allowed;
    }

    /** @deprecated Use {@link #SetAllowed(List)} instead. */
    @Deprecated(since = "2020-08-06", forRemoval = true)
    public SetAllowed(Vector<String> allowed) { //NOPMD
      ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
      this.allowed = allowed;
    }

    public List<String> getAllowedIds() {
      return allowed;
    }

    @Override
    protected void executeCommand() {
      ObscurableOptions.getInstance().override = null;
      ObscurableOptions.getInstance().allowed = this.allowed;
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }
}
