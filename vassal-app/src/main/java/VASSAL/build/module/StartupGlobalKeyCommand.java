/*
 *
 * Copyright (c) 2010 by Pieter Geerkens
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

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A Global Key Command that is automatically invoked on game start-up,
 * once the various Key Listeners have been started.
 * <p>
 * As of 3.6, multiple Startup Global Key Commands can be depended on to
 * process in the correct order.
 *
 * @author Pieter Geerkens
 *
 */
public class StartupGlobalKeyCommand extends GlobalKeyCommand implements GameComponent, CommandEncoder, UniqueIdManager.Identifyable {
  public static final String WHEN_TO_APPLY                 = "whenToApply";          //NON-NLS
  public static final String APPLY_FIRST_LAUNCH_OF_SESSION = "firstLaunchOfSession"; //NON-NLS
  public static final String APPLY_EVERY_LAUNCH_OF_SESSION = "everyLaunchOfSession"; //NON-NLS
  public static final String APPLY_START_OF_GAME_ONLY      = "startOfGameOnly";      //NON-NLS

  private static final char DELIMITER = '\t'; //$NON-NLS-1$
  public static final String COMMAND_PREFIX = "SGKC" + DELIMITER; //NON-NLS-1$

  protected static final UniqueIdManager idMgr = new UniqueIdManager("SGKC"); //$NON-NLS-1$
  protected String id = "";     // Our unique ID

  public String whenToApply = APPLY_EVERY_LAUNCH_OF_SESSION;

  private boolean hasEverApplied     = false;  // Has ever been applied during this session   (NOT saved with game state)
  private boolean hasAppliedThisGame = false;  // Has ever been applied during this *game*    (Saved with game state)

  public static class Prompt extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ APPLY_FIRST_LAUNCH_OF_SESSION, APPLY_EVERY_LAUNCH_OF_SESSION, APPLY_START_OF_GAME_ONLY };
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.StartupGlobalKeyCommand.first_launch_of_session",
        "Editor.StartupGlobalKeyCommand.every_launch_of_session",
        "Editor.StartupGlobalKeyCommand.start_of_game_only"
      };
    }
  }

  @SuppressWarnings("removal")
  public StartupGlobalKeyCommand() {
    super();
    condition = null;
    /* These four fields pertaining to the physical representation of the
     * GKC on the toolbar are not applicable in this implementation.
     */
    launch.setAttribute(BUTTON_TEXT, "");  //NON-NLS
    launch.setAttribute(TOOLTIP, "");  //NON-NLS
    launch.setAttribute(ICON, "");  //NON-NLS
    launch.setAttribute(HOTKEY, "");  //NON-NLS
  }

  @SuppressWarnings("removal")
  public StartupGlobalKeyCommand(MassKeyCommand gkc) {
    super(gkc);
    condition = null;
    /* These four fields pertaining to the physical representation of the
     * GKC on the toolbar are not applicable in this implementation.
     */
    launch.setAttribute(BUTTON_TEXT, "");  //NON-NLS
    launch.setAttribute(TOOLTIP, "");  //NON-NLS
    launch.setAttribute(ICON, "");  //NON-NLS
    launch.setAttribute(HOTKEY, "");  //NON-NLS
  }


  //---------------------- GlobalKeyCommand extension ---------------------
  @Override
  public void addTo(Buildable parent) {
    idMgr.add(this);
    super.addTo(parent);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
  }

  @Override
  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(this);
    idMgr.remove(this);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.StartupGlobalKeyCommand.component_type");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "StartupGlobalKeyCommand"); //NON-NLS
  }

  @SuppressWarnings("removal")
  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    if (List.of(BUTTON_TEXT, TOOLTIP, ICON, HOTKEY).contains(key)) {
      return () -> false;
    }
    else {
      return super.getAttributeVisibility(key);
    }
  }

  @Override
  public String[] getAttributeDescriptions() {
    final List<String> descs = new ArrayList<>();
    descs.add(Resources.getString("Editor.StartupGlobalKeyCommand.when_to_apply"));
    Collections.addAll(descs, super.getAttributeDescriptions());
    return descs.toArray(new String[0]);
  }

  @Override
  public String[] getAttributeNames() {
    final List<String> names = new ArrayList<>();

    names.add(WHEN_TO_APPLY);

    // Filter some of the crazy out of the original MassKeyCommand list, so we can add more things "safely"
    for (final String n : super.getAttributeNames()) {
      if (List.of(CHECK_VALUE, CHECK_PROPERTY, AFFECTED_PIECE_NAMES).contains(n)) {
        continue;
      }
      names.add(n);
    }

    return names.toArray(new String[0]);
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    final List<Class<?>> types = new ArrayList<>();
    types.add(Prompt.class);
    Collections.addAll(types, super.getAttributeTypes());
    return types.toArray(new Class<?>[0]);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (WHEN_TO_APPLY.equals(key)) {
      if (value instanceof String) {
        whenToApply = (String)value;
      }
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (WHEN_TO_APPLY.equals(key)) {
      return whenToApply;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }


  /**
   * Apply the command, but only if it hasn't been marked as already-applied (by whatever its when-to-apply parameters are)
   * @return true if command was applied
   */
  public boolean applyIfNotApplied() {
    if (APPLY_FIRST_LAUNCH_OF_SESSION.equals(whenToApply)) {
      if (hasEverApplied) {
        return false;
      }
    }
    else if (APPLY_START_OF_GAME_ONLY.equals(whenToApply)) {
      if (hasAppliedThisGame) {
        return false;
      }
    }

    hasEverApplied = true;     // This one will be false again next time anything calls GameState.setup(true)
    hasAppliedThisGame = true; // This one will be remembered as part of the game state (i.e. even after loading a game)
    apply();
    return true;
  }


  @Override
  public void setup(boolean gameStarting) {

  }

  /**
   * When initializing a new game from a Predefined Setup that loads a saved game, mark that this is actually a fresh game rather than a load of an old one
   */
  public void freshGame() {
    hasAppliedThisGame = false;
  }

  @Override
  public Command getRestoreCommand() {
    return new UpdateStartupGlobalKeyCommand(this, hasAppliedThisGame);
  }


  /**
   * Sets our unique ID (among Startup Global Key Commands), so that multiple SGKCs can sort their save/restore commands from each other
   * @param id Sets our unique ID
   */
  @Override
  public void setId(String id) {
    this.id = id;
  }

  /**
   * @return unique ID of this SGKC
   */
  @Override
  public String getId() {
    return id;
  }

  /**
   * Deserializes our command from a string version, if the command belongs to us.
   * @param command Serialized string command
   * @return An {@link ChessClockControl.UpdateStartupGlobalKeyCommand}
   */
  @Override
  public Command decode(final String command) {
    if (!command.startsWith(COMMAND_PREFIX + getId() + DELIMITER)) {
      return null;
    }

    final SequenceEncoder.Decoder decoder = new SequenceEncoder.Decoder(command, DELIMITER);
    decoder.nextToken(); // Skip over the Command Prefix
    decoder.nextToken(); // Skip over the Id
    final boolean applied = decoder.nextBoolean(true);

    return new UpdateStartupGlobalKeyCommand(this, applied);
  }

  /**
   * Serializes our command into a string, if it belongs to us
   * @param c Command to serialize. Only serialized if it's an UpdateClockControlCommand.
   * @return Serialized command, or null if command passed wasn't an UpdateClockControlCommand.
   */
  @Override
  public String encode(final Command c) {
    if (!(c instanceof UpdateStartupGlobalKeyCommand)) {
      return null;
    }
    final UpdateStartupGlobalKeyCommand comm = (UpdateStartupGlobalKeyCommand) c;
    final SequenceEncoder encoder = new SequenceEncoder(DELIMITER);
    encoder.append(comm.getId());
    encoder.append(comm.appliedThisGame);

    if (!id.equals(comm.getId())) {
      return null;
    }

    return COMMAND_PREFIX + encoder.getValue();
  }


  /**
   * Our "command" format for remembering whether the key command has been applied during this game
   */
  private static class UpdateStartupGlobalKeyCommand extends Command {
    private final boolean appliedThisGame;
    StartupGlobalKeyCommand sgkc;

    public UpdateStartupGlobalKeyCommand(StartupGlobalKeyCommand sgkc, boolean appliedThisGame) {
      this.sgkc = sgkc;
      this.appliedThisGame = appliedThisGame;
    }

    @Override
    protected void executeCommand() {
      sgkc.hasAppliedThisGame = appliedThisGame;
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }

    public String getId() {
      return sgkc.getId();
    }
  }
}
