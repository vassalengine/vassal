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
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.VisibilityAND;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A Global Key Command that is automatically invoked on game start-up,
 * once the various Key Listeners have been started.
 * <p>
 * As of 3.6, multiple Startup Global Key Commands can be depended on to
 * process in the correct order.
 * As of 3.7, a global hotkey can be sent instead of a GKC
 *
 * @author Pieter Geerkens, Brian Reynolds
 *
 */
public class StartupGlobalKeyCommand extends GlobalKeyCommand
        implements GameComponent,
        CommandEncoder, UniqueIdManager.Identifyable, PlayerRoster.SideChangeListener {
  public static final String WHEN_TO_APPLY                   = "whenToApply";          //NON-NLS
  public static final String APPLY_FIRST_LAUNCH_OF_SESSION   = "firstLaunchOfSession"; //NON-NLS
  public static final String APPLY_EVERY_LAUNCH_OF_SESSION   = "everyLaunchOfSession"; //NON-NLS
  public static final String APPLY_START_OF_GAME_ONLY        = "startOfGameOnly";      //NON-NLS
  public static final String APPLY_START_GAME_OR_SIDE_CHANGE = "sideChange";           //NON-NLS
  public static final String APPLY_SIDE_CHANGE               = "onlySideChange";       //NON-NLS
  public static final String GLOBAL_HOTKEY                   = "globalHotkey";         //NON-NLS
  public static final String HOTKEY_OR_KEY_COMMAND           = "hotkeyOrKeyCommand";   //NON-NLS

  public static final String SEND_KEY_COMMAND = "sendKeyCommand"; //NON-NLS
  public static final String SEND_HOTKEY = "sendHotkey"; //NON-NLS

  public static final String[] SEND_OPTIONS = { SEND_KEY_COMMAND, SEND_HOTKEY };
  public static final String[] SEND_KEYS = { "Editor.StartupGlobalKeyCommand.send_key_command", "Editor.StartupGlobalKeyCommand.send_hotkey" };

  private static final char DELIMITER = '\t'; //$NON-NLS-1$
  public static final String COMMAND_PREFIX = "SGKC" + DELIMITER; //NON-NLS-1$

  protected static final UniqueIdManager idMgr = new UniqueIdManager("SGKC"); //$NON-NLS-1$
  protected String id = "";     // Our unique ID

  protected String hotkeyOrKeyCommand = SEND_KEY_COMMAND;
  protected NamedKeyStroke globalHotkey = NamedKeyStroke.NULL_KEYSTROKE;

  public String whenToApply = APPLY_EVERY_LAUNCH_OF_SESSION;

  private boolean hasEverApplied     = false;  // Has ever been applied during this session   (NOT saved with game state)
  private boolean hasAppliedThisGame = false;  // Has ever been applied during this *game*    (Saved with game state)
  private String sideChangedSummary;

  public static class Prompt extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ APPLY_FIRST_LAUNCH_OF_SESSION, APPLY_EVERY_LAUNCH_OF_SESSION, APPLY_START_OF_GAME_ONLY, APPLY_START_GAME_OR_SIDE_CHANGE,
                           APPLY_SIDE_CHANGE};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.StartupGlobalKeyCommand.first_launch_of_session",
        "Editor.StartupGlobalKeyCommand.every_launch_of_session",
        "Editor.StartupGlobalKeyCommand.start_of_game_only",
        "Editor.StartupGlobalKeyCommand.start_or_side_change",
        "Editor.StartupGlobalKeyCommand.only_side_change"
      };
    }
  }


  public static class SendConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return SEND_OPTIONS;
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return SEND_KEYS;
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
    setShowDisabledOptions(false);
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
    setShowDisabledOptions(false);
  }


  //---------------------- GlobalKeyCommand extension ---------------------
  @Override
  public void addTo(Buildable parent) {
    idMgr.add(this);
    super.addTo(parent);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
    // Attach the listener only for the side change startup GKC.
    if (APPLY_SIDE_CHANGE.equals(whenToApply)) {
      GameModule.getGameModule().addSideChangeListenerToPlayerRoster(this);
    }
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
    return HelpFile.getReferenceManualPage("GlobalKeyCommands.html", "startup"); //NON-NLS
  }

  @SuppressWarnings("removal")
  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    if (List.of(BUTTON_TEXT, TOOLTIP, ICON, HOTKEY).contains(key)) {
      return () -> false;
    }
    else if (List.of(WHEN_TO_APPLY, HOTKEY_OR_KEY_COMMAND).contains(key)) {
      return () -> true;
    }
    else if (List.of(GLOBAL_HOTKEY).contains(key)) {
      return () -> SEND_HOTKEY.equals(hotkeyOrKeyCommand);
    }
    else if ((getNameKey() != null) && getNameKey().equals(key)) {
      return () -> true;
    }
    else {
      return new VisibilityAND(() -> SEND_KEY_COMMAND.equals(hotkeyOrKeyCommand), super.getAttributeVisibility(key));
    }
  }

  @Override
  public String[] getAttributeDescriptions() {
    final List<String> descs = new ArrayList<>();
    descs.add(Resources.getString("Editor.StartupGlobalKeyCommand.when_to_apply"));
    descs.add(Resources.getString("Editor.StartupGlobalKeyCommand.what_to_apply"));
    descs.add(Resources.getString("Editor.StartupGlobalKeyCommand.global_hotkey"));
    Collections.addAll(descs, super.getAttributeDescriptions());

    return descs.toArray(new String[0]);
  }

  @Override
  public String[] getAttributeNames() {
    final List<String> names = new ArrayList<>();

    names.add(WHEN_TO_APPLY);
    names.add(HOTKEY_OR_KEY_COMMAND);
    names.add(GLOBAL_HOTKEY);

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
    types.add(SendConfig.class);
    types.add(NamedKeyStroke.class);
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
    else if (GLOBAL_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      globalHotkey = (NamedKeyStroke) value;
    }
    else if (HOTKEY_OR_KEY_COMMAND.equals(key)) {
      if (value instanceof String) {
        hotkeyOrKeyCommand = (String)value;
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
    else if (GLOBAL_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(globalHotkey);
    }
    else if (HOTKEY_OR_KEY_COMMAND.equals(key)) {
      return hotkeyOrKeyCommand;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  /**
   * Listen for player side changes.
   */
  @Override
  public void sideChanged(String oldSide, String newSide) {
    sideChangedSummary = newSide;
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
    else if (APPLY_START_OF_GAME_ONLY.equals(whenToApply) || APPLY_START_GAME_OR_SIDE_CHANGE.equals(whenToApply)) {
      if (hasAppliedThisGame) {
        return false;
      }
    }
    else if (APPLY_SIDE_CHANGE.equals(whenToApply)) {
      if (StringUtils.isEmpty(sideChangedSummary)) {
        return false;
      }
      else {
        sideChangedSummary = null;
        apply();
        return true;
      }
    }
    hasEverApplied = true;     // This one will be false again next time anything calls GameState.setup(true)
    hasAppliedThisGame = true; // This one will be remembered as part of the game state (i.e. even after loading a game)
    apply();
    return true;
  }

  /**
   * Apply the command, but only if it is eligible to be applied on Player Join / Change
   * @return true if command was applied
   */
  public boolean applyPlayerChange() {
    if (!APPLY_START_GAME_OR_SIDE_CHANGE.equals(whenToApply) && !APPLY_SIDE_CHANGE.equals(whenToApply)) {
      return false;
    }
    if (APPLY_SIDE_CHANGE.equals(whenToApply)) {
      sideChangedSummary = null;
    }
    else {
      hasEverApplied = true;
      hasAppliedThisGame = true;
    }
    apply();
    return true;
  }

  @Override
  public void apply() {
    if (SEND_KEY_COMMAND.equals(hotkeyOrKeyCommand)) {
      super.apply();
    }
    else {
      if ((globalHotkey != null) && !globalHotkey.isNull()) {
        final GameModule gm = GameModule.getGameModule();
        final boolean loggingPausedByMe = gm.pauseLogging();
        GameModule.getGameModule().fireKeyStroke(globalHotkey);
        if (loggingPausedByMe) {
          gm.resumeLogging();
        }
      }
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    final List<NamedKeyStroke> l = new ArrayList<>(super.getNamedKeyStrokeList());
    l.add(NamedHotKeyConfigurer.decode(getAttributeValueString(GLOBAL_HOTKEY)));
    return l;
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
   * @return An {@link UpdateStartupGlobalKeyCommand}
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
    final StartupGlobalKeyCommand sgkc;

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
