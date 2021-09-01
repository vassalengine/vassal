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

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Logger;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.JOptionPane;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

/**
 * Maintains a list of players involved in the current game
 */
public class PlayerRoster extends AbstractToolbarItem implements CommandEncoder, GameComponent, GameSetupStep {
  public static final String BUTTON_ICON = "buttonIcon"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$
  public static final String TOOL_TIP = "buttonToolTip"; //$NON-NLS-1$
  public static final String BUTTON_KEYSTROKE = "buttonKeyStroke"; //$NON-NLS-1$

  public static final String SIDES = "sides"; //$NON-NLS-1$
  public static final String COMMAND_PREFIX = "PLAYER\t"; //$NON-NLS-1$
  public static final String OBSERVER = "<observer>"; //$NON-NLS-1$

  public static final String SOLITAIRE = "Solitaire"; // Various common names for sides that have access to all pieces (and chess clocks) // NON-NLS
  public static final String REFEREE   = "Referee";   // NON-NLS
  public static final String SOLO      = "Solo";      // NON-NLS
  public static final String MODERATOR = "Moderator"; // NON-NLS

  protected List<PlayerInfo> players = new ArrayList<>();
  protected List<String> sides = new ArrayList<>();
  protected String[] untranslatedSides;

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton retireButton;

  protected List<SideChangeListener> sideChangeListeners = new ArrayList<>();

  protected String translatedObserver;

  private boolean pickedSide = false;

  public PlayerRoster() {
    setButtonTextKey(BUTTON_TEXT);
    setTooltipKey(TOOL_TIP);
    setIconKey(BUTTON_ICON);
    setHotKeyKey(BUTTON_KEYSTROKE);

    setLaunchButton(makeLaunchButton(
      Resources.getString("PlayerRoster.allow_another"),
      Resources.getString("PlayerRoster.retire"),
      "",
      e -> launch()
    ));
    getLaunchButton().setVisible(false);
    retireButton = getLaunchButton(); // for compatibility

    translatedObserver = Resources.getString("PlayerRoster.observer"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
    super.removeFrom(parent);
    final GameModule gm = GameModule.getGameModule();
    gm.getGameState().removeGameComponent(this);
    gm.removeCommandEncoder(this);
  }

  @Override
  public void remove(Buildable child) {
  }

  @Override
  public void build(Element e) {
    if (e != null) {
      final NamedNodeMap attributes = e.getAttributes();
      for (int i = 0; i < attributes.getLength(); ++i) {
        final Attr att = (Attr) attributes.item(i);

        // Old versions of VASSAL (pre-2.9?) wrote "Retire" as the
        // icon filename for the retireButton, even when no such
        // image existed in the archive. This test blocks irritating
        // errors due to nonexistent "Retire" images, by ignoring
        // the buttonIcon attribute when the value is "Retire" but
        // no such image can be found in the archive.
        if (BUTTON_ICON.equals(att.getName()) && //$NON-NLS-1$
            "Retire".equals(att.getValue())) { //$NON-NLS-1$
          try {
            GameModule.getGameModule()
                      .getDataArchive()
                      .getInputStream(DataArchive.IMAGE_DIR + att.getValue());
          }
          catch (IOException ex) {
            continue;
          }
        }

        getLaunchButton().setAttribute(att.getName(), att.getValue());
        Localization.getInstance()
                    .saveTranslatableAttribute(this, att.getName(),
                                                     att.getValue());
      }

      final NodeList n = e.getElementsByTagName("*"); //$NON-NLS-1$
      sides.clear();
      for (int i = 0; i < n.getLength(); ++i) {
        final Element el = (Element) n.item(i);
        sides.add(Builder.getText(el));
      }
      Localization.getInstance()
                  .saveTranslatableAttribute(this, SIDES, getSidesAsString());
    }
  }

  @Override
  public String getConfigureName() {
    return null;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PlayerRoster.component_type"); //$NON-NLS-1$
  }

  @Override
  public void add(Buildable child) {
  }

  @Override
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  @Override
  public Element getBuildElement(Document doc) {
    final Element el = doc.createElement(getClass().getName());

    String att = super.getAttributeValueString(BUTTON_TEXT);
    if (att != null) {
      el.setAttribute(BUTTON_TEXT, att);
    }

    att = super.getAttributeValueString(BUTTON_ICON);
    if (att != null) {
      el.setAttribute(BUTTON_ICON, att);
    }

    att = super.getAttributeValueString(TOOL_TIP);
    if (att != null) {
      el.setAttribute(TOOL_TIP, att);
    }

    att = super.getAttributeValueString(BUTTON_KEYSTROKE);
    if (att != null) {
      el.setAttribute(BUTTON_KEYSTROKE, att);
    }

    for (final String s : sides) {
      final Element sub = doc.createElement("entry"); //$NON-NLS-1$
      sub.appendChild(doc.createTextNode(s));
      el.appendChild(sub);
    }

    return el;
  }

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
  }

  public void addSideChangeListenerToInstance(SideChangeListener l) {
    sideChangeListeners.add(l);
  }

  public void removeSideChangeListenerFromInstance(SideChangeListener l) {
    sideChangeListeners.remove(l);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.html", "Definition_of_Player_Sides"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void addTo(Buildable b) {
    final GameModule gm = GameModule.getGameModule();
    gm.getGameState().addGameComponent(this);
    gm.getGameState().addGameSetupStep(this);
    gm.addCommandEncoder(this);
    super.addTo(b);
  }

  /**
   * Called when the Launch Button for the player roster is clicked (i.e. the "Retire" or "Change Sides" button)
   */
  protected void launch() {
    final String mySide = getMySide();
    if (mySide == null && allSidesAllocated()) {
      return;
    }

    String newSide;
    newSide = promptForSide();
    if ((newSide == null) || newSide.equals(mySide)) {
      return;
    }

    final GameModule gm = GameModule.getGameModule();

    final PlayerInfo me = new PlayerInfo(
      GameModule.getActiveUserId(),
      GlobalOptions.getInstance().getPlayerId(),
      newSide
    );

    Command c = new Chatter.DisplayText(gm.getChatter(), Resources.getString(GlobalOptions.getInstance().chatterHTMLSupport() ? "PlayerRoster.changed_sides_2" : "PlayerRoster.changed_sides", me.playerName, mySide, newSide));
    c.execute();

    remove(GameModule.getActiveUserId());

    final Add a = new Add(this, me.playerId, me.playerName, me.side);
    a.execute();

    c = c.append(a);
    gm.getServer().sendToOthers(c);

    newSide = getMySide();
    fireSideChange(mySide, newSide);
  }

  protected void fireSideChange(String oldSide, String newSide) {
    for (final SideChangeListener l : sideChangeListeners) {
      l.sideChanged(oldSide, newSide);
    }
  }

  public static boolean isActive() {
    return GameModule.getGameModule().getPlayerRoster() != null;
  }

  /**
   * @deprecated use {@link GameModule#getPlayerRoster()}
   */
  @Deprecated(since = "2021-08-06", forRemoval = true)
  protected static PlayerRoster getInstance() {
    return GameModule.getGameModule().getPlayerRoster();
  }

  public static String getMySide() {
    return getMySide(false);
  }

  public static String getMyLocalizedSide() {
    return getMySide(true);
  }

  /**
   * @return List of currently matchable passwords, including "defaults" of various types.
   */
  protected static List<String> getCurrentPasswords() {
    return List.of(
      GameModule.getUserId(),
      Resources.getString("Prefs.password_prompt", System.getProperty("user.name")),
      ""
    );
  }

  protected static String getMySide(boolean localized) {
    final PlayerRoster r = GameModule.getGameModule().getPlayerRoster();
    if (r != null) {
      for (final PlayerInfo pi : r.getPlayers()) {
        if (pi.playerId.equals(GameModule.getActiveUserId())) {
          return localized ? pi.getLocalizedSide() : pi.getSide();
        }
      }
    }
    return null;
  }

  public PlayerInfo[] getPlayers() {
    return players.toArray(new PlayerInfo[0]);
  }

  public List<String> getSides() {
    return new ArrayList<>(sides);
  }

  /**
   * Adds a player to the list of active players occupying sides
   * @param playerId player unique id (password)
   * @param playerName player name
   * @param side player side
   */
  public void add(String playerId, String playerName, String side) {
    final PlayerInfo e = new PlayerInfo(playerId, playerName, side);
    if (players.contains(e)) {
      players.set(players.indexOf(e), e);
    }
    else {
      players.add(e);
    }

    if (GameModule.getGameModule().isMultiPlayer()) {
      final Logger log = GameModule.getGameModule().getLogger();
      if (log instanceof BasicLogger) {
        ((BasicLogger)log).setMultiPlayer(true);
      }
    }
  }

  /**
   * Remove a player from the list of active players occupying sides
   * @param playerId player unique id (password)
   */
  public void remove(String playerId) {
    final PlayerInfo e = new PlayerInfo(playerId, null, null);
    players.remove(e);
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(COMMAND_PREFIX)) {
      return null;
    }

    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
    st.nextToken();
    return new Add(this, st.nextToken(), st.nextToken(), st.nextToken());
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof Add)) {
      return null;
    }

    final Add a = (Add) c;
    final SequenceEncoder se = new SequenceEncoder('\t');
    se.append(a.id)
      .append(a.name)
      .append(a.side);
    return COMMAND_PREFIX + se.getValue();
  }

  @Override
  public Command getRestoreCommand() {
    Command c = null;
    for (final PlayerInfo entry : players) {
      final Command sub = new Add(this, entry.playerId, entry.playerName, entry.side);
      c = c == null ? sub : c.append(sub);
    }
    return c;
  }

  @Override
  public void setup(boolean gameStarting) {
    if (gameStarting) {
      claimOccupiedSide();
      if (GameModule.getGameModule().isMultiPlayer()) {
        final Logger log = GameModule.getGameModule().getLogger();
        if (log instanceof BasicLogger) {
          ((BasicLogger)log).setMultiPlayer(true);
        }
      }
    }
    else {
      GameModule.setTempUserId(null);
      players.clear();
    }
    getLaunchButton().setVisible(gameStarting && getMySide() != null);
    pickedSide = false;
  }

  /**
   * finish() step for Wizard
   */
  @Override
  public void finish() {
    final String newSide = untranslateSide(sideConfig.getValueString());
    if (newSide != null) {
      final Add a = new Add(this, GameModule.getActiveUserId(), GlobalOptions.getInstance().getPlayerId(), newSide);
      a.execute();
      GameModule.getGameModule().getServer().sendToOthers(a);
    }
    getLaunchButton().setVisible(getMySide() != null);
    pickedSide = true;
  }

  @Override
  public Component getControls() {
    final ArrayList<String> availableSides = new ArrayList<>(sides);
    final ArrayList<String> alreadyTaken = new ArrayList<>();

    for (final PlayerInfo p : players) {
      alreadyTaken.add(p.side);
    }

    availableSides.removeAll(alreadyTaken);
    availableSides.add(0, translatedObserver);
    sideConfig = new StringEnumConfigurer(null,
      Resources.getString("PlayerRoster.join_game_as"), //$NON-NLS-1$
      availableSides.toArray(new String[0]));
    sideConfig.setValue(translatedObserver);
    return sideConfig.getControls();
  }

  /**
   * @return step title for Wizard's GameSetupStep
   */
  @Override
  public String getStepTitle() {
    return Resources.getString("PlayerRoster.choose_side"); //$NON-NLS-1$
  }

  /**
   * Implement GameSetupStep for Wizard
   * @return true if Wizard GameSetupStep is finished
   */
  @Override
  public boolean isFinished() {
    if (pickedSide) {
      return true;
    }

    // Step is always finished if all sides are allocated
    if (allSidesAllocated()) {
      return true;
    }

    claimOccupiedSide();

    // If we are already recorded as a player (i.e. in Saved Game), then
    // the step is only finished if we are not the Observer.
    final PlayerInfo newPlayerInfo = new PlayerInfo(
      GameModule.getActiveUserId(),
      GlobalOptions.getInstance().getPlayerId(), null
    );

    final int i = players.indexOf(newPlayerInfo);
    if (i != -1) {
      return !OBSERVER.equals(players.get(i).getSide());
    }

    // Step is not finished
    return false;
  }

  /**
   *
   * @return true if all sides have been claimed by a player
   */
  protected boolean allSidesAllocated() {
    int allocatedSideCount = 0;
    for (final PlayerInfo p : players) {
      if (!OBSERVER.equals(p.getSide())) {
        ++allocatedSideCount;
      }
    }
    return sides.size() == allocatedSideCount;
  }

  /**
   * @param side Name of a side to see if it's a "solo side"
   * @return True if the side is "Solitaire", "Solo", "Moderator", or "Referee"
   */
  public static boolean isSoloSide(String side) {
    return Resources.getString("PlayerRoster.solitaire").equals(side) ||
           Resources.getString("PlayerRoster.solo").equals(side) ||
           Resources.getString("PlayerRoster.moderator").equals(side) ||
           Resources.getString("PlayerRoster.referee").equals(side);
  }

  /**
   * @return True if this is currently a multiPlayer game (either connected to a server, or more than one player side allocated)
   */
  public boolean isMultiPlayer() {
    // NB. Intentionally not excluding observers.
    return players.size() > 1;
  }

  /**
   * Claims an existing slot for the current player, and sets our temporary user id appropriately
   * @param index The index of the slot to be claimed in the roster
   */
  protected void claimSlot(int index) {
    final PlayerInfo[] roster = GameModule.getGameModule().getPlayerRoster().getPlayers();
    final PlayerInfo slot = roster[index];
    slot.playerName = GlobalOptions.getInstance().getPlayerId();
    GameModule.setTempUserId(slot.playerId);
  }

  /**
   * Claims the appropriate occupied side, if any, for the current player. If more than one is available, prompts.
   */
  protected void claimOccupiedSide() {
    final List<Integer> indices = new ArrayList<>();
    final List<String> availableNames = new ArrayList<>();
    final PlayerRoster r = GameModule.getGameModule().getPlayerRoster();
    final List<String> pwdsToMatch = getCurrentPasswords();
    final List<PlayerInfo> allowedSides = new ArrayList<>();

    if (r != null) {
      int index = 0;
      for (final PlayerInfo pi : r.getPlayers()) {
        if (pwdsToMatch.contains(pi.playerId)) {
          allowedSides.add(pi);
          indices.add(index);
        }
        index++;
      }
    }

    GameModule.setTempUserId(null);

    if (allowedSides.isEmpty()) {
      return;
    }
    else if (allowedSides.size() == 1) {
      claimSlot(indices.get(0));
      return;
    }

    for (final PlayerInfo p : allowedSides) {
      final String s;
      if (p.playerId.equals(GameModule.getUserId())) {
        s = Resources.getString("PlayerRoster.current_password");
      }
      else if (p.playerId.isEmpty()) {
        s = Resources.getString("PlayerRoster.empty_password");
      }
      else {
        // must be the "UserName's Password" version
        s = Resources.getString("PlayerRoster.quoted_password", p.playerId);
      }

      availableNames.add(Resources.getString("PlayerRoster.occupied_side", p.getLocalizedSide(), s));
    }

    final GameModule g = GameModule.getGameModule();
    final String choice = (String) JOptionPane.showInputDialog(
      g.getPlayerWindow(),
      Resources.getString("PlayerRoster.pick_an_occupied_side"),
      Resources.getString("PlayerRoster.choose_side"),
      JOptionPane.QUESTION_MESSAGE,
      null,
      availableNames.toArray(new String[0]),
      availableNames.get(0)
    );

    final int index = availableNames.indexOf(choice);
    if (index > 0) {
      claimSlot(indices.get(index));
    }
  }

  protected String promptForSide() {
    final ArrayList<String> availableSides = new ArrayList<>(sides);
    final ArrayList<String> alreadyTaken = new ArrayList<>();

    for (final PlayerInfo p : players) {
      alreadyTaken.add(p.side);
    }

    availableSides.removeAll(alreadyTaken);

    // If a "real" player side is available, we want to offer "the next one" as the default, rather than observer.
    // Thus hotseat players can easily cycle through the player positions as they will appear successively as the default.
    // Common names for Solitaire players (Solitaire, Solo, Referee) do not count as "real" player sides, and will be skipped.
    // If we have no "next" side available to offer, we stay with the observer side as our default offering.
    boolean found = false;       // If we find a usable side
    final String mySide = getMySide(); // Get our own side, so we can find the "next" one
    final int myidx = (mySide != null) ? sides.indexOf(mySide) : -1; // See if we have a current non-observe side.
    int i = (myidx >= 0) ? ((myidx + 1) % sides.size()) : 0;   // If we do, start looking in the "next" slot, otherwise start at beginning.
    for (int tries = 0; i != myidx && tries < sides.size(); i = (i + 1) % sides.size(), tries++) { // Wrap-around search of sides
      final String s = sides.get(i);
      if (!alreadyTaken.contains(s) && !isSoloSide(s)) {
        found = true; // Found an available slot that's not our current one and not a "solo" slot.
        break;
      }
    }

    final String nextChoice = found ? sides.get(i) : translatedObserver; // This will be our defaulted choice for the dropdown.

    availableSides.add(0, translatedObserver);

    final GameModule g = GameModule.getGameModule();
    String newSide = (String) JOptionPane.showInputDialog(
      g.getPlayerWindow(),
      Resources.getString("PlayerRoster.switch_sides", getMyLocalizedSide()), //$NON-NLS-1$
      Resources.getString("PlayerRoster.choose_side"), //$NON-NLS-1$
      JOptionPane.QUESTION_MESSAGE,
      null,
      availableSides.toArray(new String[0]),
      nextChoice // Offer calculated most likely "next side" as the default
    );

    // sides must always be stored internally in English.
    if (translatedObserver.equals(newSide)) {
      newSide = OBSERVER;
    }
    else {
      newSide = untranslateSide(newSide);
    }
    return newSide;
  }

  public static class PlayerInfo {
    public String playerId;
    public String playerName;
    private final String side;

    public PlayerInfo(String id, String name, String side) {
      playerId = Objects.requireNonNull(id);
      playerName = name;
      this.side = side;
    }

    @Override
    public boolean equals(Object o) {
      if (o instanceof PlayerInfo && playerId != null) {
        return playerId.equals(((PlayerInfo) o).playerId);
      }
      else {
        return false;
      }
    }

    @Override
    public int hashCode() {
      return Objects.hash(playerId);
    }

    public String getSide() {
      return side;
    }

    public String getLocalizedSide() {
      return GameModule.getGameModule().getPlayerRoster().translateSide(side);
    }
  }

  public static class Add extends Command {
    private final PlayerRoster roster;
    private final String id;
    private final String name;
    private final String side;

    public Add(PlayerRoster r, String playerId,
               String playerName, String side) {
      roster = r;
      id = playerId;
      name = playerName;
      this.side = side;
    }

    @Override
    protected void executeCommand() {
      roster.add(id, name, side);
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  /** Call-back interface for when a player changes sides during a game */
  @FunctionalInterface
  public interface SideChangeListener {
    void sideChanged(String oldSide, String newSide);
  }

  protected StringEnumConfigurer sideConfig;

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      SIDES,
      BUTTON_TEXT,
      TOOL_TIP,
      BUTTON_ICON,
      BUTTON_KEYSTROKE,
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String[].class,
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class,
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (SIDES.equals(key)) {
      return getSidesAsString();
    }
    return super.getAttributeValueString(key);
  }

  /*
   * Only ever called from Language.translate()
   */
  @Override
  public void setAttribute(String key, Object value) {
    if (SIDES.equals(key)) {
      untranslatedSides = sides.toArray(new String[0]);
      final String[] s = (String[]) value;
      sides = new ArrayList<>(s.length);
      Collections.addAll(sides, s);
    }
    else {
      super.setAttribute(key, value);
    }
  }

  protected String getSidesAsString() {
    final String[] s = sides.toArray(new String[0]);
    return StringArrayConfigurer.arrayToString(s);
  }

  public String untranslateSide(String side) {
    if (translatedObserver.equals(side)) {
      return OBSERVER;
    }
    if (untranslatedSides != null) {
      for (int i = 0; i < sides.size(); i++) {
        if (sides.get(i).equals(side)) {
          return untranslatedSides[i];
        }
      }
    }
    return side;
  }

  public String translateSide(String side) {
    if (OBSERVER.equals(side)) {
      return translatedObserver;
    }
    if (untranslatedSides != null) {
      for (int i = 0; i < untranslatedSides.length; i++) {
        if (untranslatedSides[i].equals(side)) {
          return sides.get(i);
        }
      }
    }
    return side;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.PlayerRoster.sides_label"),
      Resources.getString("Editor.PlayerRoster.retire_button_text"),
      Resources.getString("Editor.PlayerRoster.retire_button_tooltip"),
      Resources.getString("Editor.PlayerRoster.retire_button_icon"),
      Resources.getString("Editor.PlayerRoster.retire_button_keystroke")
    };
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return sides;
  }
}
