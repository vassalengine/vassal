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
import VASSAL.configure.ComponentDescription;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.configure.password.ToggleablePasswordConfigurer;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.i18n.Translation;
import VASSAL.tools.DataArchive;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.FlowLabel;
import net.miginfocom.swing.MigLayout;
import org.netbeans.spi.wizard.WizardController;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Maintains a list of players involved in the current game
 */
public class PlayerRoster extends AbstractToolbarItem implements CommandEncoder, GameComponent, GameSetupStep, ComponentDescription {
  public static final String DESCRIPTION = "description"; //NON-NLS

  public static final String BUTTON_ICON = "buttonIcon"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$
  public static final String TOOL_TIP = "buttonToolTip"; //$NON-NLS-1$
  public static final String BUTTON_KEYSTROKE = "buttonKeyStroke"; //$NON-NLS-1$

  public static final String SIDES = "sides"; //$NON-NLS-1$
  public static final String COMMAND_PREFIX = "PLAYER\t"; //$NON-NLS-1$
  public static final String REMOVE_PREFIX = "PYREMOVE\t"; //NON-NLS
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

  protected String description;

  /** Controls how the pick side drop-down interacts with the Startup Wizard. See below for full details */
  protected boolean freshStart = true;

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

    getLaunchButton().setEnabled(false); // not usable without a game
    retireButton = getLaunchButton(); // for compatibility

    setShowDisabledOptions(false); //AbstractToolbarItem

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
    validator = new SideTranslationValidator();
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

    String newSide = promptForSide();
    if ((newSide == null) || newSide.equals(mySide)) {
      return;
    }

    final GameModule gm = GameModule.getGameModule();
    // Avoid bug that allowed gaining access to a hidden/locked side
    if (GameModule.getGameModule().getGameState().isLoadingInBackground()) {
      return;
    }

    final PlayerInfo me = new PlayerInfo(
      GameModule.getActiveUserId(),
      GlobalOptions.getInstance().getPlayerId(),
      newSide
    );

    Command c = new Chatter.DisplayText(gm.getChatter(), Resources.getString(GlobalOptions.getInstance().chatterHTMLSupport() ? "PlayerRoster.changed_sides_2" : "PlayerRoster.changed_sides", GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME), translateSide(mySide), translateSide(newSide)));
    c.execute();

    final Remove r = new Remove(this, GameModule.getActiveUserId());
    r.execute();
    c = c.append(r);

    GameModule.setTempUserId(null); // If we were using a temp user id, stop using it now
    me.playerId = GameModule.getActiveUserId();

    final Add a = new Add(this, me.playerId, me.playerName, me.side);
    a.execute();

    c = c.append(a);
    gm.sendAndLog(c);

    newSide = getMySide();
    fireSideChange(mySide, newSide);

    GameModule.getGameModule().getGameState().doStartupGlobalKeyCommands(true);
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
  @Deprecated(since = "2021-12-01", forRemoval = true)
  protected static PlayerRoster getInstance() {
    return GameModule.getGameModule().getPlayerRoster();
  }

  /** Return my Untranslated side */
  public static String getMySide() {
    return getMySide(false);
  }

  /** Return my Translated Side */
  public static String getMyLocalizedSide() {
    return getMySide(true);
  }

  /**
   * @return List of currently matchable passwords, including "defaults" of various types.
   */
  protected static List<String> getCurrentPasswords() {
    return List.of(
      GameModule.getUserId(),
      Resources.getString("Prefs.password_prompt", System.getProperty("user.name"))
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

  public List<String> getUntranslatedSideList() {
    return Arrays.asList(getUntranslatedSides());
  }

  public String[] getUntranslatedSides() {
    // If the module is loaded in the editor, the module translation step does not run, so the untranslated side
    // list will not have been set
    if (untranslatedSides == null) {
      untranslatedSides = sides.toArray(new String[0]);
    }
    return untranslatedSides;
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
    if (command.startsWith(COMMAND_PREFIX)) {
      final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      st.nextToken();
      return new Add(this, st.nextToken(), st.nextToken(), st.nextToken());
    }

    if (command.startsWith(REMOVE_PREFIX)) {
      final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      st.nextToken();
      return new Remove(this, st.nextToken(""));
    }
    return null;
  }

  @Override
  public String encode(Command c) {
    if (c instanceof Add) {
      final Add a = (Add) c;
      final SequenceEncoder se = new SequenceEncoder('\t');
      se.append(a.id)
        .append(a.name)
        .append(a.side);
      return COMMAND_PREFIX + se.getValue();
    }
    else if (c instanceof Remove) {
      final Remove a = (Remove) c;
      final SequenceEncoder se = new SequenceEncoder('\t');
      se.append(a.id);
      return REMOVE_PREFIX + se.getValue();
    }
    return null;
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
    super.setup(gameStarting);
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
    getLaunchButton().setEnabled(gameStarting && getMySide() != null);
    pickedSide = false;
  }

  /**
   * finish() step for Wizard
   */
  @Override
  public void finish() {
    final GameModule gm = GameModule.getGameModule();
    // In case we set a new password at this step, update the prefs configurer, and write module preferences.
    gm.getPasswordConfigurer().setValue(GameModule.getUserId());
    try {
      gm.getPrefs().save();
    }
    catch (IOException e) {
      gm.warn(Resources.getString("PlayerRoster.failed_pref_write", e.getLocalizedMessage()));
    }

   // Drop into standard routine, starting with checking that the side is still available (race condition mitigation)
    // returns untranslated side
    final String newSide = promptForSide(sideConfig.getValueString());

    // null is a cancel op - player will not connect to the game
    if (newSide != null) {
      if (gm.isMultiplayerConnected()) {
        final Command c = new Chatter.DisplayText(gm.getChatter(), Resources.getString(GlobalOptions.getInstance().chatterHTMLSupport() ? "PlayerRoster.joined_side_2" : "PlayerRoster.joined_side", gm.getPrefs().getValue(GameModule.REAL_NAME), translateSide(newSide)));
        c.execute();
        gm.sendAndLog(c);

      }

      final Add a = new Add(this, GameModule.getActiveUserId(), GlobalOptions.getInstance().getPlayerId(), newSide);
      a.execute();
      gm.sendAndLog(a);

      pickedSide = true;
    }
    freshStart = true;
  }

  /***
   * Called when a Game Start wizard has been cancelled.
   * Record that the next call from the Wizard will be from a completelty new Wizard
   */
  public void reset() {
    freshStart = true;
  }

  @Override
  public Component getControls() {
    final List<String> availableSides = new ArrayList<>(sides);
    final List<String> alreadyTaken = new ArrayList<>();

    for (final PlayerInfo p : players) {
      alreadyTaken.add(p.getLocalizedSide());
    }

    availableSides.removeAll(alreadyTaken);
    availableSides.add(0, translatedObserver);
    //
    // *** WARNING*** Major Fudge Alert
    // The following code is completely ridiculous, but necessary as we have no access to the inner workings of
    // the Wizard.
    //
    // PlayerRoster.getControls() is called each time a PDS is selected, either from a PDS menu option
    // OR each time a PDS is selected from the wizard drop-down menu.
    //
    // There is a bug in the Wizard (or we are not using it properly) so that if the 'Prev' button is pressed
    // to return from the side selection page to the Scenario selection page, then a new Scenario is selected,
    // then 'Next' is pressed, then the previously generated Side selection page is used EVEN THOUGH the wizard
    // has just called here to create a new set of controls. This new set of controls is added to a page that
    // is never shown, the user is shown the old out-of-date drop-down for the previous scenario selected
    //
    // The work-around is to
    // a) reuse sideConfig rather than regenerate it, so the existing Wizard page sees the changes
    // and
    // b) return a set of dummy controls that get attached to the page that is never seen. If we return the
    //    sideConfig controls again, they get removed from the existing page, since a Swing component can only
    //    be attached to one parent.
    //
    // If the Wizard fully completes (PlayerRoster.finish() is called) OR is cancelled (PlayerRoster.reset() is called)
    // then we do want to return the real sideConfig controls for the next full Wizard. The freshStart variable tracks this.
    //
    final Component controls;
    if (sideConfig == null || freshStart) {
      sideConfig = new StringEnumConfigurer(null,
        Resources.getString("PlayerRoster.join_game_as"), //$NON-NLS-1$
        availableSides.toArray(new String[0]));
      controls = sideConfig.getControls();
      freshStart = false;
    }
    else {
      sideConfig.setValidValues(availableSides.toArray(new String[0]));
      controls = new JPanel();
    }
    sideConfig.setValue(translatedObserver);

    // If they have a non-blank password already, then we just return the side-picking controls
    final String pwd = (String)GameModule.getGameModule().getPrefs().getValue(GameModule.SECRET_NAME);
    if (!forcePwd || ((pwd != null) && !pwd.isEmpty())) {
      return controls;
    }

    // Or, if they haven't set a password yet, we plead with them to set one.
    final JPanel panel = new JPanel(new MigLayout("ins 0", "[]", "[]para[]rel[]")); //NON-NLS
    panel.add(sideConfig.getControls(), "wrap"); //NON-NLS

    final FlowLabel message = new FlowLabel(Resources.getString("PlayerRoster.need_non_blank_password"));
    panel.add(message, "wrap"); //NON-NLS

    final JLabel label = new JLabel(Resources.getString("PlayerRoster.please_set_your_password"));
    panel.add(label, "split 2"); //NON-NLS

    tpc = new ToggleablePasswordConfigurer(GameModule.SECRET_NAME, "", "");
    tpc.addPropertyChangeListener(evt -> GameModule.setUserId((String) evt.getNewValue()));

    label.setLabelFor(tpc.getControls());
    panel.add(tpc.getControls());

    return panel;
  }

  /**
   * @return step title for Wizard's GameSetupStep
   */
  @Override
  public String getStepTitle() {
    return Resources.getString("PlayerRoster.choose_side"); //$NON-NLS-1$
  }

  private ToggleablePasswordConfigurer tpc;
  private WizardController wc;
  private boolean forcePwd = false;

  public void validatePassword() {
    if (pickedSide || (wc == null) || !forcePwd) {
      return;
    }

    final String pwd = (String)GameModule.getGameModule().getPrefs().getValue(GameModule.SECRET_NAME);
    if ((pwd != null) && !pwd.isEmpty()) {
      wc.setProblem(null);
    }
    else {
      wc.setProblem(Resources.getString("PlayerRoster.please_set_non_blank_password"));
    }
  }

  @Override
  public void setController(WizardController wc) {
    this.wc = wc;

    if (forcePwd && (wc != null) && (tpc != null)) {
      tpc.addPropertyChangeListener(evt -> {
        final String newPwd = (String) evt.getNewValue();
        if (newPwd.isEmpty()) {
          wc.setProblem(Resources.getString("PlayerRoster.please_set_non_blank_password"));
        }
        else {
          wc.setProblem(null);
        }
      });
      SwingUtilities.invokeLater(this::validatePassword);
    }
  }

  public void setForcePwd(boolean forcePwd) {
    this.forcePwd = forcePwd;
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
    // true if step is finished
    return i != -1 && !OBSERVER.equals(players.get(i).getSide());
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
  public static boolean isTranslatedSoloSide(String side) {
    return Resources.getString("PlayerRoster.solitaire").equals(side) ||
           Resources.getString("PlayerRoster.solo").equals(side) ||
           Resources.getString("PlayerRoster.moderator").equals(side) ||
           Resources.getString("PlayerRoster.referee").equals(side);
  }

  public static boolean isSoloSide(String side) {
    return SOLITAIRE.equals(side) ||
      SOLO.equals(side) ||
      MODERATOR.equals(side) ||
      REFEREE.equals(side);
  }

  /**
   * @return True if this is currently a multiPlayer game (either connected to a server, or more than one player side allocated)
   */
  public boolean isMultiPlayer() {
    if (!sides.isEmpty()) {
      // If Sides are defined, do not count Observers towards multi-player total.
      int takenSideCount = 0;
      for (final PlayerInfo p : players) {
        if (sides.contains(p.side)) takenSideCount++;
      }
      return takenSideCount > 1;
    }
    else {
      // No sides, need to check Observer Count
      return players.size() > 1;
    }
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
    boolean blankMatch = false;

    if (r != null) {
      int index = 0;

      // First check for passwords that either (a) precisely match our actual *current* password, or (b) match our non-blank default password
      for (final PlayerInfo pi : r.getPlayers()) {
        if (pwdsToMatch.contains(pi.playerId)) {
          allowedSides.add(pi);
          indices.add(index);
        }
        index++;
      }

      // Only if we don't match *any* of those do we try auto-matching a blank password, and then only for non-observer slots
      if (allowedSides.isEmpty()) {
        index = 0;
        for (final PlayerInfo pi : r.getPlayers()) {
          if ("".equals(pi.playerId) && !OBSERVER.equals(pi.getSide())) {
            allowedSides.add(pi);
            indices.add(index);
          }
          index++;
          blankMatch = true;
        }
      }
    }

    GameModule.setTempUserId(null);

    if (allowedSides.isEmpty()) {
      return;
    }
    else if ((allowedSides.size() == 1) && (!blankMatch || allSidesAllocated())) {
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

    if (blankMatch) {
      availableNames.add(Resources.getString("PlayerRoster.none_of_the_above")); //NON-NLS
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

    final int pick = availableNames.indexOf(choice);
    if ((pick >= 0) && (pick < indices.size())) {
      claimSlot(indices.get(pick));
    }
  }

  public List<String> getAvailableSides() {
    final List<String> availableSides = new ArrayList<>(sides);
    final List<String> alreadyTaken = new ArrayList<>();

    for (final PlayerInfo p : players) {
      alreadyTaken.add(p.getLocalizedSide());
    }

    availableSides.removeAll(alreadyTaken);
    return availableSides;
  }

  protected String promptForSide() {
    return promptForSide("");
  }

  protected String promptForSide(String newSide) {
    final boolean alreadyConnected;
    if (newSide != null && newSide.isEmpty()) {
      alreadyConnected = true;
    }
    else {
      if (newSide == null || translatedObserver.equals(newSide)) { // Observer checked and returned translated here
        return OBSERVER;
      }
      else {
        alreadyConnected = false;
      }
    }

    // availableSides and alreadyTaken are translated side names
    final List<String> availableSides = new ArrayList<>(getSides());
    final List<String> alreadyTaken = new ArrayList<>();
    final GameModule g = GameModule.getGameModule();

    while (newSide != null) { // Loops until a valid side is found or op is canceled (repeats side check to minimise race condition window)
      // Refresh from current game state
      for (final PlayerInfo p : players) {
        alreadyTaken.add(p.getLocalizedSide());
      }

      /*
       The while loop ensures that the selected side is re-checked here and only returned if the side is still available.
       This prevents players switching to the same side if they enter the switch-side dialogue (below) at the same time,
       narrowing the race condition window to network latency.
      */
      if (!newSide.isEmpty() && !alreadyTaken.contains(newSide)) {
        // side is returned in English for sharing in the game.
        newSide = untranslateSide(newSide);
        break;
      }
      else {
        // Set up for another try...
        availableSides.clear();
        availableSides.addAll(sides);
      }

      availableSides.removeAll(alreadyTaken);
      String nextChoice = translatedObserver; // default for dropdown

      // When player is already connected, offer a hot-seat...
      // If a "real" player side is available, we want to offer "the next one" as the default, rather than observer.
      // Thus, hotseat players can easily cycle through the player positions as they will appear successively as the default.
      // Common names for Solitaire players (Solitaire, Solo, Referee) do not count as "real" player sides, and will be skipped.
      // If we have no "next" side available to offer, we stay with the observer side as our default offering.
      if (alreadyConnected) {
        final String mySide = getMyLocalizedSide(); // Get our own side, so we can find the "next" one
        final int myidx = (mySide != null) ? sides.indexOf(mySide) : -1; // See if we have a current non-observe side.
        int i = (myidx >= 0) ? ((myidx + 1) % sides.size()) : 0;   // If we do, start looking in the "next" slot, otherwise start at beginning.
        for (int tries = 0; i != myidx && tries < sides.size(); i = (i + 1) % sides.size(), tries++) { // Wrap-around search of sides
          final String s = sides.get(i);
          if (!alreadyTaken.contains(s) && !isSoloSide(untranslateSide(s))) {
            nextChoice = sides.get(i); // Found an available slot that's not our current one and not a "solo" slot.
            break;
          }
        }
      }

      availableSides.add(0, translatedObserver);

      newSide = (String) JOptionPane.showInputDialog(
              g.getPlayerWindow(),
              newSide.isEmpty() ? Resources.getString("PlayerRoster.switch_sides", getMyLocalizedSide()) : Resources.getString("PlayerRoster.switch_sides2", newSide, getMyLocalizedSide()), //$NON-NLS-1$
              Resources.getString("PlayerRoster.choose_side"), //$NON-NLS-1$
              JOptionPane.QUESTION_MESSAGE,
              null,
              availableSides.toArray(new String[0]),
              nextChoice // Offer calculated most likely "next side" as the default
      );

      // side must be returned in English
      if (translatedObserver.equals(newSide)) { // Observer returns here, other returns are checked once more.
        return OBSERVER;
      }
      else {
        alreadyTaken.clear(); // prepare to loop again for exit check
      }
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

    public boolean isObserver() {
      return OBSERVER.equals(side);
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

  public static class Remove extends Command {
    private final PlayerRoster roster;
    private final String id;

    public Remove(PlayerRoster r, String playerId) {
      roster = r;
      id = playerId;
    }

    @Override
    protected void executeCommand() {
      roster.remove(id);
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
      DESCRIPTION,
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
      String.class,
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
    else if (DESCRIPTION.equals(key)) {
      return description;
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
      // When the module is being translated, the translated sides are passed in as a comma delimited string, not as an array
      if (value instanceof String) {
        setSidesFromString((String) value);
      }
      else {
        final String[] s = (String[]) value;
        sides = new ArrayList<>(s.length);
        Collections.addAll(sides, s);
      }
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String)value;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  protected String getSidesAsString() {
    final String[] s = sides.toArray(new String[0]);
    return StringArrayConfigurer.arrayToString(s);
  }

  /**
   * Set a new set of side names from a translation
   * DO NOT apply if the number of sides does not match the number of currently defined sides as
   * there is no way to tell which translation applies to which existing side
   * @param newSides  Comma delimited string of translated sides
   */
  protected void setSidesFromString(String newSides) {
    final String[] newSideArray = StringArrayConfigurer.stringToArray(newSides);
    if (newSideArray.length == untranslatedSides.length) {
      sides = Arrays.asList(StringArrayConfigurer.stringToArray(newSides));
    }
    else {
      GameModule.getGameModule().warn(Resources.getString("PlayerRoster.side_translation_error", untranslatedSides.length, newSideArray.length));
    }
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
      Resources.getString("Editor.description_label"),
      Resources.getString("Editor.PlayerRoster.sides_label"),
      Resources.getString("Editor.PlayerRoster.retire_button_text"),
      Resources.getString("Editor.PlayerRoster.retire_button_tooltip"),
      Resources.getString("Editor.PlayerRoster.retire_button_icon"),
      Resources.getString("Editor.PlayerRoster.retire_button_keystroke")
    };
  }

  @Override
  public String getDescription() {
    return description;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    final List<String> l = super.getPropertyList();
    l.addAll(sides);
    return l;
  }

  @Override
  public ComponentI18nData getI18nData() {
    final ComponentI18nData c = super.getI18nData();
    c.setAttributeTranslatable(SIDES, true);
    return c;
  }

  @Override
  public boolean isMandatory() {
    return true;
  }

  @Override
  public boolean isUnique() {
    return true;
  }

  /**
   * Validator to check that the correct number of sides exist in any translations
   */
  private static class SideTranslationValidator implements ValidityChecker {

    @Override
    public void validate(Buildable target, ValidationReport report) {
      if (target instanceof PlayerRoster) {
        final PlayerRoster pr = (PlayerRoster) target;
        for (final String language : Localization.getInstance().getTranslationList()) {
          final Translation translation = Localization.getInstance().getTranslation(language);
          final String translatedSides = translation.translate(pr.getI18nData().getPrefix() + SIDES);
          if (translatedSides != null && !translatedSides.isEmpty()) {
            final int translatedSideCount = translatedSides.split(",").length;
            final int untranslatedSideCount = pr.sides.size();
            if (translatedSideCount != untranslatedSideCount) {
              report.addWarning(Resources.getString(
                "Editor.ValidityChecker.side_warning",
                language,
                translatedSideCount,
                ConfigureTree.getConfigureName(target.getClass()),
                untranslatedSideCount));
            }
          }
        }
      }
    }
  }
}
