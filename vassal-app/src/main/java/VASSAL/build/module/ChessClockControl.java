/*
 * Copyright (c) 2020 by Brian Reynolds and Michael Kiefte
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

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import java.util.ArrayList;
import java.util.List;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.chessclockcontrol.ChessClock;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import org.w3c.dom.Element;

/**
 * CHESS CLOCK CONTROLLER for VASSAL.
 *
 * This Module-level component manages a set of one or more {@link ChessClock} clocks. It creates a master "Chess Clocks" button on the
 * Module's toolbar. Clicking the button shows/hides the clocks themselves, when no clocks are running, or stops all clocks if one
 * is running. The Chess Clock Control component also contains the configurable elements that specify how the clocks are to be displayed.
 */
public class ChessClockControl extends AbstractConfigurable
        implements CommandEncoder, GameComponent, UniqueIdManager.Identifyable {
  protected static final UniqueIdManager ID_MGR = new UniqueIdManager("ChessClockControl"); //$NON-NLS-1$

  protected List<ChessClock> chessclocks = new ArrayList<>();  // List of individual chess clocks attached to our control

  protected String showTenthSeconds = STYLE_AUTO;  // Configurable clock display style
  protected String showSeconds = STYLE_AUTO;
  protected String showHours = STYLE_AUTO;
  protected String showDays = STYLE_AUTO;

  protected boolean allowReset = false; // Allow reset

  protected LaunchButton chessClockButton;  // Main chess clock button on toolbar
  protected boolean chessClocksVisible;     // Whether the individual clocks are visible or hidden (as opposed to just main chess clock button)
  protected boolean onlineGame;             // If we've ever detected a live "online" connection during use of the clocks

  protected NamedKeyStrokeListener nextListener;   // Hotkey listeners
  protected NamedKeyStrokeListener showListener;
  protected NamedKeyStrokeListener pauseListener;

  protected boolean instanceIsActive;       // True when this instance has been fully initialized & registered (and not yet shut down)

  protected String id;

  public static final char DELIMITER = '\t';

  public static final String NAME = "name"; //NON-NLS
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$
  public static final String BUTTON_TOOLTIP = "buttonTooltip"; //$NON-NLS-1$

  public static final String PAUSE_HOTKEY = "pauseHotkey"; //NON-NLS
  public static final String NEXT_HOTKEY = "nextHotkey"; //NON-NLS
  public static final String SHOW_HOTKEY = "showHotkey"; //NON-NLS

  public static final String SHOW_TENTHSECONDS = "showTenths"; //NON-NLS
  public static final String SHOW_SECONDS = "showSeconds"; //NON-NLS
  public static final String SHOW_HOURS = "showHours"; //NON-NLS
  public static final String SHOW_DAYS = "showDays"; //NON-NLS

  public static final String ALLOW_RESET = "allowReset"; //NON-NLS

  public static final String STYLE_ALWAYS = "Always"; //NON-NLS (really)
  public static final String STYLE_AUTO   = "Auto"; //NON-NLS (really)
  public static final String STYLE_NEVER  = "Never"; //NON-NLS (really)

  public static final String CHESSMENU_PAUSE = Resources.getString("ChessClock.pause"); //NON-NLS
  public static final String CHESSMENU_SHOW  = Resources.getString("ChessClock.show"); //NON-NLS
  public static final String CHESSMENU_HIDE  = Resources.getString("ChessClock.hide"); //NON-NLS
  public static final String CHESSMENU_NEXT  = Resources.getString("ChessClock.next"); //NON-NLS

  public static final String COMMAND_PREFIX = "CLOCKCONTROL" + DELIMITER; //NON-NLS-1$

  public ChessClockControl() {
    setConfigureName(Resources.getString("Editor.ChessClock.chess_clock_control"));

    chessClocksVisible = false;
    onlineGame         = false;

    final ActionListener al = e -> pressControlButton();

    chessClockButton = new LaunchButton("Chess Clocks", BUTTON_TOOLTIP, BUTTON_TEXT, SHOW_HOTKEY, ICON, al); //$NON-NLS-1$
    chessClockButton.setToolTipText(Resources.getString("Editor.tooltip_text_label"));
    chessClockButton.setAttribute(ICON, "chess_clock.png");
    chessClockButton.addMouseListener(new ChessMouseListener());

    // Set up listeners for hotkeys
    nextListener = new NamedKeyStrokeListener(e -> {
      final Command c = startNextClock();
      if ((c != null) && !c.isNull()) {
        c.execute();
        GameModule.getGameModule().sendAndLog(c);
      }
    });
    GameModule.getGameModule().addKeyStrokeListener(nextListener);

    showListener = new NamedKeyStrokeListener(e -> pressControlButton());
    GameModule.getGameModule().addKeyStrokeListener(showListener);

    pauseListener = new NamedKeyStrokeListener(e -> {
      final Command command = stopAllClocks();
      command.execute();
      GameModule.getGameModule().sendAndLog(command);
    });
    GameModule.getGameModule().addKeyStrokeListener(pauseListener);
  }

  public boolean isInstanceIsActive() {
    return instanceIsActive;
  }

  /**
   * @return Our list of chessclocks
   */
  public List<ChessClock> getChessClocks() {
    return chessclocks;
  }

  /**
   * Builds the clock control's component hierarchy from a given XML element, or a null one is given initializes
   * a brand new default "new clock control" hierarchy.
   * @param e XML element to build from, or null to build the default hierarchy
   */
  @Override
  public void build(Element e) {
    super.build(e);
    if (e != null) {
      return;
    }

    // When creating a brand new Chess Clock Control, start with one chess clock for each "non solo" side in the roster.
    final PlayerRoster r = GameModule.getGameModule().getPlayerRoster();
    int added = 0;
    if (r != null) {
      for (final String s : r.sides) {
        if (!r.isSoloSide(s)) {
          addChild(new ChessClock(s));
          added++;
        }
      }
    }

    if (added == 0) {
      addChild(new ChessClock());
    }
  }


  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
  }

  /**
     * Registers us with the game module, tool bar, command encoder, etc.
     * @param parent - Should be the main GameModule, but in any event that's what we add it to
     */
  @Override
  public void addTo(Buildable parent) {
    final GameModule gameModule = GameModule.getGameModule();
    gameModule.getToolBar().add(getComponent());
    gameModule.addCommandEncoder(this);
    gameModule.getGameState().addGameComponent(this);
    ID_MGR.add(this);

    //BR// We handle adding our chessclocks to the toolbar during initial build, because otherwise their buttons
    //BR// would annoyingly appear before ours, because of the awkward post-order behavior of build.
    for (final ChessClock c : chessclocks) {
      c.addToToolbar();
    }

    instanceIsActive = true;
  }

  /**
   * Unregisters us from everything when shutting down.
   * @param parent - Presumably the main GameModule, but in any event that's what we remove ourselves from.
   */
  @Override
  public void removeFrom(Buildable parent) {
    instanceIsActive = false;

    final GameModule gameModule = GameModule.getGameModule();
    gameModule.getToolBar().remove(getComponent());
    gameModule.removeCommandEncoder(this);
    gameModule.getGameState().removeGameComponent(this);
    ID_MGR.remove(this);
  }

  /**
   * Manages our unique ID
   * @param id - sets our unique id
   */
  @Override
  public void setId(String id) {
    this.id = id;
  }

  /**
   * @return our unique id
   */
  @Override
  public String getId() {
    return id;
  }

  /**
   * @return Whether clocks can be reset manually
   */
  public boolean isAllowReset() {
    return allowReset;
  }

  /**
   * @return true if the group of chess clocks we manage is currently visible
   */
  public boolean getChessClocksVisible() {
    return chessClocksVisible;
  }

  /**
   * @return the JButton for our master chessclocks button
   */
  private JButton getComponent() {
    return chessClockButton;
  }

  /**
   * @return Component type name, which appears in [..] in Editor window
   */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ChessClock.chess_clock_control");
  }

  // XML file attributes - next six methods configure them, and handle setting/getting.

  /**
   * @return List of valid subcomponent class types for this component (in our case, individual Chess Clocks!) which
   * can be added in the Editor.
   */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { ChessClock.class };
  }

  /**
   * @return Key names for our attributes from the buildFile (XML) definition.
   */
  @Override
  public String[] getAttributeNames() {
    return new String[] { NAME, ICON, BUTTON_TEXT, BUTTON_TOOLTIP, SHOW_HOTKEY, NEXT_HOTKEY,
      PAUSE_HOTKEY, SHOW_TENTHSECONDS, SHOW_SECONDS, SHOW_HOURS, SHOW_DAYS, ALLOW_RESET };
  }

  /**
   * @return Descriptions for our buildFile (XML) attributes. These appear when our component is configured in the Editor window.
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.button_icon_label"),
      Resources.getString("Editor.button_text_label"),
      Resources.getString("Editor.tooltip_text_label"),
      Resources.getString("Editor.ChessClock.show_clocks_hotkey"),
      Resources.getString("Editor.ChessClock.start_next_clock_hotkey"),
      Resources.getString("Editor.ChessClock.pause_all_clocks_hotkey"),
      Resources.getString("Editor.ChessClock.show_tenths_of_seconds"),
      Resources.getString("Editor.ChessClock.show_seconds"),
      Resources.getString("Editor.ChessClock.show_hours"),
      Resources.getString("Editor.ChessClock.show_days"),
      Resources.getString("Editor.ChessClock.allow_reset")
    };
  }

  /**
   * @return Class types for configuring each of our buildFile (XML) attributes. Specifies which flavor of configurer to uses.
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] { String.class, IconConfig.class, String.class, String.class,
      NamedKeyStroke.class, NamedKeyStroke.class, NamedKeyStroke.class, TimeStyleConfig.class, TimeStyleConfig.class,
      TimeStyleConfig.class, TimeStyleConfig.class, Boolean.class };
  }

  /**
   * Sets the value of one of this component's XML attributes
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @param value New value for the attribute. Can be either String version or the actual Object.
   */
  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (BUTTON_TOOLTIP.equals(key)) {
      chessClockButton.setToolTipText((String) value);
    }
    else if (NEXT_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      nextListener.setKeyStroke((NamedKeyStroke) value);
    }
    else if (PAUSE_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      pauseListener.setKeyStroke((NamedKeyStroke) value);
    }
    else if (SHOW_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      showListener.setKeyStroke((NamedKeyStroke) value);
    }
    else if (SHOW_TENTHSECONDS.equals(key)) {
      showTenthSeconds = (String) value;
      updateAllClocks();
    }
    else if (SHOW_SECONDS.equals(key)) {
      showSeconds = (String) value;
      updateAllClocks();
    }
    else if (SHOW_HOURS.equals(key)) {
      showHours = (String) value;
      updateAllClocks();
    }
    else if (SHOW_DAYS.equals(key)) {
      showDays = (String) value;
      updateAllClocks();
    }
    else if (ALLOW_RESET.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      allowReset = (Boolean) value;
    }
    else {
      chessClockButton.setAttribute(key, value);
    }
  }

  /**
   * Gets the value of one of this component's XML attributes
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @return String value of the attribute.
   */
  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (BUTTON_TOOLTIP.equals(key)) {
      return chessClockButton.getToolTipText();
    }
    else if (NEXT_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(nextListener.getNamedKeyStroke());
    }
    else if (PAUSE_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(pauseListener.getNamedKeyStroke());
    }
    else if (SHOW_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(showListener.getNamedKeyStroke());
    }
    else if (SHOW_TENTHSECONDS.equals(key)) {
      return showTenthSeconds;
    }
    else if (SHOW_SECONDS.equals(key)) {
      return showSeconds;
    }
    else if (SHOW_HOURS.equals(key)) {
      return showHours;
    }
    else if (SHOW_DAYS.equals(key)) {
      return showDays;
    }
    else if (ALLOW_RESET.equals(key)) {
      return String.valueOf(allowReset);
    }
    else {
      return chessClockButton.getAttributeValueString(key);
    }
  }

  /**
   * @return The help file for this component. Used when user clicks "Help" button while configuring the component in the Editor.
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChessClock.html");  //$NON-NLS-1$
  }

  /**
   * When we need to find a ChessClockControl in a static context
   * @return First ChessClockControl in our module
   */
  public static ChessClockControl getInstance() {
    return GameModule.getGameModule().getAllDescendantComponentsOf(ChessClockControl.class).stream().findFirst().orElse(null);
  }

  /**
   * @return Setting for showing tenths of seconds. (Always, Auto, Never)
   */
  public String getShowTenths() {
    return showTenthSeconds;
  }

  /**
   * @return Setting for showing seconds. (Always, Auto, Never)
   */
  public String getShowSeconds() {
    return showSeconds;
  }

  /**
   * @return Setting for showing hours. (Always, Auto, Never)
   */
  public String getShowHours() {
    return showHours;
  }

  /**
   * @return Setting for showing days. (Always, Auto, Never)
   */
  public String getShowDays() {
    return showDays;
  }

  /**
   * @return the number of chess clocks currently ticking/active
   */
  public int getClocksTicking() {
    int running = 0;
    if (instanceIsActive && (getInstance() != null)) {
      for (final ChessClock c : getInstance().chessclocks) {
        if (c.isTicking()) {
          running++;
        }
      }
    }
    return running;
  }

  /**
   * @return A command to stop all the clocks
   */
  public Command stopAllClocks() {
    Command command = new NullCommand();
    if (instanceIsActive && getInstance() != null) {
      for (final ChessClock c : getInstance().chessclocks) {
        if (!c.isTicking()) {
          continue;
        }
        command = command.append(c.updateState(false));
      }
    }
    return command;
  }

  /**
   * @return a command to start the next clock after stopping the one currently running (if any)
   */
  public Command startNextClock() {
    Command command = new NullCommand();

    if (chessclocks.isEmpty()) { // If we don't have any clocks, can't start one.
      return command;
    }

    ChessClock first = null;
    boolean found = false;
    for (final ChessClock clock : chessclocks) {
      if (first == null) {
        first = clock; // Mark first clock, in case we run off the end and that's the one we need
        // to start.
        if (chessclocks.size() == 1) {
          // If we only HAVE one clock in the set, toggle it on/off
          command = command.append(clock.updateState(!clock.isTicking()));
          return command;
        }
      }

      // If we already found the ticking clock, that means we're now on the
      // "next" clock and should start it.
      if (found) {
        command = command.append(clock.updateState(true));
        return command;
      }

      // Okay, found the clock that is currently ticking.
      if (clock.isTicking()) {
        command = command.append(clock.updateState(false));
        found = true;
      }
    }

    // If we get here, either (a) no clocks were running, (b) the last clock
    // in the list was running, or (c) there aren't any clocks at all. In either
    // of the first two cases, that means we want to start the first clock.
    if (first != null) {
      command = command.append(first.updateState(true));
    }
    return command;
  }

  /**
   * Hide all the clocks on the toolbar
   */
  public void hideClocks() {
    chessClocksVisible = false;
    if (instanceIsActive && (getInstance() != null)) {
      for (final ChessClock c : getInstance().chessclocks) {
        c.hideClock();
      }
    }
  }

  /**
   * Show all the clocks on the toolbar
   */
  public void showClocks() {
    chessClocksVisible = true;
    if (instanceIsActive && (getInstance() != null)) {
      for (final ChessClock c : getInstance().chessclocks) {
        c.showClock();
      }
    }
  }

  /**
   * Update all clocks (when our time-display format changes)
   */
  public void updateAllClocks() {
    if (instanceIsActive && (getInstance() != null)) {
      for (final ChessClock c : getInstance().chessclocks) {
        c.setTimerButton();
      }
    }
  }

  /**
   * Figures out "the right thing to do" when player clicks the main Chess Clock Control button.
   * Does one of the following, in order of priority:
   * (1) If the clocks are hidden, show them
   * (2) If any clock is running, stop them all
   * (3) Hide all the clocks
   */
  public void pressControlButton() {
    if (!chessClocksVisible) {
      showClocks();
    }
    else {
      if (getClocksTicking() > 0) {
        final Command command = stopAllClocks();
        command.execute();
        GameModule.getGameModule().sendAndLog(command);
      }
      else {
        hideClocks();
      }
    }
  }

  /**
   * Tells Chess Clock Control that this is a real online game, meaning we move to stricter verification
   * standards for clocks (completely non-online clocks are really just "sandbox toys")
   * @param online True if this is an online game
   */
  public void setOnline(boolean online) {
    onlineGame = online;
  }

  /**
   * @return True if this has been marked as an online game
   */
  public boolean isOnline() {
    return onlineGame;
  }

  /**
   * Handles starting and ending a game from the point of view of Chess Clocks.
   * @param gameStarting If true, a game is starting. If false, then a game is ending.
   */
  @Override
  public void setup(final boolean gameStarting) {
    if (gameStarting) {
      setOnline(GameModule.getGameModule().getServer().isConnected());
    }
    else {
      stopAllClocks();
    }
  }

  /**
   * Allows individual chess clocks to add themselves to our control scheme
   * @param clock clock to be added
   */
  public void addChessClock(ChessClock clock) {
    chessclocks.add(clock);
  }

  /**
   * Removes a chess clock
   * @param clock clock to be removed
   */
  public void removeChessClock(ChessClock clock) {
    chessclocks.remove(clock);
  }

  /**
   * Autoconfigurer for master chessclock button icon (allows user to choose an icon image)
   */
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((ChessClockControl) c).chessClockButton.getAttributeValueString(ICON));
    }
  }

  /**
   * Autoconfigurer for the days/hours/minutes/seconds/tenths styles
   */
  public static class TimeStyleConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { STYLE_ALWAYS, STYLE_AUTO, STYLE_NEVER };
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.ChessClock.always",
        "Editor.ChessClock.auto",
        "Editor.ChessClock.never" };
    }
  }

  /**
   * Deserializes our command from a string version, if the command belongs to us.
   * @param command Serialized string command
   * @return An {@link UpdateClockControlCommand}
   */
  @Override
  public Command decode(final String command) {
    if (!command.startsWith(COMMAND_PREFIX)) {
      return null;
    }

    final SequenceEncoder.Decoder decoder = new SequenceEncoder.Decoder(command, DELIMITER);
    decoder.nextToken();
    final boolean showing = decoder.nextBoolean(false);
    final boolean online  = decoder.nextBoolean(false);
    return new UpdateClockControlCommand(showing, online);
  }

  /**
   * Serializes our command into a string, if it belongs to us
   * @param c Command to serialize. Only serialized if it's an UpdateClockControlCommand.
   * @return Serialized command, or null if command passed wasn't an UpdateClockControlCommand.
   */
  @Override
  public String encode(final Command c) {
    if (!(c instanceof UpdateClockControlCommand)) {
      return null;
    }
    final UpdateClockControlCommand comm = (UpdateClockControlCommand) c;
    final SequenceEncoder encoder = new SequenceEncoder(DELIMITER);
    encoder.append(comm.showing);
    encoder.append(comm.online);
    return COMMAND_PREFIX + encoder.getValue();
  }

  /**
   * @return Our command for restoring from a saved game (or adding an online player)
   */
  @Override
  public Command getRestoreCommand() {
    return new UpdateClockControlCommand(chessClocksVisible, onlineGame);
  }

  /**
   * Our "command" format for passing information about the master clock control between computers (or to/from save and log files)
   */
  private class UpdateClockControlCommand extends Command {
    private final boolean showing;
    private final boolean online;

    public UpdateClockControlCommand(boolean showing, boolean online) {
      this.showing = showing;
      this.online  = online;
    }

    /**
     * Process a master clock button update.
     * (1) Update visibility status as appropriate
     * (2) Check current online status.
     */
    @Override
    protected void executeCommand() {
      if (showing != chessClocksVisible) {
        if (showing) {
          showClocks();
        }
        else {
          hideClocks();
        }
      }
      if (GameModule.getGameModule().getServer().isConnected()) {
        onlineGame = true;
      }
      else {
        if (!onlineGame) {
          onlineGame = online;
        }
      }
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }

    @Override
    public boolean isLoggable() {
      return false;
    }
  }


  /**
   * Processes right-clicks on our button to put up a context menu
   */
  private class ChessMouseListener implements MouseListener, ActionListener {
    protected JPopupMenu popup;

    /**
     * Process popup results
     * @param e popup result
     */
    @Override
    public void actionPerformed(ActionEvent e) {
      final String command = e.getActionCommand();
      if (command.contains(CHESSMENU_NEXT)) {
        final Command c = startNextClock();
        if ((c != null) && !c.isNull()) {
          c.execute();
          GameModule.getGameModule().sendAndLog(c);
        }
      }
      else if (command.contains(CHESSMENU_PAUSE)) {
        final Command c = stopAllClocks();
        c.execute();
        GameModule.getGameModule().sendAndLog(c);
      }
      else if (command.contains(CHESSMENU_SHOW)) {
        pressControlButton();
      }
      else if (command.contains(CHESSMENU_HIDE)) {
        pressControlButton();
      }
    }

    /**
     * Build our context menu
     */
    void buildPopup() {
      popup = new JPopupMenu();
      popup.addPopupMenuListener(new PopupMenuListener() {
        @Override
        public void popupMenuCanceled(PopupMenuEvent evt) {
          getComponent().repaint();
        }

        @Override
        public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) {
          getComponent().repaint();
        }

        @Override
        public void popupMenuWillBecomeVisible(PopupMenuEvent evt) {
        }
      });

      JMenuItem item;
      final String s;

      if (getClocksTicking() > 0) {
        s = CHESSMENU_PAUSE + "  " + NamedHotKeyConfigurer.getString(pauseListener.getNamedKeyStroke());
      }
      else {
        if (getChessClocksVisible()) {
          s = CHESSMENU_HIDE + "  " + NamedHotKeyConfigurer.getString(showListener.getNamedKeyStroke());
        }
        else {
          s = CHESSMENU_SHOW + "  " + NamedHotKeyConfigurer.getString(showListener.getNamedKeyStroke());
        }
      }
      item = new JMenuItem(s);
      item.addActionListener(this);
      popup.add(item);

      item = new JMenuItem(CHESSMENU_NEXT + "  " + NamedHotKeyConfigurer.getString(nextListener.getNamedKeyStroke()));
      item.addActionListener(this);
      popup.add(item);
    }

    /**
     * Build and display our context menu
     * @param p Coordinates for popup
     */
    void doPopup(Point p) {
      buildPopup();
      popup.show(getComponent(), p.x, p.y);
    }


    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (e.isPopupTrigger()) { // how we detect context menu clicks in this age of the world
        doPopup(e.getPoint());
      }
    }

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (e.isPopupTrigger()) { // how we detect context menu clicks in this age of the world
        doPopup(e.getPoint());
      }
    }
  }
}
