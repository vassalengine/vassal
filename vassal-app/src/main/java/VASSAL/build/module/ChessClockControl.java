/*
 *
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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

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
import VASSAL.configure.StringEnum;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;

public class ChessClockControl extends AbstractConfigurable
        implements CommandEncoder, GameComponent, UniqueIdManager.Identifyable {
  protected static UniqueIdManager idMgr = new UniqueIdManager("ChessClockControl"); //$NON-NLS-1$

  protected Set<ChessClock> chessclocks = new HashSet<>();  // List of individual chess clocks attached to our control

  protected String showTenthSeconds = STYLE_AUTO;  // Configurable clock display style
  protected String showSeconds = STYLE_AUTO;
  protected String showHours = STYLE_AUTO;
  protected String showDays = STYLE_AUTO;

  protected LaunchButton chessClockButton;  // Main chess clock button on toolbar
  protected boolean chessClocksVisible;     // Whether the individual clocks are visible or hidden (as opposed to just main chess clock button)
  protected boolean onlineGame;             // If we've ever detected a live "online" connection during use of the clocks

  protected NamedKeyStrokeListener nextListener;   // Hotkey listeners
  protected NamedKeyStrokeListener showListener;
  protected NamedKeyStrokeListener pauseListener;

  protected boolean instanceIsActive;       // True when this instance has been fully initialized & registered (and not yet shut down)

  protected String id;

  public static final String NAME = "name";
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$
  public static final String BUTTON_TOOLTIP = "buttonTooltip"; //$NON-NLS-1$

  public static final String PAUSE_HOTKEY = "pauseHotkey";
  public static final String NEXT_HOTKEY = "nextHotkey";
  public static final String SHOW_HOTKEY = "showHotkey";

  public static final String SHOW_TENTHSECONDS = "showTenths";
  public static final String SHOW_SECONDS = "showSeconds";
  public static final String SHOW_HOURS = "showHours";
  public static final String SHOW_DAYS = "showDays";

  public static final String STYLE_ALWAYS = "Always";
  public static final String STYLE_AUTO   = "Auto";
  public static final String STYLE_NEVER  = "Never";

  public static final String CHESSMENU_PAUSE = "Pause All Clocks";
  public static final String CHESSMENU_SHOW  = "Show Chess Clocks";
  public static final String CHESSMENU_HIDE  = "Hide Chess Clocks";
  public static final String CHESSMENU_NEXT  = "Start Next Clock";

  public static final String COMMAND_PREFIX = "CLOCKCONTROL:"; //NON-NLS-1$

  public ChessClockControl() {
    setConfigureName("Chess Clock Control");

    chessClocksVisible = false;
    onlineGame         = false;

    ActionListener al = e -> pressControlButton();

    chessClockButton = new LaunchButton("", BUTTON_TOOLTIP, BUTTON_TEXT, SHOW_HOTKEY, ICON, al); //$NON-NLS-1$
    chessClockButton.setToolTipText("Tooltip Text");
    chessClockButton.setAttribute(ICON, "chessclock.png");
    chessClockButton.addMouseListener(new ChessMouseListener());

    // Set up listeners for hotkeys
    nextListener = new NamedKeyStrokeListener(e -> {
      Command c = startNextClock();
      if ((c != null) && !c.isNull()) {
        c.execute();
        GameModule.getGameModule().sendAndLog(c);
      }
    });
    GameModule.getGameModule().addKeyStrokeListener(nextListener);

    showListener = new NamedKeyStrokeListener(e -> pressControlButton());
    GameModule.getGameModule().addKeyStrokeListener(showListener);

    pauseListener = new NamedKeyStrokeListener(e -> {
      Command command = stopAllClocks();
      command.execute();
      GameModule.getGameModule().sendAndLog(command);
    });
    GameModule.getGameModule().addKeyStrokeListener(pauseListener);
  }

  // Registers us with the game module, tool bar, command encoder, etc.
  public void addTo(Buildable parent) {
    final GameModule gameModule = GameModule.getGameModule();
    gameModule.getToolBar().add(getComponent());
    gameModule.addCommandEncoder(this);
    gameModule.getGameState().addGameComponent(this);
    idMgr.add(this);

    for (ChessClock c : chessclocks) {
      c.addToToolbar();
    }

    instanceIsActive = true;
  }

  // Unregisters us when shutting down
  public void removeFrom(Buildable parent) {
    instanceIsActive = false;

    final GameModule gameModule = GameModule.getGameModule();
    gameModule.getToolBar().remove(getComponent());
    gameModule.removeCommandEncoder(this);
    gameModule.getGameState().removeGameComponent(this);
    idMgr.remove(this);
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }


  public boolean getChessClocksVisible() {
    return chessClocksVisible;
  }

  private JButton getComponent() {
    return chessClockButton;
  }

  public static String getConfigureTypeName() {
    return "Chess Clock Control";
  }


  // XML file attributes - next six methods configure them, and handle setting/getting.

  @Override
  public String[] getAttributeNames() {
    return new String[] { NAME, ICON, BUTTON_TEXT, BUTTON_TOOLTIP, SHOW_HOTKEY, NEXT_HOTKEY,
      PAUSE_HOTKEY, SHOW_TENTHSECONDS, SHOW_SECONDS, SHOW_HOURS, SHOW_DAYS };
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { ChessClock.class };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] { "Name: ", "Button Image: ", "Button Text: ", "Tooltip Text: ",
      "Show Clocks Hotkey: ", "Start Next Clock Hotkey: ", "Pause All Clocks Hotkey: ", "Show Tenths of Seconds: ", "Show Seconds: ",
      "Show Hours: ", "Show Days: " };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] { String.class, IconConfig.class, String.class, String.class,
      NamedKeyStroke.class, NamedKeyStroke.class, NamedKeyStroke.class, TimeStyleConfig.class, TimeStyleConfig.class,
      TimeStyleConfig.class, TimeStyleConfig.class };
  }

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
    else {
      chessClockButton.setAttribute(key, value);
    }
  }


  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + ""; //$NON-NLS-1$
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
    else {
      return chessClockButton.getAttributeValueString(key);
    }
  }

  public HelpFile getHelpFile() {
    return null;
  }


  // When we need to find the ChessClockControl in a static context
  public static ChessClockControl getInstance() {
    return GameModule.getGameModule().getAllDescendantComponentsOf(ChessClockControl.class).get(0);
  }


  public String getShowTenths() {
    return showTenthSeconds;
  }


  public String getShowSeconds() {
    return showSeconds;
  }


  public String getShowHours() {
    return showHours;
  }


  public String getShowDays() {
    return showDays;
  }


  //Returns the number of chess clocks currently ticking (active)
  public int getClocksTicking() {
    int running = 0;
    if (instanceIsActive) {
      for (ChessClock c : getInstance().chessclocks) {
        if (c.isTicking()) {
          running++;
        }
      }
    }
    return running;
  }


  //Returns a command to stop all the clocks
  public Command stopAllClocks() {
    Command command = new NullCommand();
    if (instanceIsActive) {
      for (ChessClock c : getInstance().chessclocks) {
        if (!c.isTicking())
          continue;
        command = command.append(c.updateState(false));
      }
    }
    return command;
  }

  // Returns a command to start the next clock after stopping the one currently running (if any)
  public Command startNextClock() {
    Command command = new NullCommand();

    if (chessclocks.isEmpty()) { // If we don't have any clocks, can't start one.
      return command;
    }

    Iterator<ChessClock> it = chessclocks.iterator();
    ChessClock first = null;
    boolean found = false;
    while (it.hasNext()) {
      ChessClock clock = it.next();
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

  // Hide all the clocks on the toolbar
  public void hideClocks() {
    chessClocksVisible = false;
    if (instanceIsActive) {
      for (ChessClock c : getInstance().chessclocks) {
        c.hideClock();
      }
    }
  }

  // Show all the clocks on the toolbar
  public void showClocks() {
    chessClocksVisible = true;
    if (instanceIsActive) {
      for (ChessClock c : getInstance().chessclocks) {
        c.showClock();
      }
    }
  }


  // Update all clocks (when our time-display format changes)
  public void updateAllClocks() {
    if (instanceIsActive) {
      for (ChessClock c : getInstance().chessclocks) {
        c.setTimerButton();
      }
    }
  }


  // Figures out the "right thing to do" when player clicks the main chess clock control button
  public void pressControlButton() {
    if (!chessClocksVisible) {
      showClocks();
    }
    else {
      if (getClocksTicking() > 0) {
        Command command = stopAllClocks();
        command.execute();
        GameModule.getGameModule().sendAndLog(command);
      }
      else {
        hideClocks();
      }
    }
  }



  public void setOnline(boolean online) {
    onlineGame = online;
  }


  public boolean isOnline() {
    return onlineGame;
  }


  public void setup(final boolean gameStarting) {
    if (gameStarting) {
      setOnline(GameModule.getGameModule().getServer().isConnected());
    }
    else {
      stopAllClocks();
    }
  }

  // Allow individual chess clocks to add themselves to our control scheme
  public void addChessClock(ChessClock clock) {
    chessclocks.add(clock);
  }

  public void removeChessClock(ChessClock clock) {
    chessclocks.remove(clock);
  }


  // Autoconfigurer for master chessclock button icon
  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((ChessClockControl) c).chessClockButton.getAttributeValueString(ICON));
    }
  }

  // Autoconfigurer for the days/hours/minutes/seconds/tenths styles
  public static class TimeStyleConfig extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { STYLE_ALWAYS, STYLE_AUTO, STYLE_NEVER };
    }
  }


  public Command decode(final String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      final SequenceEncoder.Decoder decoder = new SequenceEncoder.Decoder(command, ':');
      decoder.nextToken();
      final boolean showing = decoder.nextBoolean(false);
      final boolean online  = decoder.nextBoolean(false);
      return new UpdateClockControlCommand(showing, online);
    }
    else {
      return null;
    }
  }

  public String encode(final Command c) {
    if (c instanceof UpdateClockControlCommand) {
      final UpdateClockControlCommand comm = (UpdateClockControlCommand) c;
      final SequenceEncoder encoder = new SequenceEncoder(':');
      encoder.append(comm.showing);
      encoder.append(comm.online);
      return COMMAND_PREFIX + encoder.getValue();
    }
    else {
      return null;
    }
  }


  public Command getRestoreCommand() {
    return new UpdateClockControlCommand(chessClocksVisible, onlineGame);
  }


  //Our "command" format for passing information about the master clock control between computers (or to/from save and log files)
  public class UpdateClockControlCommand extends Command {
    private final boolean showing;
    private final boolean online;

    public UpdateClockControlCommand(boolean showing, boolean online) {
      this.showing = showing;
      this.online  = online;
    }

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


  // ChessMouseListener class exists so that we can process right-clicks and put up a popup menu.
  class ChessMouseListener implements MouseListener, ActionListener {
    protected JPopupMenu popup;

    // Process popup results
    public void actionPerformed(ActionEvent e) {
      String command = e.getActionCommand();
      if (command.contains(CHESSMENU_NEXT)) {
        Command c = startNextClock();
        if ((c != null) && !c.isNull()) {
          c.execute();
          GameModule.getGameModule().sendAndLog(c);
        }
      }
      else if (command.contains(CHESSMENU_PAUSE)) {
        Command c = stopAllClocks();
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


    //Build a popup
    void buildPopup() {
      popup = new JPopupMenu();
      popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
        public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
          getComponent().repaint();
        }

        public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
          getComponent().repaint();
        }

        public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
        }
      });

      JMenuItem item;
      String s;

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


    void doPopup(Point p) {
      buildPopup();
      popup.show(getComponent(), p.x, p.y);
    }


    public void mouseEntered(MouseEvent e) {

    }

    public void mouseExited(MouseEvent e) {

    }

    public void mouseReleased(MouseEvent e) {
      if (e.isPopupTrigger()) { // how we detect context menu clicks in this age of the world
        doPopup(e.getPoint());
      }
    }

    public void mouseClicked(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
      if (e.isPopupTrigger()) { // how we detect context menu clicks in this age of the world
        doPopup(e.getPoint());
      }
    }
  }
}
