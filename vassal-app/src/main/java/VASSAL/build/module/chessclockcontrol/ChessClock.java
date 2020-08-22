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
package VASSAL.build.module.chessclockcontrol;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;
import javax.swing.UIManager;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.ChessClockControl;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;

//
// CHESS CLOCK class for VASSAL.
//
// Class originally created by Michael Kiefte for Twilight Struggle, including handshaking for verification of time totals between online
// machines.
//
// Made more generically configurable, and ChessClockControl added, by Brian Reynolds
//
// The ChessClock class itself implements a single timer. A pair (or more, or I suppose only one) of them are then added to ChessClockControl.
//
public class ChessClock extends AbstractConfigurable implements CommandEncoder, GameComponent, ActionListener {

  private static final int MILLISECONDS_PER_MINUTE = 1000 * 60;
  private static final int MILLISECONDS_PER_HOUR = MILLISECONDS_PER_MINUTE * 60;
  private static final int MILLISECONDS_PER_DAY = MILLISECONDS_PER_HOUR * 24;
  public static final String COMMAND_PREFIX = "CLOCK:"; //$NON-NLS-1$

  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String SIDE = "side"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$

  public static final String TICKING_BACKGROUND_COLOR = "tickingBackgroundColor";
  public static final String TICKING_FONT_COLOR       = "tickingFontColor";
  public static final String TOCKING_FONT_COLOR       = "tockingFontColor";

  public static final String SOLITAIRE = "Solitaire"; // Various common names for sides that have access to all pieces (and therefore clocks)
  public static final String REFEREE   = "Referee";
  public static final String SOLO      = "Solo";

  private static Color defaultColor;       // Stores default look-and-feel colors for buttons
  private static Color defaultFontColor;

  protected Color tickingBackgroundColor = new Color(255, 255, 0);  // Starting (default) colors
  protected Color tickingFontColor       = new Color(51, 51, 51);
  protected Color tockingFontColor       = new Color(0, 0, 0);

  protected static Timer timer;    // Millisecond timer (but only actually updated every ~100ms)
  protected long startTime = -1;   // Millisecond timer count of last timer check

  protected LaunchButton timerButton;  // Button (and display) for this clock
  protected String image;              // Icon for this clock
  protected String side;               // Name of side/player whose time this clock represents
  protected long elapsedTime;          // Apparent elapsed time
  protected long verifiedTime;         // Time successfully verified through handshake
  protected boolean clockTicking;      // Is this clock running

  protected boolean tocking; // Tracks the half-second "pulse" for the font color

  protected boolean instanceIsActive;  // True if this ChessClock has been fully initialized & registered (and not yet shut down)

  public ChessClock() {
    defaultColor = UIManager.getColor ("Panel.background");      // Store our original default background color
    defaultFontColor = UIManager.getColor ("Button.foreground"); // Store our original default font color

    side = "Player";
    setConfigureName(side);

    image        = "";
    elapsedTime  = 0;
    verifiedTime = 0;
    clockTicking = false;

    // Timer is static so it gets created exactly once ever
    if (timer == null) {
      timer = new Timer(100, this);
      timer.stop();
    }

    // But every ChessClock instance needs its own action listener
    timer.addActionListener(this);

    // Action listener for clicking on the clock
    ActionListener al = e -> {
      Command command = new NullCommand();
      if (ChessClockControl.getInstance().getClocksTicking() > 0) {
        command = command.append(ChessClockControl.getInstance().startNextClock());
      }
      else {
        command = command.append(updateState(true));
      }
      command.execute();
      GameModule.getGameModule().sendAndLog(command);

      if (timer == null) {
        setup(true);
      }
      startTimer();
    };

    // Create our actual timer button
    timerButton = new LaunchButton("0:00:00", TOOLTIP, null, null, ICON, al); //$NON-NLS-1$
    timerButton.setFont(new Font("SansSerif", Font.BOLD, 12)); //$NON-NLS-1$
    initTimerButton();
  }

  public boolean isTicking() {
    return clockTicking;
  }

  public long getElapsed() {
    return elapsedTime;
  }

  public long getVerified() {
    return verifiedTime;
  }

  protected static boolean isReferee(String name) {
    return SOLITAIRE.equals(name) || REFEREE.equals(name) || SOLO.equals(name);
  }


  // Make sure the static millisecond timer is actually running (unless it hasn't even been initialized yet)
  protected void startTimer() {
    if (!timer.isRunning()) {
      timer.start();
    }
  }


  // Defines the Command by which information about this clock is communicated between machines (and to/from save and log files)
  public class UpdateTimerCommand extends Command {
    private final String who; // who = player who is doing the reporting
    private final String name; // name = whose timer is being reported
    private final long elapsed; // Elapsed time from our point of view
    private final long verified; // Verified time (both clients have agreed)
    private final boolean ticking; // BR// Timer currently ticking?
    private final boolean restore; // BR// Restoring save?

    // who = who is reporting; which player
    // name = which timer
    public UpdateTimerCommand(String who, String name, long elapsed, long verified, boolean ticking, boolean restore) {
      this.who = who;
      this.name = name;
      this.elapsed = elapsed;
      this.verified = verified;
      this.ticking = ticking;
      this.restore = restore;
    }

    // everything that comes from another computer is verified unless its verified
    // time is earlier than this one
    @Override
    protected void executeCommand() {
      if (!name.equals(side)) // this shouldn't happen
        return;
      final String me = PlayerRoster.getMySide();
      if (restore) {
        startTime = -1; // BR// If restoring saved game, clear out millisecond timer
      }
      startTimer();
      if (GameModule.getGameModule().getServer().isConnected()) {
        ChessClockControl.getInstance().setOnline(true);
      }

      // This part is mostly so that "testing" the chess clocks when game not yet going feels responsive. Also gives referee control access to clocks.
      boolean noChecks = GameModule.getGameModule().getGameState().isGameStarted() || isReferee(who) || !ChessClockControl.getInstance().isOnline();

      if (who.equals(me) || noChecks) {
        // my computer reporting back: restore
        // don't update elapsed time. It could have run on before saving
        if (noChecks) {
          elapsedTime = elapsed;
          verifiedTime = elapsed;
        }
        else {
          elapsedTime = verified;
          verifiedTime = verified;
        }
        clockTicking = ticking;
      }
      else if (name.equals(me)) { // someone else's computer is reporting my time
        if (verified > elapsedTime) {
          // later than what we thought due to synchronization
          verifiedTime = verified;
          elapsedTime = verified;
        }
        else if (verified > 0L) {
          // getting back the time I originally reported (successful connection!)
          verifiedTime = verified;
        }
      }
      else if (who.equals(name)) {
        // someone else's computer reporting back their own time
        elapsedTime = elapsed;
        verifiedTime = elapsed; // we assume this is correct
        clockTicking = ticking;
        // send it back for verification
        if (me != null) {
          GameModule.getGameModule().sendAndLog(new UpdateTimerCommand(me, getName(), elapsedTime, verifiedTime, ticking, false));
        }
      }

      setTimerButton();
      updateTimerColor();

      if (clockTicking || (restore && (elapsedTime > 0))) {
        ChessClockControl.getInstance().showClocks();
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


  // Updates the color of this chess clock's button/display.
  // Non-ticking clock uses "default" button look.
  // Ticking clock uses special background and pulsing foreground.
  protected void updateTimerColor() {
    if (!instanceIsActive) return;
    if (clockTicking) {
      timerButton.setBackground(tickingBackgroundColor);
      timerButton.setOpaque(true);
      timerButton.setBorderPainted(false);
      timerButton.setForeground(tocking ? tockingFontColor : tickingFontColor);
    }
    else {
      timerButton.setBackground(defaultColor);
      timerButton.setOpaque(false);
      timerButton.setBorderPainted(true);
      timerButton.setForeground(defaultFontColor);
    }
  }


  private void initTimerButton() {
    timerButton.setVisible(false);
    updateTimerColor();
  }


  public void hideClock() {
    timerButton.setVisible(false);
  }

  public void showClock() {
    timerButton.setVisible(true);
  }


  // Updates the clock display for this clock
  // (1) Checks new elapsed value from millisecond timer.
  // (2) Calls setTimerButton to clock time (text) on the button
  // (3) Updates timer button colors if appropriate
  public void updateDisplay() {
    if (startTime == -1) {
      startTime = System.currentTimeMillis();
      return;
    }

    long currentTime = System.currentTimeMillis();
    long elapsed = currentTime - startTime;
    startTime = currentTime;

    if (clockTicking) {
      elapsedTime += elapsed;
      if (setTimerButton()) {
        updateTimerColor();
      }
    }
  }

  // Updates clock text on the button, based on our configured style
  public boolean setTimerButton() {
    if (!instanceIsActive) return false;

    long time = elapsedTime;
    int days = (int) (time / MILLISECONDS_PER_DAY);
    time %= MILLISECONDS_PER_DAY;
    int hours = (int) (time / MILLISECONDS_PER_HOUR);
    time %= MILLISECONDS_PER_HOUR;
    int minutes = (int) (time / MILLISECONDS_PER_MINUTE);
    time %= MILLISECONDS_PER_MINUTE;
    int seconds = (int) time / 1000;
    time %= 1000;
    int tenths  = (int) time / 100;

    ChessClockControl c   = ChessClockControl.getInstance();
    String showingDays    = c.getShowDays();
    String showingHours   = c.getShowHours();
    String showingSeconds = c.getShowSeconds();
    String showingTenths  = c.getShowTenths();

    boolean doDays  = (ChessClockControl.STYLE_ALWAYS).equals(showingDays) || ((days > 0) && !((ChessClockControl.STYLE_NEVER).equals(showingDays)));
    boolean doHours = doDays || (ChessClockControl.STYLE_ALWAYS).equals(showingHours) || (((hours > 0) || ((ChessClockControl.STYLE_NEVER).equals(showingTenths))) && !((ChessClockControl.STYLE_NEVER).equals(showingHours)));
    boolean doTenths  = (ChessClockControl.STYLE_ALWAYS).equals(showingTenths) || (!doHours && !((ChessClockControl.STYLE_NEVER).equals(showingTenths)));
    boolean doSeconds = doTenths || (ChessClockControl.STYLE_ALWAYS).equals(showingSeconds) || (!doDays && !((ChessClockControl.STYLE_NEVER).equals(showingSeconds)));

    boolean oldTocking = tocking;
    tocking = (tenths >= 5);

    if (doDays) {
      if (doTenths) {
        timerButton.setText(String.format("%d:%02d:%02d:%02d.%d", days, hours, minutes, seconds, tenths));
      }
      else if (doSeconds) {
        timerButton.setText(String.format("%d:%02d:%02d:%02d", days, hours, minutes, seconds));
      }
      else {
        timerButton.setText(String.format("%dd:%02d:%02d", days, hours, minutes));
      }
    }
    else {
      hours = hours + days * 24;
      if (doHours) {
        if (doTenths) {
          timerButton.setText(String.format("%d:%02d:%02d.%d", hours, minutes, seconds, tenths));
        }
        else if (doSeconds) {
          timerButton.setText(String.format("%d:%02d:%02d", hours, minutes, seconds));
        }
        else {
          timerButton.setText(String.format("%d:%02d", hours, minutes));
        }
      }
      else {
        minutes = minutes + hours * 60;

        if (doTenths) {
          timerButton.setText(String.format("%d:%02d.%d", minutes, seconds, tenths));
        }
        else {
          timerButton.setText(String.format("%d:%02d", minutes, seconds));
        }
      }
    }

    return tocking != oldTocking;
  }

  public String getName() {
    return side;
  }

  // Registers us with game module, command encoder, and the Chess Clock Control.
  public void addTo(Buildable parent) {
    initTimerButton();
    final GameModule gameModule = GameModule.getGameModule();
    gameModule.addCommandEncoder(this);
    gameModule.getGameState().addGameComponent(this);

    // BR// Add ourselves to the chess clock controller
    if (parent instanceof ChessClockControl) {
      ((ChessClockControl) parent).addChessClock(this);
    }
    else {
      ChessClockControl.getInstance().addChessClock(this);
    }

    instanceIsActive = true;
  }

  public void addToToolbar() {
    final GameModule gameModule = GameModule.getGameModule();
    gameModule.getToolBar().add(timerButton);
  }

  // Unregisters us when we are shutting down
  public void removeFrom(Buildable parent) {
    instanceIsActive = false;

    timerButton.setVisible(false);
    final GameModule gameModule = GameModule.getGameModule();
    gameModule.getToolBar().remove(timerButton);
    gameModule.removeCommandEncoder(this);
    gameModule.getGameState().removeGameComponent(this);
    if (timer != null) {
      timer.removeActionListener(this);
    }

    if (parent instanceof ChessClockControl) {
      ((ChessClockControl) parent).removeChessClock(this);
    }
    else {
      ChessClockControl.getInstance().removeChessClock(this);
    }
  }


  // Next several classes deal with our XML file attributes, allowing getting/setting, etc.

  @Override
  public String[] getAttributeNames() {
    return new String[] { SIDE, ICON, TICKING_BACKGROUND_COLOR, TICKING_FONT_COLOR, TOCKING_FONT_COLOR, TOOLTIP };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] { "Player Side for timer: ", "Timer Icon: ", "Ticking Background Color: ", "Ticking Foreground Color: ", "Tocking Foreground Color: ", "Chess Clock Tooltip" };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] { String.class, IconConfig.class, ColorConfig.class, ColorConfig2.class, ColorConfig3.class, String.class };
  }


  @Override
  public void setAttribute(String key, Object value) {
    if (SIDE.equals(key)) {
      side = (String)value;
      setConfigureName(side);
    }
    else if (TICKING_BACKGROUND_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      tickingBackgroundColor = (Color) value;
      updateTimerColor();
    }
    else if (TICKING_FONT_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      tickingFontColor = (Color) value;
      updateTimerColor();
    }
    else if (TOCKING_FONT_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      tockingFontColor = (Color) value;
      updateTimerColor();
    }
    else {
      timerButton.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (SIDE.equals(key)) {
      return side;
    }
    else if (TICKING_BACKGROUND_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(tickingBackgroundColor);
    }
    else if (TICKING_FONT_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(tickingFontColor);
    }
    else if (TOCKING_FONT_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(tockingFontColor);
    }
    else {
      return timerButton.getAttributeValueString(key);
    }
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Chess Clock";
  }

  @SuppressWarnings("rawtypes")
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public Command decode(final String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      final SequenceEncoder.Decoder decoder = new SequenceEncoder.Decoder(command, ':');
      decoder.nextToken();
      final String who = decoder.nextToken();
      final String name = decoder.nextToken();
      if (!getName().equals(name))
        return null;
      final long elapsed = decoder.nextLong(0L);
      final long verified = decoder.nextLong(0L);
      final boolean ticking = decoder.nextBoolean(false);
      final boolean restore = decoder.nextBoolean(false);
      return new UpdateTimerCommand(who, name, elapsed, verified, ticking, restore);
    }
    else {
      return null;
    }
  }


  public String encode(final Command c) {
    if (c instanceof UpdateTimerCommand) {
      final UpdateTimerCommand comm = (UpdateTimerCommand) c;
      final SequenceEncoder encoder = new SequenceEncoder(':');
      encoder.append(comm.who);
      encoder.append(comm.name);
      encoder.append(comm.elapsed);
      encoder.append(comm.verified);
      encoder.append(comm.ticking);
      encoder.append(comm.restore);
      return COMMAND_PREFIX + encoder.getValue();
    }
    else {
      return null;
    }
  }

  // Detect when game is starting
  public void setup(final boolean gameStarting) {
    clockTicking = false;
    if (!gameStarting) {
      elapsedTime  = 0;
      verifiedTime = 0;
    }
    setTimerButton();
    updateTimerColor();
  }

  // Our command for restoring from a saved game (or adding an online player)
  public Command getRestoreCommand() {
    long verified = isReferee(PlayerRoster.getMySide()) ? elapsedTime : verifiedTime;
    return new UpdateTimerCommand(PlayerRoster.getMySide(), getName(), elapsedTime, verified, false, true);
  }

  // This processes our timer updates every 100ms
  public void actionPerformed(ActionEvent evt) {
    if (evt.getSource() == timer) {
      updateDisplay();
    }
  }

  // Returns a command to communicate the status of this clock to others
  public Command updateState(boolean ticking) {
    final String mySide = PlayerRoster.getMySide();
    if (mySide != null) {
      return new UpdateTimerCommand(mySide, getName(), elapsedTime, verifiedTime, ticking, false);
    }
    else if (!GameModule.getGameModule().getGameState().isGameStarted()) {
      return new UpdateTimerCommand("", getName(), elapsedTime, verifiedTime, ticking, false);
    }
    else {
      return new NullCommand();
    }
  }

  // These autoconfigurers allow the attributes to be edited in the editor, using intuitive controls instead of just a fill-in-the-blank field.

  // Autoconfigurer for the icon for this timer
  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "");
    }
  }

  // Color auto-configurers
  public static class ColorConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorConfigurer(key, name, ColorConfigurer.stringToColor(c.getAttributeValueString(TICKING_BACKGROUND_COLOR)));
    }
  }

  public static class ColorConfig2 implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorConfigurer(key, name, ColorConfigurer.stringToColor(c.getAttributeValueString(TICKING_FONT_COLOR)));
    }
  }

  public static class ColorConfig3 implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorConfigurer(key, name, ColorConfigurer.stringToColor(c.getAttributeValueString(TOCKING_FONT_COLOR)));
    }
  }
}
