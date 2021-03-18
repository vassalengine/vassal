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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.ChessClockControl;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;
import org.apache.commons.lang3.StringUtils;

/**
 * CHESS CLOCK class for VASSAL.
 *
 * The ChessClock class itself implements a single timer, which is added as a button to the Module toolbar.
 * Clicking the button starts and stops the clock, which we attempt to keep (roughly) in sync across multiple
 * players' computers. A pair (or more, or I suppose only one) of them are then added to {@link ChessClockControl}.
 *
 * @author Brian Reynolds and Michael Kiefte
 *
 * Class originally created by Michael Kiefte for Twilight Struggle, including handshaking for verification of time totals between online
 * machines. Made more generically configurable, ChessClockControl added, and integrated into standard VASSAL, by Brian Reynolds.
 *
 */
public class ChessClock extends AbstractConfigurable implements CommandEncoder, GameComponent, ActionListener {
  private static final int MILLISECONDS_PER_MINUTE = 1000 * 60;
  private static final int MILLISECONDS_PER_HOUR = MILLISECONDS_PER_MINUTE * 60;
  private static final int MILLISECONDS_PER_DAY = MILLISECONDS_PER_HOUR * 24;
  public static final char DELIMITER = '\t';
  public static final String COMMAND_PREFIX = "CLOCK" + DELIMITER; //$NON-NLS-1$

  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String SIDE = "side"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$

  public static final String TICKING_BACKGROUND_COLOR = "tickingBackgroundColor"; //NON-NLS
  public static final String TICKING_FONT_COLOR       = "tickingFontColor"; //NON-NLS
  public static final String TOCKING_FONT_COLOR       = "tockingFontColor"; //NON-NLS

  public static final String GENERIC   = "Player"; //NON-NLS

  public static final String CHESSMENU_START  = Resources.getString("ChessClock.start"); //NON-NLS
  public static final String CHESSMENU_STOP   = Resources.getString("ChessClock.stop"); //NON-NLS
  public static final String CHESSMENU_RESET  = Resources.getString("ChessClock.reset"); //NON-NLS

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
  protected String buttonText;         // Button text for clock button
  protected long elapsedTime;          // Apparent elapsed time
  protected long verifiedTime;         // Time successfully verified through handshake
  protected boolean clockTicking;      // Is this clock running

  protected boolean tocking; // Tracks the half-second "pulse" for the font color

  protected boolean instanceIsActive;  // True if this ChessClock has been fully initialized & registered (and not yet shut down)


  public ChessClock(String side) {
    this();
    this.side       = side;
    this.buttonText = side;
    setConfigureName(side);
    setTimerButton();
  }


  public ChessClock() {
    defaultColor = UIManager.getColor("Panel.background");      // Store our original default background color
    defaultFontColor = UIManager.getColor("Button.foreground"); // Store our original default font color

    side       = GENERIC;
    buttonText = side;

    // In case we're adding these on the fly, let's see if we can find a "decent" player side to "be",
    // in other words one that hasn't been assigned to any clock yet. This is a little bit tongue-in-cheek,
    // but it's intended to be heuristic not perfect, and doesn't have to be perfect. So there.
    final PlayerRoster r = GameModule.getGameModule().getPlayerRoster();
    final ChessClockControl ctrl = ChessClockControl.getInstance();
    if ((r != null) && (ctrl != null)) {
      for (final String s : r.getSides()) {
        boolean okay = true;
        for (final ChessClock c : ctrl.getChessClocks()) {
          if (c.getSide().equals(s)) {
            okay = false;
            break;
          }
        }
        if (!okay) {
          continue;
        }
        side       = s;
        buttonText = s;
        break;
      }
    }

    setConfigureName(side);

    image        = "";
    elapsedTime  = 0;
    verifiedTime = 0;
    clockTicking = false;

    // Timer is static so it gets created exactly once ever
    if (timer == null) {
      timer = new Timer(100, this);
    }

    // But every ChessClock instance needs its own action listener
    timer.addActionListener(this);

    // Action listener for clicking on the clock
    final ActionListener al = e -> {
      Command command = new NullCommand();
      final ChessClockControl ccc = ChessClockControl.getInstance();
      if ((ccc != null) && ccc.getClocksTicking() > 0) {
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
    timerButton = new LaunchButton(buttonText + " " + "0:00:00", TOOLTIP, null, null, ICON, al); //$NON-NLS-1$
    timerButton.setFont(new Font("SansSerif", Font.BOLD, 12)); //$NON-NLS-1$
    timerButton.setHorizontalAlignment(SwingConstants.LEFT);
    timerButton.addMouseListener(new ChessClock.ClockMouseListener());
    initTimerButton();
  }

  /**
   * @return side this clock is assigned to
   */
  public String getSide() {
    return side;
  }

  /**
   * @return true if this clock is running
   */
  public boolean isTicking() {
    return clockTicking;
  }

  /**
   * @return elapsed time from local perspective
   */
  public long getElapsed() {
    return elapsedTime;
  }

  /**
   * @return elapsed time verified by handshake (or fiat, depending on situation)
   */
  public long getVerified() {
    return verifiedTime;
  }

  /**
   * @param name side to check if a referee
   * @return True if passed side is a referee
   */
  protected static boolean isReferee(String name) {
    return PlayerRoster.isSoloSide(name);
  }

  /**
   *  Make sure the static millisecond timer is actually running (unless it hasn't even been initialized yet)
   */
  protected void startTimer() {
    if (!timer.isRunning()) {
      timer.start();
    }
  }

  /**
   *  Defines the Command by which information about this clock is communicated between machines (and to/from save and log files)
   */
  private class UpdateTimerCommand extends Command {
    private final String who; // who = player who is doing the reporting
    private final String name; // name = whose timer is being reported
    private final long elapsed; // Elapsed time from our point of view
    private final long verified; // Verified time (both clients have agreed)
    private final boolean ticking; //BR// Timer currently ticking?
    private final boolean restore; //BR// Restoring save?

    /**
     * @param who who is doing the reporting; which player side
     * @param name side for whom timer is being reported
     * @param elapsed elapsed time from our point of view
     * @param verified verified time (both clients have agreed)
     * @param ticking Time currently ticking?
     * @param restore Restoring saved game?
     */
    public UpdateTimerCommand(String who, String name, long elapsed, long verified, boolean ticking, boolean restore) {
      this.who = who;
      this.name = name;
      this.elapsed = elapsed;
      this.verified = verified;
      this.ticking = ticking;
      this.restore = restore;
    }

    /**
     * Resets the clock to 0
     * @param who who is doing the resetting
     * @param name side for whom clock is to be reset
     */
    public UpdateTimerCommand(String who, String name) {
      this(who, name, -1, -1, false, false);
    }

    /**
     * Everything that comes from another computer is verified unless its verified time is earlier than what we've already verified.
     */
    @Override
    protected void executeCommand() {
      if (!name.equals(side)) { // this shouldn't happen
        return;
      }
      final String me = PlayerRoster.getMySide();
      if (restore) {
        startTime = -1; // If restoring saved game, clear out millisecond timer
      }
      startTimer();
      if (GameModule.getGameModule().getServer().isConnected()) {
        final ChessClockControl ccc = ChessClockControl.getInstance();
        if (ccc != null) ccc.setOnline(true);
      }

      // This part is mostly so that "testing" the chess clocks when game not yet going feels responsive. Also gives referee control access to clocks.
      final boolean noChecks = GameModule.getGameModule().getGameState().isGameStarted() || isReferee(who) || ((ChessClockControl.getInstance() != null) && !ChessClockControl.getInstance().isOnline());

      if ((elapsedTime == -1) && (verifiedTime == -1)) {
        // We are resetting this clock
        elapsedTime  = 0;
        verifiedTime = 0;
        clockTicking = false;
      }
      else if (who.equals(me) || noChecks) {
        // my computer reporting back: restore
        // Don't update elapsed time. It could have run on before saving
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
        final ChessClockControl ccc = ChessClockControl.getInstance();
        if (ccc != null) ccc.showClocks();
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
   * Updates the color of this chess clock's button/display.
   * Non-ticking clock uses "default" button look.
   * Ticking clock uses special background and pulsing foreground.
   */
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

  /**
   * Put timer in initial state, and invisible.
   */
  private void initTimerButton() {
    timerButton.setVisible(false);
    updateTimerColor();
  }

  /**
   * Hides this clock
   */
  public void hideClock() {
    timerButton.setVisible(false);
  }

  /**
   * Shows this clock
   */
  public void showClock() {
    timerButton.setVisible(true);
    setTimerButton();
  }

  /**
   * Updates the clock display for this clock
   * (1) Checks new elapsed value from millisecond timer.
   * (2) Calls setTimerButton to clock time (text) on the button
   * (3) Updates timer button colors if appropriate
   */
  public void updateDisplay() {
    if (startTime == -1) {
      startTime = System.currentTimeMillis();
      return;
    }

    final long currentTime = System.currentTimeMillis();
    final long elapsed = currentTime - startTime;
    startTime = currentTime;

    if (clockTicking) {
      elapsedTime += elapsed;
      if (setTimerButton()) {
        updateTimerColor();
      }
    }
  }

  /**
   * @return Clock button text (if any) formatted to be ready to be prepended to time clock string
   */
  private String getFormattedButtonText() {
    return (StringUtils.isEmpty(buttonText) ? "" : buttonText + " ");
  }

  /**
   * Updates clock text on the button, based on our configured style
   * @return half second tick-tock pulse on/off
   */
  public boolean setTimerButton() {
    long time = elapsedTime;
    final int days = (int) (time / MILLISECONDS_PER_DAY);
    time %= MILLISECONDS_PER_DAY;
    int hours = (int) (time / MILLISECONDS_PER_HOUR);
    time %= MILLISECONDS_PER_HOUR;
    int minutes = (int) (time / MILLISECONDS_PER_MINUTE);
    time %= MILLISECONDS_PER_MINUTE;
    final int seconds = (int) time / 1000;
    time %= 1000;
    final int tenths  = (int) time / 100;

    final String showingDays;
    final String showingHours;
    final String showingSeconds;
    final String showingTenths;

    final ChessClockControl c = ChessClockControl.getInstance();
    if ((c != null) && instanceIsActive) {
      showingDays = c.getShowDays();
      showingHours = c.getShowHours();
      showingSeconds = c.getShowSeconds();
      showingTenths = c.getShowTenths();
    }
    else {
      showingDays    = ChessClockControl.STYLE_AUTO;
      showingHours   = ChessClockControl.STYLE_AUTO;
      showingSeconds = ChessClockControl.STYLE_AUTO;
      showingTenths  = ChessClockControl.STYLE_AUTO;
    }

    final boolean doDays  = (ChessClockControl.STYLE_ALWAYS).equals(showingDays) || ((days > 0) && !((ChessClockControl.STYLE_NEVER).equals(showingDays)));
    final boolean doHours = doDays || (ChessClockControl.STYLE_ALWAYS).equals(showingHours) || (((hours > 0) || ((ChessClockControl.STYLE_NEVER).equals(showingTenths))) && !((ChessClockControl.STYLE_NEVER).equals(showingHours)));
    final boolean doTenths  = (ChessClockControl.STYLE_ALWAYS).equals(showingTenths) || (!doHours && !((ChessClockControl.STYLE_NEVER).equals(showingTenths)));
    final boolean doSeconds = doTenths || (ChessClockControl.STYLE_ALWAYS).equals(showingSeconds) || (!doDays && !((ChessClockControl.STYLE_NEVER).equals(showingSeconds)));

    final boolean oldTocking = tocking;
    tocking = (tenths >= 5);
    final String baseline;

    if (doDays) {
      if (doTenths) {
        timerButton.setText(String.format("%s%d:%02d:%02d:%02d.%d", getFormattedButtonText(), days, hours, minutes, seconds, tenths)); //NON-NLS
        baseline = "0:00:00:00.0"; //NON-NLS
      }
      else if (doSeconds) {
        timerButton.setText(String.format("%s%d:%02d:%02d:%02d", getFormattedButtonText(), days, hours, minutes, seconds)); //NON-NLS
        baseline = "0:00:00:00"; //NON-NLS
      }
      else {
        timerButton.setText(String.format("%s%dd:%02d:%02d", getFormattedButtonText(), days, hours, minutes)); //NON-NLS
        baseline = "0d:00:00"; //NON-NLS
      }
    }
    else {
      hours = hours + days * 24;
      if (doHours) {
        if (doTenths) {
          timerButton.setText(String.format("%s%d:%02d:%02d.%d", getFormattedButtonText(), hours, minutes, seconds, tenths)); //NON-NLS
          if (hours >= 10) {
            baseline = String.format("%d0:00:00.0", hours/10); //NON-NLS
          }
          else {
            baseline = "0:00:00.0"; //NON-NLS
          }
        }
        else if (doSeconds) {
          timerButton.setText(String.format("%s%d:%02d:%02d", getFormattedButtonText(), hours, minutes, seconds)); //NON-NLS
          if (hours >= 10) {
            baseline = String.format("%d0:00:00", hours/10); //NON-NLS
          }
          else {
            baseline = "0:00:00"; //NON-NLS
          }
        }
        else {
          timerButton.setText(String.format("%s%d:%02d", getFormattedButtonText(), hours, minutes)); //NON-NLS
          if (hours >= 10) {
            baseline = String.format("%d0:00", hours/10); //NON-NLS
          }
          else {
            baseline = "0:00"; //NON-NLS
          }
        }
      }
      else {
        minutes = minutes + hours * 60;

        if (doTenths) {
          timerButton.setText(String.format("%s%d:%02d.%d", getFormattedButtonText(), minutes, seconds, tenths)); //NON-NLS
          if (minutes >= 10) {
            baseline = String.format("%d0:00.0", minutes/10); //NON-NLS
          }
          else {
            baseline = "0:00.0"; //NON-NLS
          }
        }
        else {
          timerButton.setText(String.format("%s%d:%02d", getFormattedButtonText(), minutes, seconds)); //NON-NLS
          if (minutes >= 10) {
            baseline = String.format("%d0:00", minutes/10); //NON-NLS
          }
          else {
            baseline = "0:00"; //NON-NLS
          }
        }
      }
    }

    // adjust size to prevent button wobbling
    final Insets ins = timerButton.getInsets();

    final int iconWidth;
    final int iconHeight;

    final Icon icon = timerButton.getIcon();
    if (icon != null) {
      iconWidth = icon.getIconWidth() + ins.left;
      iconHeight = icon.getIconHeight() + ins.top + ins.bottom;
    }
    else {
      iconWidth  = 0;
      iconHeight = 0;
    }

    final FontMetrics metrics = timerButton.getFontMetrics(timerButton.getFont());
    final int width = ins.left + iconWidth + metrics.stringWidth(getFormattedButtonText() + baseline + ins.right);
    final int textHeight = ins.top + metrics.getHeight() + ins.bottom;
    timerButton.setPreferredSize(new Dimension(width, Math.max(textHeight, iconHeight)));

    return tocking != oldTocking;
  }

  public String getName() {
    return side;
  }

  /**
   * Registers us with game module, command encoder, and the Chess Clock Control.
   * @param parent Should be ChessClockControl
   */
  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof ChessClockControl) {
      initTimerButton();
      final GameModule gameModule = GameModule.getGameModule();
      gameModule.addCommandEncoder(this);
      gameModule.getGameState().addGameComponent(this);

      //BR// Add ourselves to the chess clock controller
      final ChessClockControl ccc = ((ChessClockControl) parent);
      ccc.addChessClock(this);

      //BR// Chess Clock Control handles adding our button to the toolbar when it is built (because otherwise
      //BR// its own button would annoyingly appear after ours), but if it has already been built (we're adding this
      //BR// new Clock "live", in the Editor), then we need to add ourselves to the toolbar.
      if (ccc.isInstanceIsActive()) {
        gameModule.getToolBar().add(timerButton);
      }

      instanceIsActive = true; //BR// We're now open for business.
    }
    else {
      ErrorDialog.dataWarning(new BadDataReport("Chess Clock can only be added to Chess Clock Control", ""));  //NON-NLS
    }
  }

  /**
   * Adds our clock button to the Module's toolbar
   */
  public void addToToolbar() {
    final GameModule gameModule = GameModule.getGameModule();
    gameModule.getToolBar().add(timerButton);
  }

  /**
   * Unregisters us when we are shutting down
   * @param parent Should be ChessClockControl
   */
  @Override
  public void removeFrom(Buildable parent) {
    instanceIsActive = false;

    if (parent instanceof ChessClockControl) {
      timerButton.setVisible(false);
      final GameModule gameModule = GameModule.getGameModule();
      gameModule.getToolBar().remove(timerButton); // We are always responsible for removing ourselves from toolbar, even though Clock Control sometimes adds us.
      gameModule.removeCommandEncoder(this);
      gameModule.getGameState().removeGameComponent(this);
      if (timer != null) {
        timer.removeActionListener(this);
      }

      ((ChessClockControl) parent).removeChessClock(this);
    }
  }

  /**
   * @return Key names for our attributes from the buildFile (XML) definition.
   */
  @Override
  public String[] getAttributeNames() {
    return new String[] { SIDE, BUTTON_TEXT, ICON, TICKING_BACKGROUND_COLOR, TICKING_FONT_COLOR, TOCKING_FONT_COLOR, TOOLTIP };
  }

  /**
   * @return Descriptions for our buildFile (XML) attributes. These appear when our component is configured in the Editor window.
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.ChessClock.player_side_for_timer"),
      Resources.getString("Editor.ChessClock.timer_button_text"),
      Resources.getString("Editor.ChessClock.timer_icon"),
      Resources.getString("Editor.ChessClock.ticking_background_color"),
      Resources.getString("Editor.ChessClock.ticking_foreground_color"),
      Resources.getString("Editor.ChessClock.tocking_foreground_color"),
      Resources.getString("Editor.ChessClock.chess_clock_tooltip")
    };
  }

  /**
   * @return Class types for configuring each of our buildFile (XML) attributes. Specifies which flavor of configurer to uses.
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] { PlayerSidesConfig.class, String.class, IconConfig.class, ColorConfig.class, ColorConfig2.class, ColorConfig3.class, String.class };
  }

  /**
   * Sets the value of one of this component's XML attributes
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @param value New value for the attribute. Can be either String version or the actual Object.
   */
  @Override
  public void setAttribute(String key, Object value) {
    if (SIDE.equals(key)) {
      side = (String)value;
      setConfigureName(side);
    }
    else if (BUTTON_TEXT.equals(key)) {
      buttonText = (String)value;
      setTimerButton();
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

  /**
   * Gets the value of one of this component's XML attributes
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @return String value of the attribute.
  */
  @Override
  public String getAttributeValueString(String key) {
    if (SIDE.equals(key)) {
      return side;
    }
    else if (BUTTON_TEXT.equals(key)) {
      return buttonText;
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

  /**
   * @return The help file for this component. Used when user clicks "Help" button while configuring the component in the Editor.
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChessClock.html");  //$NON-NLS-1$
  }

  /**
   * @return Configure Tree name for component. Appears in [..] in module editor.
   */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ChessClock.chess_clock");
  }

  /**
   * @return Array of subcomponent types that can be added to this component
   */
  @SuppressWarnings("rawtypes")
  @Override
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }


  /**
   * @return the JButton for our clock's button
   */
  private JButton getComponent() {
    return timerButton;
  }


  /**
   * Deserializes our command from a string version, if the command belongs to us.
   * @param command Serialized string command
   * @return An {@link UpdateTimerCommand}
   */
  @Override
  public Command decode(final String command) {
    if (!command.startsWith(COMMAND_PREFIX)) {
      return null;
    }
    final SequenceEncoder.Decoder decoder = new SequenceEncoder.Decoder(command, DELIMITER);
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

  /**
   * Serializes our command into a string, if it belongs to us
   * @param c Command to serialize. Only serialized if it's an UpdateTimerCommand.
   * @return Serialized command, or null if command passed wasn't an UpdateTimerCommand.
   */
  @Override
  public String encode(final Command c) {
    if (!(c instanceof UpdateTimerCommand)) {
      return null;
    }
    final UpdateTimerCommand comm = (UpdateTimerCommand) c;
    final SequenceEncoder encoder = new SequenceEncoder(DELIMITER);
    encoder.append(comm.who);
    encoder.append(comm.name);
    encoder.append(comm.elapsed);
    encoder.append(comm.verified);
    encoder.append(comm.ticking);
    encoder.append(comm.restore);
    return COMMAND_PREFIX + encoder.getValue();
  }

  /**
   * Detect when game is starting.
   * @param gameStarting if true, a game is starting. If false, then a game is ending
   */
  @Override
  public void setup(final boolean gameStarting) {
    clockTicking = false;
    if (!gameStarting) {
      elapsedTime  = 0;
      verifiedTime = 0;
    }
    setTimerButton();
    updateTimerColor();
  }

  /**
   * @return Our command for restoring from a saved game (or adding an online player)
   */
  @Override
  public Command getRestoreCommand() {
    final long verified = isReferee(PlayerRoster.getMySide()) ? elapsedTime : verifiedTime;
    return new UpdateTimerCommand(PlayerRoster.getMySide(), getName(), elapsedTime, verified, false, true);
  }

  /**
   * This processes our timer updates every 100ms
   * @param evt Timer event
   */
  @Override
  public void actionPerformed(ActionEvent evt) {
    if (evt.getSource() == timer) {
      updateDisplay();
    }
  }

  /**
   * Creates a command to update the clock and/or turn it on/off.
   * @param ticking If true, the clock will be set to "running". If false the clock will be set to "stopped".
   * @return a command to communicate the status of this clock to others
   */
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


  /**
   * Creates a command to reset the clock and turn it off
   * @param ticking If true, the clock will be set to "running". If false the clock will be set to "stopped".
   * @return a command to communicate the status of this clock to others
   */
  public Command resetState() {
    final String mySide = PlayerRoster.getMySide();
    if (mySide != null) {
      return new UpdateTimerCommand(mySide, getName());
    }
    else if (!GameModule.getGameModule().getGameState().isGameStarted()) {
      return new UpdateTimerCommand("", getName());
    }
    else {
      return new NullCommand();
    }
  }


  // These autoconfigurers allow the attributes to be edited in the editor, using intuitive controls instead of just a fill-in-the-blank field.

  /**
   * PlayerSidesConfig makes a dropdown of all the player sides
   */
  public static class PlayerSidesConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      final PlayerRoster r = GameModule.getGameModule().getPlayerRoster();
      final ArrayList<String> sides = new ArrayList<>(r.getSides());
      if (r.getSides().size() == 0) {
        sides.add(GENERIC);
      }
      return sides.toArray(new String[0]);
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      final ArrayList<String> sides = new ArrayList<>();
      final PlayerRoster r = GameModule.getGameModule().getPlayerRoster();
      for (final String s : r.getSides()) {
        sides.add(r.translateSide(s));
      }
      if (r.getSides().size() == 0) {
        sides.add(GENERIC);
      }
      return sides.toArray(new String[0]);
    }
  }

  /**
   * Autoconfigurer for the icon for this timer. Lets user pick an icon.
   */
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "");
    }
  }

  /**
   * Color auto-configurer. Lets user pick a color from the table and/or values.
   */
  public static class ColorConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorConfigurer(key, name, ColorConfigurer.stringToColor(c.getAttributeValueString(TICKING_BACKGROUND_COLOR)));
    }
  }

  /**
   * Color auto-configurer. Lets user pick a color from the table and/or values.
   */
  public static class ColorConfig2 implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorConfigurer(key, name, ColorConfigurer.stringToColor(c.getAttributeValueString(TICKING_FONT_COLOR)));
    }
  }

  /**
   * Color auto-configurer. Lets user pick a color from the table and/or values.
   */
  public static class ColorConfig3 implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorConfigurer(key, name, ColorConfigurer.stringToColor(c.getAttributeValueString(TOCKING_FONT_COLOR)));
    }
  }


  /**
   * Processes right-clicks on our clock's button to put up a context menu
   */
  private class ClockMouseListener implements MouseListener, ActionListener {
    protected JPopupMenu popup;

    /**
     * Process popup results
     * @param e popup result
     */
    @Override
    public void actionPerformed(ActionEvent e) {
      final String command = e.getActionCommand();

      Command c = new NullCommand();
      final ChessClockControl ccc = ChessClockControl.getInstance();
      if (command.contains(CHESSMENU_START) || command.contains(CHESSMENU_STOP)) {
        if ((ccc != null) && ccc.getClocksTicking() > 0) {
          c = c.append(ChessClockControl.getInstance().startNextClock());
        }
        else {
          c = c.append(updateState(true));
        }
      }
      else if (command.contains(CHESSMENU_RESET)) {
        c = resetState();

        final PlayerRoster.PlayerInfo me = new PlayerRoster.PlayerInfo(
          GameModule.getUserId(),
          GlobalOptions.getInstance().getPlayerId(),
          PlayerRoster.getMySide()
        );

        c = c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), Resources.getString(GlobalOptions.getInstance().chatterHTMLSupport() ? "ChessClock.reset_clock_2" : "ChessClock.reset_clock", me.playerName, side)));
      }
      c.execute();
      GameModule.getGameModule().sendAndLog(c);

      if (timer == null) {
        setup(true);
      }
      startTimer();
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

      if (clockTicking) {
        s = CHESSMENU_STOP;
      }
      else {
        s = CHESSMENU_START;
      }
      item = new JMenuItem(s);
      item.addActionListener(this);
      popup.add(item);

      final ChessClockControl ccc = ChessClockControl.getInstance();
      if ((ccc == null) || ccc.isAllowReset()) {
        item = new JMenuItem(CHESSMENU_RESET);
        item.addActionListener(this);
        popup.add(item);
      }
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
