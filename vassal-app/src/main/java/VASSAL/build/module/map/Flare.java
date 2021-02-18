/*
 * Copyright (c) 2020 Vassalengine.org   Brian Reynolds
 * Inspired by VASL piece finder by David Sullivan
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

package VASSAL.build.module.map;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Point;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.List;

import VASSAL.build.BadDataReport;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;
import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.TimingTargetAdapter;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.FlareCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.FlareFormattedStringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.SwingUtils;

/**
 * Allows a player to ping a location ("send up a flare") by clicking on a map with the correct modifier key
 * combination held down (default: Alt+LeftClick). Can be shown as an animated colored circle, or a plain one.
 * If reportFormat field is provided, a message is also displayed in the chat log.
 *
 * Flare will work with both online play and PBEM play.
 */
public class Flare extends AbstractConfigurable
        implements CommandEncoder, GameComponent, Drawable, MouseListener, UniqueIdManager.Identifyable {
  private static final char DELIMITER = '\t'; //$NON-NLS-1$
  public  static final String COMMAND_PREFIX = "FLARE" + DELIMITER; //$NON-NLS-1$

  public static final String NO_ANIMATION = "noFlareAnimation"; //$NON-NLS-1$

  protected static final UniqueIdManager idMgr = new UniqueIdManager("Flare"); //$NON-NLS-1$
  protected String id = "";     // Our unique ID

  // Attributes
  private int circleSize;       // Maximum circle size in pixels
  private boolean circleScale;  // If true, scale circle to map zoom
  private int pulses;           // How many total "pulses" to animate, or 0 for steady w/o animation
  private int pulsesPerSec;     // How many pulses per second, or 0 for steady w/o animation
  private String flareKey;      // Configures which set of modifier keys and click will produce the flare
  private Color color;          // Color for the flare circle
  private FormattedString reportFormat = new FormattedString(); // Report format for reporting the flare to the chat log

  // Internal properties
  private Map map;                  // The map for this Flare
  private Point clickPoint;         // Clicked point where this Flare is to appear
  private boolean animate;          // Internal flag if we should be animating
  private volatile boolean active;  // Internal flag if we're currently active

  // Attribute names
  public static final String CIRCLE_SIZE    = "circleSize";         //$NON-NLS-1$
  public static final String CIRCLE_SCALE   = "circleScale";        //$NON-NLS-1$
  public static final String CIRCLE_COLOR   = "circleColor";        //$NON-NLS-1$
  public static final String FLARE_KEY      = "flareKey";           //$NON-NLS-1$
  public static final String PULSES         = "flarePulses";        //$NON-NLS-1$
  public static final String PULSES_PER_SEC = "flarePulsesPerSec";  //$NON-NLS-1$
  public static final String REPORT_FORMAT  = "reportFormat";       //$NON-NLS-1$
  public static final String NAME           = "flareName";          //$NON-NLS-1$

  // Friendly (localizable) names for modifier key combinations
  public static final String FLARE_ALT_LOCAL       = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt"));                       //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_CTRL_LOCAL      = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl"));                      //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_COMMAND_LOCAL   = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.meta"));                      //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_ALT_SHIFT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt_shift"));                 //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_SHIFT_COMMAND_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.shift_command"));         //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_CTRL_SHIFT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl_shift"));               //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_ALT_COMMAND_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt_command"));             //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_CTRL_ALT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl_alt"));                   //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_ALT_SHIFT_COMMAND_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt_shift_command")); //$NON-NLS-1$ //$NON-NLS-2$
  public static final String FLARE_CTRL_ALT_SHIFT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl_alt_shift"));       //$NON-NLS-1$ //$NON-NLS-2$

  // The modifier key codes we actually store
  public static final String FLARE_ALT            = "keyAlt";          //$NON-NLS-1$
  public static final String FLARE_CTRL           = "keyCtrl";         //$NON-NLS-1$
  public static final String FLARE_ALT_SHIFT      = "keyAltShift";     //$NON-NLS-1$
  public static final String FLARE_CTRL_SHIFT     = "keyCtrlShift";    //$NON-NLS-1$
  public static final String FLARE_CTRL_ALT       = "keyCtrlAlt";      //$NON-NLS-1$
  public static final String FLARE_CTRL_ALT_SHIFT = "keyCtrlAltShift"; //$NON-NLS-1$

  // Special properties for our FormattedString reportFormat
  public static final String FLARE_NAME           = "FlareName";       //$NON-NLS-1$
  public static final String FLARE_LOCATION       = "FlareLocation";   //$NON-NLS-1$
  public static final String FLARE_ZONE           = "FlareZone";       //$NON-NLS-1$
  public static final String FLARE_MAP            = "FlareMap";        //$NON-NLS-1$

  private static final int STROKE = 3;

  public Flare() {
    circleSize   = 100;
    circleScale  = true;
    color        = Color.RED;
    active       = false;
    flareKey     = FLARE_ALT;
    pulses       = 6;
    pulsesPerSec = 3;
    setConfigureName(Resources.getString("Editor.Flare.desc"));
  }

  /**
   * @return String description of this component, displayed in Editor.
   */
  public String getDescription() {
    return Resources.getString("Editor.Flare.desc"); //$NON-NLS-1$
  }

  /**
   * @return String name of component class. The part in [..] in the Editor.
   */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Flare.configure"); //$NON-NLS-1$
  }

  /**
   * @return the Map associated with this Flare component.
   */
  public Map getMap() {
    return map;
  }

  /**
   * Attribute types for this component's buildFile (xml) entry. These launch the proper configurers when the component is edited in the Editor.
   * @return list of classes for attributes
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] {
      String.class,
      FlareKeyConfig.class,
      Integer.class,
      Color.class,
      Boolean.class,
      Integer.class,
      Integer.class,
      ReportFormatConfig.class
    };
  }

  /**
   * Attribute names for this component's buildFile (xml) entry.
   * @return list of names for attributes
   */
  @Override
  public String[] getAttributeNames() {
    return new String[] {
      NAME,
      FLARE_KEY,
      CIRCLE_SIZE,
      CIRCLE_COLOR,
      CIRCLE_SCALE,
      PULSES,
      PULSES_PER_SEC,
      REPORT_FORMAT
    };
  }

  /**
   * Attribute names for this component's buildFile (xml) entry. These show up in the Editor next to the configurers for each attribute.
   * @return list of names for attributes
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.name_label"), //$NON-NLS-1$
      Resources.getString("Editor.Flare.flare_key"), //$NON-NLS-1$
      Resources.getString("Editor.Flare.circle_size"), //$NON-NLS-1$
      Resources.getString("Editor.Flare.circle_color"), //$NON-NLS-1$
      Resources.getString("Editor.Flare.circle_scale"), //$NON-NLS-1$
      Resources.getString("Editor.Flare.pulses"), //$NON-NLS-1$
      Resources.getString("Editor.Flare.pulses_per_sec"), //$NON-NLS-1$
      Resources.getString("Editor.report_format") //$NON-NLS-1$
    };
  }

  /**
   * Gets current attribute value in string form.
   *
   * @param key - attribute name
   *
   * @return current the value of one of this component's attributes, in string form.
   */
  @Override
  public String getAttributeValueString(final String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (FLARE_KEY.equals(key)) {
      return flareKey;
    }
    else if (CIRCLE_SIZE.equals(key)) {
      return String.valueOf(circleSize);
    }
    else if (CIRCLE_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (CIRCLE_SCALE.equals(key)) {
      return String.valueOf(circleScale);
    }
    else if (PULSES.equals(key)) {
      return String.valueOf(pulses);
    }
    else if (PULSES_PER_SEC.equals(key)) {
      return String.valueOf(pulsesPerSec);
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    return null;
  }

  /**
   * Sets the value of one of this component's attributes.
   *
   * @param key the name of the attribute
   *
   * @param value new value for attribute. Can pass either the Object itself or the String version.
   */
  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (FLARE_KEY.equals(key)) {
      if (FLARE_ALT_LOCAL.equals(value)) {
        flareKey = FLARE_ALT;
      }
      else if (FLARE_COMMAND_LOCAL.equals(value) || FLARE_CTRL_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL;
      }
      else if (FLARE_ALT_SHIFT_LOCAL.equals(value)) {
        flareKey = FLARE_ALT_SHIFT;
      }
      else if (FLARE_SHIFT_COMMAND_LOCAL.equals(value) || FLARE_CTRL_SHIFT_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL_SHIFT;
      }
      else if (FLARE_ALT_COMMAND_LOCAL.equals(value) || FLARE_CTRL_ALT_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL_ALT;
      }
      else if (FLARE_ALT_SHIFT_COMMAND_LOCAL.equals(value) || FLARE_CTRL_ALT_SHIFT_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL_ALT_SHIFT;
      }
      else {
        flareKey = (String) value;
      }
    }
    else if (CIRCLE_SIZE.equals(key)) {
      if (value instanceof String) {
        circleSize = Integer.parseInt((String) value);
      }
      else if (value instanceof Integer) {
        circleSize = (Integer) value;
      }
    }
    else if (CIRCLE_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color)value;
    }
    else if (CIRCLE_SCALE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      circleScale = (Boolean)value;
    }
    else if (PULSES.equals(key)) {
      if (value instanceof String) {
        pulses = Integer.parseInt((String) value);
      }
      else {
        pulses = (Integer) value;
      }
    }
    else if (PULSES_PER_SEC.equals(key)) {
      if (value instanceof String) {
        pulsesPerSec = Integer.parseInt((String) value);
      }
      else {
        pulsesPerSec = (Integer) value;
      }
    }
    else if (REPORT_FORMAT.equals(key)) {
      if (value instanceof String) {
        reportFormat.setFormat((String)value);
      }
      else {
        reportFormat = (FormattedString)value;
      }
    }
  }

  /**
   * @return Help file for this component, accessed when "Help" button in Editor is clicked while configuring component.
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Flare.html");  //$NON-NLS-1$
  }

  /**
   * Adds this component to a Buildable component. In this case, a Map.
   * @param parent - the Map to add the Flare to.
   */
  @Override
  public void addTo(final Buildable parent) {
    idMgr.add(this);
    if (parent instanceof Map) {
      map = (Map) parent;
      final GameModule g = GameModule.getGameModule();

      g.addCommandEncoder(this);
      map.addDrawComponent(this);
      map.addLocalMouseListener(this);

      g.getPrefs().addOption(
        Resources.getString("Prefs.general_tab"), // $NON-NLS-1$
        new BooleanConfigurer(
          NO_ANIMATION,
          Resources.getString("Flare.no_animation"), // $NON-NLS-1$
          Boolean.FALSE
        )
      );
    }
    else {
      ErrorDialog.dataWarning(new BadDataReport("Flare - can only be added to a Map. ", //NON-NLS
                                                 reportFormat.getFormat()));
    }
  }

  /**
   * Removes this component from a Buildable parent.
   * @param parent - the Map to remove the Flare from.
   */
  @Override
  public void removeFrom(final Buildable parent) {
    if (parent instanceof Map) {
      GameModule.getGameModule().removeCommandEncoder(this);
    }
    idMgr.remove(this);
  }

  private double os_scale = 1.0;

  private volatile float animfrac;

  /**
   * Repaint only the necessary area
   */
  private void repaintArea() {
    map.repaint(new Rectangle(
      (int)(clickPoint.x - circleSize / 2.0 - 2 * STROKE * os_scale),
      (int)(clickPoint.y - circleSize / 2.0 - 2 * STROKE * os_scale),
      (int)(circleSize + 4 * STROKE * os_scale + 0.5),
      (int)(circleSize + 4 * STROKE * os_scale + 0.5)
    ));
  }

  /**
   * Animator to loop the Flare animation. Use the LOOP behavior so that it's always shrinking bullseye rings.
   */
  private final Animator animator = new Animator(0, 1, Animator.RepeatBehavior.LOOP, new TimingTargetAdapter() {
    @Override
    public void begin() {
      active = true;
      animfrac = 0.0f;
      repaintArea();
    }

    /**
     * Animator tells us when to update the image.
     * @param fraction Animator lets us know how far we are through our cycle
     */
    @Override
    public void timingEvent(float fraction) {
      animfrac = fraction;
      repaintArea();
    }

    /**
     * Animator tells us we're done.
     */
    @Override
    public void end() {
      active = false;
      repaintArea();
    }
  });

  /**
   * Start the Flare animation.
   * @param isLocal - true if this Flare was launched on this client (i.e. by this player). Otherwise, center on
   *                  the ping location if user preferences are set for centering on opponent moves.
   */
  public void startAnimation(final boolean isLocal) {
    if (map == null) {
      return; // Means we already mentioned BadModuleData earlier. Now we simply avoid crashing.
    }

    if (!isLocal) {
      if (GlobalOptions.getInstance().centerOnOpponentsMove()) {
        map.centerAt(clickPoint);
      }
    }

    animator.stop();
    animate = Boolean.FALSE.equals(GameModule.getGameModule().getPrefs().getValue(NO_ANIMATION));
    animator.setRepeatCount(Math.max(pulses, 1));
    animator.setDuration(1000 / Math.max(pulsesPerSec, 1));
    animator.start();
  }

  /**
   * Draw the Flare at current animation frame
   * @param g - Graphics
   * @param map - Map component
   */
  @Override
  public void draw(final Graphics g, final Map map) {
    if (!active || clickPoint == null) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    double diameter = (circleScale ? map.getZoom() : 1.0) * os_scale * circleSize;
    if (animate) {
      diameter *= (1.0 - animfrac);
    }

    if (diameter <= 0.0) {
      return;
    }

    // translate the click location for current zoom
    final Point p = map.mapToDrawing(clickPoint, os_scale);

    // draw a circle around the selected point
    g2d.setColor(color);
    g2d.setStroke(new BasicStroke((float)(STROKE * os_scale)));
    g2d.drawOval(
      (int)(p.x - diameter / 2.0),
      (int)(p.y - diameter / 2.0),
      (int)(diameter + 0.5),
      (int)(diameter + 0.5)
    );
  }

  /**
   * Flare is always drawn above counters
   * @return true
   */
  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  /**
   * Serializes Flare commands into string form.
   * @param c Command
   * @return String version of Flare Command, or null if not a Flare Command.
   */
  @Override
  public String encode(final Command c) {
    if (c instanceof FlareCommand) {
      final FlareCommand fc = (FlareCommand) c;
      final SequenceEncoder se = new SequenceEncoder(DELIMITER);
      se.append(fc.getId())
        .append(fc.getClickPoint().x)
        .append(fc.getClickPoint().y);
      return COMMAND_PREFIX + se.getValue();
    }
    return null;
  }

  /**
   * Deserializes string command info into a Flare Command.
   * @param s String for a Flare Command
   * @return Flare Command object
   */
  @Override
  public Command decode(final String s) {
    if (s.startsWith(COMMAND_PREFIX + getId() + DELIMITER)) { // Make sure this command is for this flare
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, DELIMITER);
      sd.nextToken(); // Skip over the Command Prefix
      sd.nextToken(); // Skip over the Id
      final int x = sd.nextInt(0);
      final int y = sd.nextInt(0);
      clickPoint = new Point(x, y);
      return new FlareCommand(this);
    }
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  /**
   * @param e MouseEvent
   */
  @Override
  public void mouseClicked(final MouseEvent e) {
  }

  /**
   * Check for our modifier key, and if so launch a Flare sequence on our map, and send a command to other players' along
   * with a Chat Log message if a Report Format has been specified in the component.
   * @param e - a MouseEvent
   */
  @Override
  public void mousePressed(final MouseEvent e) {
    if (!SwingUtils.isMainMouseButtonDown(e)) {
      return;
    }
    if (FLARE_CTRL.equals(flareKey)) {
      if (!SwingUtils.isSelectionToggle(e) || e.isAltDown() || e.isShiftDown()) {
        return;
      }
    }
    else if (FLARE_ALT.equals(flareKey)) {
      if (!e.isAltDown() || e.isShiftDown() || SwingUtils.isSelectionToggle(e)) {
        return;
      }
    }
    else if (FLARE_ALT_SHIFT.equals(flareKey)) {
      if (!e.isAltDown() || !e.isShiftDown() || SwingUtils.isSelectionToggle(e)) {
        return;
      }
    }
    else if (FLARE_CTRL_SHIFT.equals(flareKey)) {
      if (!e.isShiftDown() || !SwingUtils.isSelectionToggle(e) || e.isAltDown()) {
        return;
      }
    }
    else if (FLARE_CTRL_ALT.equals(flareKey)) {
      if (!e.isAltDown() || !SwingUtils.isSelectionToggle(e) || e.isShiftDown()) {
        return;
      }
    }
    else if (FLARE_CTRL_ALT_SHIFT.equals(flareKey)) {
      if (!e.isAltDown() || !e.isShiftDown() || !SwingUtils.isSelectionToggle(e)) {
        return;
      }
    }
    clickPoint = new Point(e.getX(), e.getY());

    final GameModule mod = GameModule.getGameModule();

    Command c = new NullCommand();

    // Set special properties for our reportFormat to use ($FlareLocation$, $FlareZone$, $FlareMap$)
    reportFormat.setProperty(FLARE_NAME, getAttributeValueString(NAME));
    reportFormat.setProperty(FLARE_LOCATION, map.locationName(clickPoint));
    final Zone z = map.findZone(clickPoint);
    reportFormat.setProperty(FLARE_ZONE, (z != null) ? z.getName() : "");
    reportFormat.setProperty(FLARE_MAP, map.getMapName());
    final String reportText = reportFormat.getLocalizedText(map); // Map and global properties also available (e.g. PlayerName, PlayerSide)
    if (!reportText.isBlank()) {
      c = new Chatter.DisplayText(mod.getChatter(), "* " + reportText);
      c.execute();
    }

    // Send to other players
    c = c.append(new FlareCommand(this));
    mod.sendAndLog(c);

    // Start animation on our own client
    startAnimation(true);
  }

  /**
   * @param e MouseEvent
   */
  @Override
  public void mouseReleased(final MouseEvent e) {
  }

  /**
   * @param e MouseEvent
   */
  @Override
  public void mouseEntered(final MouseEvent e) {
  }

  /**
   * @param e MouseEvent
   */
  @Override
  public void mouseExited(final MouseEvent e) {
  }

  /**
   * @param gameStarting true if starting a game, false if ending one
   */
  @Override
  public void setup(final boolean gameStarting) {
  }

  /**
   * @return Theoretically returns the command to "restore" this from a saved game or when adding a new player, but of
   * course Flare doesn't need to be "restored", so null.
   */
  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * Record our clicked point on the map
   * @param p - point where player clicked and where flare should be shown
   */
  public void setClickPoint(final Point p) {
    clickPoint = p;
  }

  /**
   * @return point where player clicked and where flare should be shown
   */
  public Point getClickPoint() {
    return clickPoint;
  }

  /**
   * Sets our unique ID (among Flares), so that multiple Flares don't inadvertently start each other when we
   * send commands to other clients.
   * @param id Sets our unique ID
   */
  @Override
  public void setId(String id) {
    this.id = id;
  }

  /**
   * @return unique ID of this flare, for purposes of distinguishing between Flare commands
   */
  @Override
  public String getId() {
    return id;
  }

  /**
   * A configurer for different combinations of modifier key
   */
  public static class FlareKeyConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {
        FLARE_ALT,
        FLARE_CTRL,
        FLARE_ALT_SHIFT,
        FLARE_CTRL_SHIFT,
        FLARE_CTRL_ALT,
        FLARE_CTRL_ALT_SHIFT
      };
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      final boolean macLegacy = GlobalOptions.getInstance().getPrefMacLegacy();
      return new String[] {
        FLARE_ALT_LOCAL,
        SystemUtils.IS_OS_MAC && !macLegacy ? FLARE_COMMAND_LOCAL : FLARE_CTRL_LOCAL,
        FLARE_ALT_SHIFT_LOCAL,
        SystemUtils.IS_OS_MAC && !macLegacy ? FLARE_SHIFT_COMMAND_LOCAL : FLARE_CTRL_SHIFT_LOCAL,
        SystemUtils.IS_OS_MAC && !macLegacy ? FLARE_ALT_COMMAND_LOCAL : FLARE_CTRL_ALT_LOCAL,
        SystemUtils.IS_OS_MAC && !macLegacy ? FLARE_ALT_SHIFT_COMMAND_LOCAL : FLARE_CTRL_ALT_SHIFT_LOCAL,
      };
    }

    @Override
    public boolean isDisplayNames() {
      return true;
    }
  }

  /**
   * A configurer for our reportFormat, which includes the unique $FlareLocation$, $FlareZone$, $FlareMap$ properties as
   * well as $PlayerName$ and $PlayerSide$ in the "Insert" pulldown.
   */
  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FlareFormattedStringConfigurer(key, name, new String[0]);
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(reportFormat.getFormat());
  }

  /**
   * In case reports use HTML and  refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    final HTMLImageFinder h = new HTMLImageFinder(reportFormat.getFormat());
    h.addImageNames(s);
  }
}
