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
package VASSAL.build.module.map;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.LaunchButton;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.swing.SwingUtils;

/**
 * A class that allows the user to draw a straight line on a Map (LOS
 * = Line Of Sight).  No automatic detection of obstacles is
 * performed; the user must simply observe the thread against the
 * image of the map.  However, if the user clicks on a board with a
 * {@link Map Grid}, the thread may snap to the grid and report the
 * distance between endpoints of the line
 * */
public class LOS_Thread extends AbstractToolbarItem implements
    MouseListener, MouseMotionListener,
    Drawable, Configurable,
    UniqueIdManager.Identifyable,
    CommandEncoder {

  public static final String LOS_THREAD_COMMAND = "LOS\t"; //NON-NLS

  public static final String NAME = "threadName"; //NON-NLS
  public static final String HOTKEY = "hotkey"; //NON-NLS
  public static final String TOOLTIP = "tooltip"; //NON-NLS
  public static final String ICON_NAME = "iconName"; //NON-NLS
  public static final String LABEL = "label"; //NON-NLS

  public static final String SNAP_LOS = "snapLOS"; //NON-NLS
  public static final String SNAP_START = "snapStart"; //NON-NLS
  public static final String SNAP_END = "snapEnd"; //NON-NLS
  public static final String REPORT = "report"; //NON-NLS
  public static final String PERSISTENCE = "persistence"; //NON-NLS
  public static final String PERSISTENT_ICON_NAME = "persistentIconName"; //NON-NLS
  public static final String GLOBAL = "global"; //NON-NLS
  public static final String LOS_COLOR = "threadColor"; //NON-NLS
  public static final String DRAW_RANGE = "drawRange"; //NON-NLS
  public static final String HIDE_COUNTERS = "hideCounters"; //NON-NLS
  public static final String HIDE_OPACITY = "hideOpacity"; //NON-NLS
  public static final String RANGE_BACKGROUND = "rangeBg"; //NON-NLS
  public static final String RANGE_FOREGROUND = "rangeFg"; //NON-NLS
  public static final String RANGE_SCALE = "scale"; //NON-NLS
  public static final String RANGE_ROUNDING = "round"; //NON-NLS
  public static final String ROUND_UP = "Up"; //NON-NLS
  public static final String ROUND_DOWN = "Down"; //NON-NLS
  public static final String ROUND_OFF = "Nearest whole number"; //NON-NLS
  public static final Font RANGE_FONT = new Font("Dialog", Font.PLAIN, 11); //NON-NLS
  public static final String DEFAULT_ICON = "/images/thread.gif"; //NON-NLS

  public static final String FROM_LOCATION = "FromLocation"; //NON-NLS
  public static final String TO_LOCATION = "ToLocation"; //NON-NLS
  public static final String CHECK_COUNT = "NumberOfLocationsChecked"; //NON-NLS
  public static final String CHECK_LIST = "AllLocationsChecked"; //NON-NLS
  public static final String RANGE = "Range"; //NON-NLS

  public static final String NEVER = "Never"; //NON-NLS
  public static final String ALWAYS = "Always"; //NON-NLS
  public static final String CTRL_CLICK = "Ctrl-Click & Drag"; //NON-NLS
  public static final String WHEN_PERSISTENT = "When Persisting"; //NON-NLS

  protected static final UniqueIdManager idMgr = new UniqueIdManager("LOS_Thread"); //NON-NLS

  protected boolean retainAfterRelease = false;
  protected long lastRelease = 0;

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected Map map;
  protected KeyStroke hotkey;
  protected Point anchor;
  protected Point arrow;
  protected boolean visible;
  protected boolean drawRange;
  protected int rangeScale;
  protected double rangeRounding = 0.5;
  protected boolean hideCounters;
  protected int hideOpacity = 0;
  protected String fixedColor;
  protected Color threadColor = Color.black, rangeFg = Color.white, rangeBg = Color.black;
  protected boolean snapStart;
  protected boolean snapEnd;
  protected Point lastAnchor = new Point();
  protected Point lastArrow = new Point();
  protected Rectangle lastRangeRect = new Rectangle();
  protected String anchorLocation = "";
  protected String lastLocation = "";
  protected String lastRange = "";
  protected FormattedString reportFormat = new FormattedString(Resources.getString("Editor.LosThread.los_check_string"));
  protected List<String> checkList = new ArrayList<>();
  protected String persistence = CTRL_CLICK;
  protected String persistentIconName;
  protected String global = ALWAYS;
  protected String threadId = "";
  protected boolean persisting;
  protected boolean mirroring;
  protected String iconName;
  protected boolean ctrlWhenClick = false;
  protected boolean initializing;

  public LOS_Thread() {
    anchor = new Point(0, 0);
    arrow = new Point(0, 0);
    visible = false;
    persisting = false;
    mirroring = false;

    setNameKey(NAME);
    setButtonTextKey(LABEL);
    setTooltipKey(TOOLTIP);
    setIconKey(ICON_NAME);
    setHotKeyKey(HOTKEY);

    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.LosThread.show_los_thread"),
      Resources.getString("Editor.LosThread.thread"),
      DEFAULT_ICON,
      e -> launch()
    ));
    launch = getLaunchButton(); // for compatibility
  }

  /**
   * @return whether the thread should be drawn
   */
  public boolean isVisible() {
    return visible;
  }

  /**
   * If true, draw the thread on the map
   */
  public void setVisible(boolean state) {
    visible = state;
  }

  /**
   * Expects to be added to a {@link Map}.  Adds a button to the map
   * window's toolbar.  Pushing the button pushes a MouseListener
   * onto the Map that draws the thread.  Adds some entries to
   * preferences
   *
   * @see Map#pushMouseListener*/
  @Override
  public void addTo(Buildable b) {
    idMgr.add(this);
    map = (Map) b;
    map.getView().addMouseMotionListener(this);
    map.addDrawComponent(this);
    map.getToolBar().add(getLaunchButton());
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getPrefs().addOption(getConfigureName(),
      new BooleanConfigurer(SNAP_LOS,
        Resources.getString("LOS_Thread.snap_thread_preference")));

    if (fixedColor == null) {
      final ColorConfigurer config = new ColorConfigurer(LOS_COLOR,
        Resources.getString("LOS_Thread.thread_color_preference"));
      GameModule.getGameModule().getPrefs().addOption(
        getConfigureName(), config);
      threadColor = (Color) GameModule.getGameModule()
                                      .getPrefs().getValue(LOS_COLOR);
      config.addPropertyChangeListener(evt -> threadColor = (Color) evt.getNewValue());
      config.fireUpdate();
    }
  }

  @Override
  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.removeDrawComponent(this);
    map.getToolBar().remove(getLaunchButton());
    GameModule.getGameModule().removeCommandEncoder(this);
    idMgr.remove(this);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (DRAW_RANGE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      drawRange = (Boolean) value;
    }
    else if (RANGE_SCALE.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      rangeScale = (Integer) value;
    }
    else if (RANGE_ROUNDING.equals(key)) {
      if (ROUND_UP.equals(value)) {
        rangeRounding = 1.0;
      }
      else if (ROUND_DOWN.equals(value)) {
        rangeRounding = 0.0;
      }
      else {
        rangeRounding = 0.5;
      }
    }
    else if (HIDE_COUNTERS.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      hideCounters = (Boolean) value;
    }
    else if (HIDE_OPACITY.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      setTransparency((Integer) value);
    }
    else if (RANGE_FOREGROUND.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      rangeFg = (Color) value;
    }
    else if (RANGE_BACKGROUND.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      rangeBg = (Color) value;
    }
    else if (LOS_COLOR.equals(key)) {
      if (value instanceof Color) {
        value = ColorConfigurer.colorToString((Color) value);
      }
      fixedColor = (String) value;
      threadColor = ColorConfigurer.stringToColor(fixedColor);
    }
    else if (SNAP_START.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      snapStart = (Boolean) value;
    }
    else if (SNAP_END.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      snapEnd = (Boolean) value;
    }
    else if (REPORT.equals(key)) {
      reportFormat.setFormat((String) value);
    }
    else if (PERSISTENCE.equals(key)) {
      persistence = (String) value;
    }
    else if (PERSISTENT_ICON_NAME.equals(key)) {
      persistentIconName = (String) value;
    }
    else if (GLOBAL.equals(key)) {
      global = (String) value;
    }
    else if (ICON_NAME.equals(key)) {
      iconName = (String) value;
      super.setAttribute(ICON_NAME, iconName);
    }
    else {
      super.setAttribute(key, value);
    }
  }

  protected void setTransparency(int h) {
    if (h < 0) {
      hideOpacity = 0;
    }
    else hideOpacity = Math.min(h, 100);
  }

  @Override
  public String getAttributeValueString(String key) {
    if (DRAW_RANGE.equals(key)) {
      return String.valueOf(drawRange);
    }
    else if (RANGE_SCALE.equals(key)) {
      return String.valueOf(rangeScale);
    }
    else if (RANGE_ROUNDING.equals(key)) {
      if (rangeRounding == 1.0) {
        return ROUND_UP;
      }
      else if (rangeRounding == 0.0) {
        return ROUND_DOWN;
      }
      else {
        return ROUND_OFF;
      }
    }
    else if (HIDE_COUNTERS.equals(key)) {
      return String.valueOf(hideCounters);
    }
    else if (HIDE_OPACITY.equals(key)) {
      return String.valueOf(hideOpacity);
    }
    else if (RANGE_FOREGROUND.equals(key)) {
      return ColorConfigurer.colorToString(rangeFg);
    }
    else if (RANGE_BACKGROUND.equals(key)) {
      return ColorConfigurer.colorToString(rangeBg);
    }
    else if (LOS_COLOR.equals(key)) {
      return fixedColor;
    }
    else if (SNAP_START.equals(key)) {
      return String.valueOf(snapStart);
    }
    else if (SNAP_END.equals(key)) {
      return String.valueOf(snapEnd);
    }
    else if (REPORT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (PERSISTENCE.equals(key)) {
      return persistence;
    }
    else if (PERSISTENT_ICON_NAME.equals(key)) {
      return persistentIconName;
    }
    else if (GLOBAL.equals(key)) {
      return global;
    }
    else if (ICON_NAME.equals(key)) {
      return iconName;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public void setup(boolean show) {
    visible = false;
    initializing = false;
    persisting = false;
    mirroring = false;
    getLaunchButton().setEnabled(show);
  }

  /**
   * Reset the LOS Thread back to hidden, ready for next activation
   */
  public void reset() {
    visible = false;
    initializing = false;
    persisting = false;
    mirroring = false;
    map.setPieceOpacity(1.0f);
    map.popMouseListener(this); // Only pop the top listener if it is us
    map.repaint();
  }

  /**
   * With Global visibility, LOS_Thread now has a state that needs to be
   * communicated to clients on other machines
   */

  public String getState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(anchor.x).append(anchor.y).append(arrow.x).append(arrow.y);
    se.append(persisting);
    se.append(mirroring);
    return se.getValue();
  }

  public void setState(String state) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(state, ';');
    anchor.x = sd.nextInt(anchor.x);
    anchor.y = sd.nextInt(anchor.y);
    arrow.x = sd.nextInt(arrow.x);
    arrow.y = sd.nextInt(arrow.y);
    setPersisting(sd.nextBoolean(false));
    setMirroring(sd.nextBoolean(false));
  }

  @Override
  public void draw(Graphics g, Map m) {
    if (initializing || !visible) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    g.setColor(threadColor);
    final Point mapAnchor = map.mapToDrawing(anchor, os_scale);
    final Point mapArrow = map.mapToDrawing(arrow, os_scale);
    g.drawLine(mapAnchor.x, mapAnchor.y, mapArrow.x, mapArrow.y);

    if (drawRange) {
      if (rangeScale > 0) {
        final int dist = (int)(rangeRounding + anchor.getLocation().distance(arrow.getLocation()) / rangeScale);
        drawRange(g, dist);
      }
      else  {
        final Board b = map.findBoard(anchor);
        if (b != null) {
          MapGrid grid = b.getGrid();

          if (grid != null) {
            if (grid instanceof ZonedGrid) {
              final Point bp = new Point(anchor);
              bp.translate(-b.bounds().x, -b.bounds().y);
              final Zone z = ((ZonedGrid) b.getGrid()).findZone(bp);
              if (z != null) {
                grid = z.getGrid();
              }
            }

            if (grid != null) {
              drawRange(g, grid.range(anchor, arrow));
            }
          }
        }
      }
    }

    lastAnchor = anchor;
    lastArrow = arrow;
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  protected void launch() {
    if (!visible) {
      map.pushMouseListener(this);
      if (hideCounters) {
        map.setPieceOpacity(hideOpacity / 100.0f);
        map.repaint();
      }
      visible = true;
      anchor.move(0, 0);
      arrow.move(0, 0);
      retainAfterRelease = false;
      initializing = true;
    }
    else {
      // Launch button has been clicked while the LOS is currently visible.
      // Reset the LOS and tell other clients to do the same if this is a shared LOS.
      final Command com = new LOSCommand(this);
      com.execute();
      if (! global.equals(NEVER)) {
        GameModule.getGameModule().sendAndLog(com);
      }
    }
  }

  /**
   * Commands controlling persistence are passed between players, so LOS Threads
   * must have a unique ID.
   */
  @Override
  public void setId(String id) {
    threadId = id;
  }

  @Override
  public String getId() {
    return threadId;
  }

  /** Since we register ourselves as a MouseListener using {@link
   * Map#pushMouseListener}, these mouse events are received in map
   * coordinates */
  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (!SwingUtils.isMainMouseButtonDown(e)) {
      return;
    }

    initializing = false;
    if (visible && !persisting && !mirroring) {
      Point p = e.getPoint();
      if (Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(SNAP_LOS)) || snapStart) {
        p = map.snapTo(p);
      }
      anchor = p;
      anchorLocation = map.localizedLocationName(anchor);
      lastLocation = anchorLocation;
      lastRange = "";
      checkList.clear();
      ctrlWhenClick = SwingUtils.isSelectionToggle(e);
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    if (!SwingUtils.isMainMouseButtonDown(e)) {
      return;
    }

    if (!persisting && !mirroring) {
      if (retainAfterRelease && !(ctrlWhenClick && persistence.equals(CTRL_CLICK))) {
        retainAfterRelease = false;
        if (global.equals(ALWAYS)) {
          final Command com = new LOSCommand(this, getAnchor(), getArrow(), false, true);
          GameModule.getGameModule().sendAndLog(com);
        }
      }
      else if (e.getWhen() != lastRelease) {
        visible = false;
        if (global.equals(ALWAYS) || global.equals(WHEN_PERSISTENT)) {
          if (persistence.equals(ALWAYS) || (ctrlWhenClick && persistence.equals(CTRL_CLICK))) {
            anchor = lastAnchor;
            final Command com = new LOSCommand(this, getAnchor(), getArrow(), true, false);
            GameModule.getGameModule().sendAndLog(com);
            setPersisting(true);
          }
          else {
            final Command com = new LOSCommand(this, getAnchor(), getArrow(), false, false);
            GameModule.getGameModule().sendAndLog(com);
          }
        }
        map.setPieceOpacity(1.0f);
        map.popMouseListener();
        map.repaint();
      }
      lastRelease = e.getWhen();

      if (getLosCheckCount() > 0) {
        reportFormat.setProperty(FROM_LOCATION, anchorLocation);
        reportFormat.setProperty(TO_LOCATION, lastLocation);
        reportFormat.setProperty(RANGE, lastRange);
        reportFormat.setProperty(CHECK_COUNT, String.valueOf(getLosCheckCount()));
        reportFormat.setProperty(CHECK_LIST, getLosCheckList());

        GameModule.getGameModule().getChatter().send(reportFormat.getLocalizedText());
      }
    }
    ctrlWhenClick = false;
  }

  protected void setPersisting(boolean b) {
    persisting = b;
    visible = b;
    setMirroring(false);
    if (persisting) {
      getLaunchButton().setAttribute(ICON_NAME, persistentIconName);
    }
    else {
      getLaunchButton().setAttribute(ICON_NAME, iconName);
      map.repaint();
    }
  }

  protected boolean isPersisting() {
    return persisting;
  }

  protected void setMirroring(boolean b) {
    mirroring = b;
    if (mirroring) {
      visible = true;
    }
  }

  protected boolean isMirroring() {
    return mirroring;
  }

  protected Point getAnchor() {
    return new Point(anchor);
  }

  protected void setEndPoints(Point newAnchor, Point newArrow) {
    anchor.x = newAnchor.x;
    anchor.y = newAnchor.y;
    arrow.x = newArrow.x;
    arrow.y = newArrow.y;
    map.repaint();
  }

  protected Point getArrow() {
    return new Point(arrow);
  }

  protected int getLosCheckCount() {
    return checkList.size();
  }

  protected String getLosCheckList() {
    return StringUtils.join(checkList, ", ");
  }

  /* Since we register ourselves as a MouseMotionListener directly,
     these mouse events are received in component coordinates */
  @Override
  public void mouseMoved(MouseEvent e) {
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    if (!SwingUtils.isMainMouseButtonDown(e)) {
      return;
    }

    if (visible && !persisting && !mirroring) {
      retainAfterRelease = true;

      Point p = e.getPoint();

      map.scrollAtEdge(p, 15);

      if (Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(SNAP_LOS)) || snapEnd) {
        p = map.mapToComponent(map.snapTo(map.componentToMap(p)));
      }
      arrow = map.componentToMap(p);

      final String location = map.localizedLocationName(arrow);
      if (!checkList.contains(location) && !location.equals(anchorLocation)) {
        checkList.add(location);
        lastLocation = location;
      }

      final Point mapAnchor = lastAnchor;
      final Point mapArrow = lastArrow;
      final int fudge = (int) (1.0 / map.getZoom() * 2);
      final Rectangle r = new Rectangle(Math.min(mapAnchor.x, mapArrow.x) - fudge,
          Math.min(mapAnchor.y, mapArrow.y) - fudge,
          Math.abs(mapAnchor.x - mapArrow.x) + 1 + fudge * 2,
          Math.abs(mapAnchor.y - mapArrow.y) + 1 + fudge * 2);
      map.repaint(r);

      if (drawRange) {
        map.repaint(lastRangeRect);
      }
    }
  }

  /**
   * Writes text showing the range
   *
   * @param range the range to display, in whatever units returned
   * by the {@link MapGrid} containing the thread */
  public void drawRange(Graphics g, int range) {
    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    final Point mapArrow = map.mapToDrawing(arrow, os_scale);
    final Point mapAnchor = map.mapToDrawing(anchor, os_scale);

    g.setColor(Color.black);
    g.setFont(RANGE_FONT.deriveFont((float)(RANGE_FONT.getSize() * os_scale)));
    final FontMetrics fm = g.getFontMetrics();

    final StringBuilder buffer = new StringBuilder();
    int dummy = range;
    while (dummy >= 1) {
      dummy = dummy / 10;
      buffer.append('8');
    }
    if (buffer.length() == 0) {
      buffer.append('8');
    }
    final String rangeMsg = Resources.getString("LOS_Thread.range");

    final int wid = fm.stringWidth(" " + rangeMsg + "  " + buffer);
    final int hgt = fm.getAscent() + 2;

    final int w = mapArrow.x - mapAnchor.x;
    final int h = mapArrow.y - mapAnchor.y;
    final double hypot = Math.sqrt(w * w + h * h);
    final int x0 = mapArrow.x + (int) ((wid / 2 + 20) * w / hypot);
    final int y0 = mapArrow.y + (int) ((hgt / 2 + 20) * h / hypot);

    g.fillRect(x0 - wid / 2, y0 + hgt / 2 - fm.getAscent(), wid, hgt);
    g.setColor(Color.white);
    g.drawString(rangeMsg + " " + range,
                 x0 - wid / 2 + fm.stringWidth(" "), y0 + hgt / 2);

    // Fudge the bounding box for repaint, 50% margin on all sides.
    // Everything else is so hosed here, it's not worth sorting out more
    // precisely and the range rectangle is small anyhow.
    lastRangeRect.x = x0 - wid / 2 - (int)(wid * 0.5);
    lastRangeRect.y = y0 + hgt / 2 - fm.getAscent() - (int)(hgt * 0.5);
    lastRangeRect.width = wid * 2;
    lastRangeRect.height = hgt * 2;
    lastRangeRect = map.drawingToMap(lastRangeRect, os_scale);

    lastRange = String.valueOf(range);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.LosThread.component_type"); //$NON-NLS-1$
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "LOS"); //NON-NLS
  }


  /**
   * The attributes of an LOS_Thread are:
   * <pre>
   * <code>NAME</code>:  the name of the Preferences tab
   * <code>LABEL</code>:  the label of the button
   * <code>HOTKEY</code>:  the hotkey equivalent of the button
   * <code>DRAW_RANGE</code>:  If true, draw the distance between endpoints of the thread
   * <code>RANGE_FOREGROUND</code>:  the color of the text when drawing the distance
   * <code>RANGE_BACKGROUND</code>:  the color of the background rectangle when drawing the distance
   * <code>HIDE_COUNTERS</code>:  If true, hide all {@link GamePiece}s on the map when drawing the thread
   * </pre>
   */
  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      REPORT,
      PERSISTENCE,
      PERSISTENT_ICON_NAME,
      GLOBAL,
      SNAP_START,
      SNAP_END,
      DRAW_RANGE,
      RANGE_SCALE,
      RANGE_ROUNDING,
      HIDE_COUNTERS,
      HIDE_OPACITY,
      LOS_COLOR,
      RANGE_FOREGROUND,
      RANGE_BACKGROUND
    );
  }


  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.report_format"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.persistence"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.icon_persist"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.visible"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.start_grid"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.end_grid"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.draw_range"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.pixel_range"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.round_fractions"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.hidden"), //$NON-NLS-1$
      Resources.getString("Editor.LosThread.opacity"), //$NON-NLS-1$
      Resources.getString(Resources.COLOR_LABEL)
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      ReportFormatConfig.class,
      PersistenceOptions.class,
      IconConfig.class,
      GlobalOptions.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Integer.class,
      RoundingOptions.class,
      Boolean.class,
      Integer.class,
      Color.class
    );
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, DEFAULT_ICON);
    }
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { FROM_LOCATION, TO_LOCATION, RANGE, CHECK_COUNT, CHECK_LIST });
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    VisibilityCondition cond = null;
    if (RANGE_SCALE.equals(name)
      || RANGE_ROUNDING.equals(name)) {
      cond = () -> drawRange;
    }
    else if (HIDE_OPACITY.equals(name)) {
      cond = () -> hideCounters;
    }
    else if (PERSISTENT_ICON_NAME.equals(name)) {
      cond = () -> persistence.equals(CTRL_CLICK) || persistence.equals(ALWAYS);
    }

    return cond;
  }

  public static class RoundingOptions extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ROUND_UP, ROUND_DOWN, ROUND_OFF};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[]{
        "Editor.up",
        "Editor.down",
        "Editor.LosThread.round_off",
      };
    }
  }

  public static class PersistenceOptions extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{CTRL_CLICK, NEVER, ALWAYS};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[]{
        "Editor.LosThread.ctrl_click",
        "Editor.never",
        "Editor.always"
      };
    }
  }

  public static class GlobalOptions extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{WHEN_PERSISTENT, NEVER, ALWAYS};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[]{
        "Editor.LosThread.when_persistent",
        "Editor.never",
        "Editor.always"
      };
    }
  }

  @Override
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(LOS_THREAD_COMMAND + getId() + "\t")) {
      return null;
    }
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, '\t');
    sd.nextToken();
    sd.nextToken();
    final Point anchor = new Point(sd.nextInt(0), sd.nextInt(0));
    final Point arrow = new Point(sd.nextInt(0), sd.nextInt(0));
    final boolean persisting = sd.nextBoolean(false);
    final boolean mirroring = sd.nextBoolean(false);
    final boolean reset = sd.nextBoolean(false);
    return reset ? new LOSCommand(this) : new LOSCommand(this, anchor, arrow, persisting, mirroring);
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof LOSCommand)) {
      return null;
    }
    final LOSCommand com = (LOSCommand) c;
    final SequenceEncoder se = new SequenceEncoder(com.target.getId(), '\t');
    se
      .append(com.newAnchor.x)
      .append(com.newAnchor.y)
      .append(com.newArrow.x)
      .append(com.newArrow.y)
      .append(com.newPersisting)
      .append(com.newMirroring)
      .append(com.reset);
    return LOS_THREAD_COMMAND + se.getValue();
  }

  public static class LOSCommand extends Command {
    protected LOS_Thread target;
    protected String oldState;
    protected Point newAnchor, oldAnchor;
    protected Point newArrow, oldArrow;
    protected boolean newPersisting, oldPersisting;
    protected boolean newMirroring, oldMirroring;
    protected boolean reset;

    public LOSCommand(LOS_Thread oTarget, Point anchor, Point arrow, boolean persisting, boolean mirroring) {
      target = oTarget;
      oldAnchor = target.getAnchor();
      oldArrow = target.getArrow();
      oldPersisting = target.isPersisting();
      oldMirroring = target.isMirroring();
      newAnchor = anchor;
      newArrow = arrow;
      newPersisting = persisting;
      newMirroring = mirroring;
      reset = false;
    }

    /**
     * Constructor for a Reset command to reset the thread to inactive
     * @param oTarget target Thread
     */
    public LOSCommand(LOS_Thread oTarget) {
      reset = true;
      target = oTarget;
      oldAnchor = target.getAnchor();
      oldArrow = target.getArrow();
      oldPersisting = target.isPersisting();
      oldMirroring = target.isMirroring();
      newAnchor = new Point(0, 0);
      newArrow = new Point(0, 0);
      newPersisting = false;
      newMirroring = false;
    }

    @Override
    protected void executeCommand() {
      if (reset) {
        target.reset();
      }
      else {
        target.setEndPoints(newAnchor, newArrow);
        target.setPersisting(newPersisting);
        target.setMirroring(newMirroring);
      }
    }

    @Override
    protected Command myUndoCommand() {
      return new LOSCommand(target, oldAnchor, oldArrow, oldPersisting, oldMirroring);
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
    super.addLocalImageNames(s);
    final HTMLImageFinder h = new HTMLImageFinder(reportFormat.getFormat());
    h.addImageNames(s);
  }
}
