/*
 * $Id$
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
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.tools.LaunchButton;

/**
 * A class that allows the user to draw a straight line on a Map (LOS
 * = Line Of Sight).  No automatic detection of obstacles is
 * performed; the user must simply observe the thread against the
 * image of the map.  However, if the user clicks on a board with a
 * {@link Map Grid}, the thread may snap to the grid and report the
 * distance between endpoints of the line
 * */
public class LOS_Thread extends AbstractConfigurable implements
    MouseListener, MouseMotionListener,
    Drawable, Configurable {
  public static final String SNAP_LOS = "snapLOS";
  public static final String LOS_COLOR = "threadColor";
  public static final String HOTKEY = "hotkey";
  public static final String TOOLTIP = "tooltip";
  public static final String ICON_NAME = "iconName";
  public static final String LABEL = "label";
  public static final String DRAW_RANGE = "drawRange";
  public static final String HIDE_COUNTERS = "hideCounters";
  public static final String HIDE_OPACITY = "hideOpacity";
  public static final String RANGE_BACKGROUND = "rangeBg";
  public static final String RANGE_FOREGROUND = "rangeFg";
  public static final String RANGE_SCALE = "scale";
  public static final String RANGE_ROUNDING = "round";
  public static final String ROUND_UP = "Up";
  public static final String ROUND_DOWN = "Down";
  public static final String ROUND_OFF = "Nearest whole number";
  public static Font RANGE_FONT = new Font("Dialog", 0, 11);
  public static final String DEFAULT_ICON = "/images/thread.gif";

  protected boolean retainAfterRelease = false;
  protected long lastRelease = 0;

  protected Map map;
  protected LaunchButton launch;
  protected KeyStroke hotkey;
  protected Point anchor;
  protected Point arrow;
  protected boolean visible;
  protected boolean drawRange;
  protected int rangeScale;
  protected double rangeRounding=0.5;
  protected boolean hideCounters;
  protected int hideOpacity = 0;
  protected String fixedColor;
  protected Color threadColor = Color.black, rangeFg = Color.white, rangeBg = Color.black;

  public LOS_Thread() {
    anchor = new Point(0, 0);
    arrow = new Point(0, 0);
    visible = false;
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
    launch = new LaunchButton("Thread", TOOLTIP, LABEL, HOTKEY, ICON_NAME, al);
    launch.setAttribute(ICON_NAME, DEFAULT_ICON);
    launch.setAttribute(TOOLTIP, "Show LOS Thread");
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
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getView().addMouseMotionListener(this);
    map.addDrawComponent(this);
    map.getToolBar().add(launch);
    GameModule.getGameModule().getPrefs().addOption
        (getAttributeValueString(LABEL),
         new BooleanConfigurer(SNAP_LOS, "Snap Thread to grid"));
    if (fixedColor == null) {
      ColorConfigurer config = new ColorConfigurer(LOS_COLOR, "Thread Color");
      GameModule.getGameModule().getPrefs().addOption
          (getAttributeValueString(LABEL), config);
      config.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          threadColor = (Color) evt.getNewValue();
        }
      });
      config.fireUpdate();
    }
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.removeDrawComponent(this);
    map.getToolBar().remove(launch);
  }

  /**
   * The attributes of an LOS_Thread are:
   * <pre>
   * <code>LABEL</code>:  the label of the button
   * <code>HOTKEY</code>:  the hotkey equivalent of the button
   * <code>DRAW_RANGE</code>:  If true, draw the distance between endpoints of the thread
   * <code>RANGE_FOREGROUND</code>:  the color of the text when drawing the distance
   * <code>RANGE_BACKGROUND</code>:  the color of the background rectangle when drawing the distance
   * <code>HIDE_COUNTERS</code>:  If true, hide all {@link GamePiece}s on the map when drawing the thread
   * </pre>
   */
  public String[] getAttributeNames() {
    return new String[]{TOOLTIP, LABEL, ICON_NAME, HOTKEY, DRAW_RANGE, RANGE_SCALE, RANGE_ROUNDING, HIDE_COUNTERS, HIDE_OPACITY, LOS_COLOR, RANGE_FOREGROUND, RANGE_BACKGROUND};
  }

  public void setAttribute(String key, Object value) {
    if (DRAW_RANGE.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      drawRange = ((Boolean) value).booleanValue();
    }
    else if (RANGE_SCALE.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      rangeScale = ((Integer) value).intValue();
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
        value = new Boolean((String) value);
      }
      hideCounters = ((Boolean) value).booleanValue();
    }
    else if (HIDE_OPACITY.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      setTransparency (((Integer) value).intValue());
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
    else {
      launch.setAttribute(key, value);
    }
  }


  protected void setTransparency(int h) {
    if (h < 0) {
      hideOpacity = 0;
    }
    else if (h > 100) {
      hideOpacity = 100;
    }
    else {
      hideOpacity = h;
    }
  }

  public String getAttributeValueString(String key) {
    if (DRAW_RANGE.equals(key)) {
      return "" + drawRange;
    }
    else if (RANGE_SCALE.equals(key)) {
      return "" + rangeScale;
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
      return "" + hideCounters;
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
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public void setup(boolean show) {
    launch.setEnabled(show);
  }

  public void draw(java.awt.Graphics g, Map m) {
    if (!visible) {
      return;
    }
    g.setColor(threadColor);
    Point mapAnchor = map.componentCoordinates(anchor);
    Point mapArrow = map.componentCoordinates(arrow);
    g.drawLine(mapAnchor.x, mapAnchor.y, mapArrow.x, mapArrow.y);
    Board b;
    if (drawRange) {
      if (rangeScale > 0) {
        int dist = (int)(rangeRounding + anchor.getLocation().distance(arrow.getLocation())/rangeScale);
        drawRange(g, dist);
      }
      else if ((b = map.findBoard(anchor)) != null
        && b.getGrid() != null) {
        drawRange(g, b.getGrid().range(anchor, arrow));
      }
    }
  }

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
    }
  }

  /** Since we register ourselves as a MouseListener using {@link
   * Map#pushMouseListener}, these mouse events are received in map
   * coordinates */
  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
    if (visible) {
      Point p = e.getPoint();
      if (Boolean.TRUE.equals
          (GameModule.getGameModule().getPrefs().getValue(SNAP_LOS))) {
        p = map.snapTo(p);
      }
      anchor = p;
    }
  }

  public void mouseReleased(MouseEvent e) {
    if (retainAfterRelease) {
      retainAfterRelease = false;
    }
    else if (e.getWhen() != lastRelease) {
      visible = false;
      //map.setPiecesVisible(true);
      map.setPieceOpacity(1.0f);
      map.popMouseListener();
      map.repaint();
    }
    lastRelease = e.getWhen();
  }

  /** Since we register ourselves as a MouseMotionListener directly,
   * these mouse events are received in component
   * coordinates */
  public void mouseMoved(MouseEvent e) {
  }

  public void mouseDragged(MouseEvent e) {
    if (visible) {
      retainAfterRelease = true;

      Point p = e.getPoint();
      if (Boolean.TRUE.equals
          (GameModule.getGameModule().getPrefs().getValue(SNAP_LOS))) {
        p = map.componentCoordinates(map.snapTo(map.mapCoordinates(p)));
      }
      arrow = map.mapCoordinates(p);

      map.repaint();
    }
  }

  /**
   * Writes text showing the range
   *
   * @param range the range to display, in whatever units returned
   * by the {@link MapGrid} containing the thread */
  public void drawRange(Graphics g, int range) {
    Point mapArrow = map.componentCoordinates(arrow);
    Point mapAnchor = map.componentCoordinates(anchor);
    g.setColor(Color.black);
    g.setFont(RANGE_FONT);
    FontMetrics fm = g.getFontMetrics();
    StringBuffer buffer = new StringBuffer();
    int dummy = range;
    while (dummy >= 1) {
      dummy = dummy / 10;
      buffer.append("8");
    }
    if (buffer.length() == 0) {
      buffer.append("8");
    }
    int wid = fm.stringWidth(" Range  "+buffer.toString());
    int hgt = fm.getAscent() + 2;
    int w = mapArrow.x - mapAnchor.x;
    int h = mapArrow.y - mapAnchor.y;
    int x0 = mapArrow.x + (int) ((wid / 2 + 20) * w / Math.sqrt(w * w + h * h));
    int y0 = mapArrow.y + (int) ((hgt / 2 + 20) * h / Math.sqrt(w * w + h * h));
    g.fillRect(x0 - wid / 2, y0 + hgt / 2 - fm.getAscent(), wid, hgt);
    g.setColor(Color.white);
    g.drawString("Range " + range,
                 x0 - wid / 2 + fm.stringWidth(" "), y0 + hgt / 2);
  }

  public static String getConfigureTypeName() {
    return "Line of Sight Thread";
  }

  public String getConfigureName() {
    return null;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#LOS");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Tooltip text",
                        "Button text",
                        "Button Icon",
                        "Hotkey",
                        "Draw Range",
                        "Pixels per range unit",
                        "Round fractions",
                        "Hide Pieces while drawing",
                        "Opacity of hidden pieces (0-100%)",
                        "Thread color"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
        			   String.class,
                       IconConfig.class,
        			   KeyStroke.class,
                       Boolean.class,
                       Integer.class,
                       RoundingOptions.class,
                       Boolean.class,
                       Integer.class,
                       Color.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, DEFAULT_ICON);
    }
  }
  public VisibilityCondition getAttributeVisibility(String name) {
    VisibilityCondition cond = null;
    if (RANGE_SCALE.equals(name)
      || RANGE_ROUNDING.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return drawRange;
        }
      };
    } else if (HIDE_OPACITY.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return hideCounters;
        }
      };
    }
      
    return cond;
  }

  public static class RoundingOptions extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ROUND_UP, ROUND_DOWN, ROUND_OFF};
    }
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
}
