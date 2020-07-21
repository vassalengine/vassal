/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton, Joel Uckelman
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
package VASSAL.counters;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.image.BufferedImage;
import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.DoubleConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;

/**
 * Displays a movement trail indicating where a piece has been moved
 */
public class Footprint extends MovementMarkable {

  public static final String ID = "footprint;";
  private KeyCommand[] commands;

  // State Variables (Saved in logfile/sent to opponent)
  protected boolean globalVisibility = false;  // Shared trail visibility (if globallyVisible == true)
  protected String startMapId = "";            // Map Id trail started on
                                               // List of points
  protected List<Point> pointList = new ArrayList<>();

  // Type Variables (Configured in Ed)
  protected NamedKeyStroke trailKey;           // Control Key to invoke
  protected NamedKeyStroke trailKeyOn;         // Control Key to force trails on
  protected NamedKeyStroke trailKeyOff;        // Control Key to force trails off
  protected NamedKeyStroke trailKeyClear;      // Control Key to force trails clear
  protected String menuCommand;                // Menu Command
  protected boolean initiallyVisible = false;  // Are Trails initially visible?
  protected boolean globallyVisible = false;   // Are Trails shared between players?
  protected int circleRadius;                  // Radius of trail point circle
  protected int selectedTransparency;          // Transparency of trail when unit is selected
  protected int unSelectedTransparency;        // Transparency of trail when unit is selected/unselected
  protected Color lineColor;                   // Color of Trail lines
  protected Color fillColor;                   // Color of Trail circle fill
  protected int edgePointBuffer;               // How far Off-map to draw trail points (pixels)?
  protected int edgeDisplayBuffer;             // How far Off-map to draw trail lines (pixels)?

  // Defaults for Type variables
  protected static final char DEFAULT_TRAIL_KEY = 'T';
  protected static final String DEFAULT_MENU_COMMAND = "Movement Trail";
  protected static final Boolean DEFAULT_INITIALLY_VISIBLE = Boolean.FALSE;
  protected static final Boolean DEFAULT_GLOBALLY_VISIBLE = Boolean.FALSE;
  protected static final int DEFAULT_CIRCLE_RADIUS = 10;
  protected static final Color DEFAULT_FILL_COLOR = Color.WHITE;
  protected static final Color DEFAULT_LINE_COLOR = Color.BLACK;
  protected static final int DEFAULT_SELECTED_TRANSPARENCY = 100;
  protected static final int DEFULT_UNSELECTED_TRANSPARENCY = 50;
  protected static final int DEFAULT_EDGE_POINT_BUFFER = 20;
  protected static final int DEFAULT_EDGE_DISPLAY_BUFFER = 30;
  protected static final float LINE_WIDTH = 1.0f;

  // Local Variables
  protected Rectangle myBoundingBox;
  protected Font font;
  protected double lastZoom;
  protected boolean localVisibility;
  protected boolean initialized = false; // Protect against multiple re-initializations

  protected double lineWidth;
  private KeyCommand showTrailCommand;
  private KeyCommand showTrailCommandOn;      // Commands to force specific trail states
  private KeyCommand showTrailCommandOff;
  private KeyCommand showTrailCommandClear;


  public Footprint() {
    super(Footprint.ID, null);
  }

  public Footprint(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  /** @deprecated Use {@link #pointList} directly. */
  @Deprecated
  protected Enumeration<Point> getPointList() {
    return Collections.enumeration(pointList);
  }

  @Override
  public void mySetState(String newState) {
    pointList.clear();
    final SequenceEncoder.Decoder ss =
      new SequenceEncoder.Decoder(newState, ';');
    globalVisibility = ss.nextBoolean(initiallyVisible);
    startMapId = ss.nextToken("");
    final int items = ss.nextInt(0);
    for (int i = 0; i < items; i++) {
      final String point = ss.nextToken("");
      if (point.length() != 0) {
        final SequenceEncoder.Decoder sp =
          new SequenceEncoder.Decoder(point, ',');
        final int x = sp.nextInt(0);
        final int y = sp.nextInt(0);
        pointList.add(new Point(x, y));
      }
    }
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(globalVisibility)
      .append(startMapId)
      .append(pointList.size());

    for (Point p : pointList) {
      se.append(p.x + "," + p.y);
    }

    return se.getValue();
  }

  /**
   * Type is the character command that toggles footprint visiblity
   */
  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();

    trailKey = st.nextNamedKeyStroke(DEFAULT_TRAIL_KEY);
    menuCommand = st.nextToken(DEFAULT_MENU_COMMAND);
    initiallyVisible = st.nextBoolean(DEFAULT_INITIALLY_VISIBLE);
    globallyVisible = st.nextBoolean(DEFAULT_GLOBALLY_VISIBLE);
    circleRadius = st.nextInt(DEFAULT_CIRCLE_RADIUS);
    fillColor = st.nextColor(DEFAULT_FILL_COLOR);
    lineColor = st.nextColor(DEFAULT_LINE_COLOR);
    selectedTransparency = st.nextInt(DEFAULT_SELECTED_TRANSPARENCY);
    unSelectedTransparency = st.nextInt(DEFULT_UNSELECTED_TRANSPARENCY);
    edgePointBuffer = st.nextInt(DEFAULT_EDGE_POINT_BUFFER);
    edgeDisplayBuffer = st.nextInt(DEFAULT_EDGE_DISPLAY_BUFFER);
    lineWidth = st.nextDouble(LINE_WIDTH);
    trailKeyOn = st.nextNamedKeyStroke(NamedKeyStroke.getNamedKeyStroke(DEFAULT_TRAIL_KEY, InputEvent.CTRL_MASK + InputEvent.SHIFT_MASK));
    trailKeyOff = st.nextNamedKeyStroke(NamedKeyStroke.getNamedKeyStroke(DEFAULT_TRAIL_KEY, InputEvent.CTRL_MASK + InputEvent.ALT_MASK));
    trailKeyClear = st.nextNamedKeyStroke(NamedKeyStroke.getNamedKeyStroke(DEFAULT_TRAIL_KEY, InputEvent.CTRL_MASK + InputEvent.SHIFT_MASK + InputEvent.ALT_MASK));

    commands = null;
    showTrailCommand = null;
    showTrailCommandOn = null;
    showTrailCommandOff = null;
    showTrailCommandClear = null;

    if (initiallyVisible) {
      localVisibility = true;
      globalVisibility = true;
    }
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(trailKey)
      .append(menuCommand)
      .append(initiallyVisible)
      .append(globallyVisible)
      .append(circleRadius)
      .append(fillColor)
      .append(lineColor)
      .append(selectedTransparency)
      .append(unSelectedTransparency)
      .append(edgePointBuffer)
      .append(edgeDisplayBuffer)
      .append(lineWidth)
      .append(trailKeyOn)
      .append(trailKeyOff)
      .append(trailKeyClear);
    return ID + se.getValue();
  }

  @Override
  public void setProperty(Object key, Object val) {
    if (Properties.MOVED.equals(key)) {
      setMoved(Boolean.TRUE.equals(val));
      piece.setProperty(key, val); // Pass on to MovementMarkable
      myBoundingBox = null;
    }
    else {
      super.setProperty(key, val);
    }
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (Properties.MOVED.equals(key)) {
      final Object value = piece.getProperty(key);
      return value == null ? super.getProperty(key) : value;
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public Object getProperty(Object key) {
    // If this piece has a real MovementMarkable trait,
    // use it to store the MOVED status
    if (Properties.MOVED.equals(key)) {
      final Object value = piece.getProperty(key);
      return value == null ? super.getProperty(key) : value;
    }
    return super.getProperty(key);
  }

  /**
   * setMoved is called with an argument of true each time the piece is moved.
   * The argument is false when the unit is marked as not moved.
   */
  @Override
  public void setMoved(boolean justMoved) {
    if (justMoved) {
      recordCurrentPosition();
      final Map map = getMap();
      startMapId = map != null ? map.getId() : null;
    }
    else {
      clearTrail();
    }
    redraw();
  }

  protected void recordCurrentPosition() {
    final Point here = this.getPosition();
    if (pointList.isEmpty() ||
        !pointList.get(pointList.size()-1).equals(here)) {
      addPoint(here);
    }
    else {
      myBoundingBox = null;
    }
  }

  protected void clearTrail() {
    pointList.clear();
    addPoint(getPosition());
    if (!initialized) {       //BR// Bug 12980 - prevent multiple re-initializations
      localVisibility = initiallyVisible;
      globalVisibility = initiallyVisible;
      initialized = true;
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MovementTrail.htm");
  }

  /**
   * Add Point to list and adjust the overall boundingBox to encompass the
   * trail.
   */
  protected void addPoint(Point p) {
    pointList.add(p);
    myBoundingBox = null;
  }

  private Rectangle getBB() {
    final Rectangle bb = piece.boundingBox();
    final Point pos = piece.getPosition();

    bb.x += pos.x;
    bb.y += pos.y;

    final int circleDiameter = 2*circleRadius;
    final Rectangle pr = new Rectangle();

    for (final Point p: pointList) {
      pr.setBounds(
        p.x - circleRadius, p.y - circleRadius, circleDiameter, circleDiameter
      );
      bb.add(pr);
    }

    bb.x -= pos.x;
    bb.y -= pos.y;

    return bb;
  }

  public void redraw() {
    final Map m = getMap();
    if (m != null) {
      m.repaint(getMyBoundingBox());
    }
  }

  @Override
  public String getDescription() {
    return "Movement trail";
  }

// FIXME: This method is inefficient.
  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);

    final Map map = getMap();

    // Do nothing when piece is not on a map, we are drawing the map
    // to something other than its normal view, or the trail is invisible,
    if (map == null || map.getView() != obs || !isTrailVisible()) {
      return;
    }

    /*
     * If we have changed Maps, then start a new trail. Note that this check is
     * here because setMoved is called before the piece has been moved.
     */
    final String currentMap = map.getId();
    if (!currentMap.equals(startMapId)) {
      startMapId = currentMap;
      clearTrail();
      return;
    }

    // Anything to draw?
    if (pointList.isEmpty()) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    /*
     * If we are asked to be drawn at a different zoom from the current map zoom
     * setting, then don't draw the trail as it will be in the wrong place.
     * (i.e. Mouse-over viewer)
     */
    if (zoom != map.getZoom() * os_scale) {
      return;
    }

    final boolean selected = Boolean.TRUE.equals(
      Decorator.getOutermost(this).getProperty(Properties.SELECTED));
    final int transparencyPercent = Math.max(0, Math.min(100,
      selected ? selectedTransparency : unSelectedTransparency));
    final float transparency = transparencyPercent / 100.0f;
    final Composite oldComposite = g2d.getComposite();
    final Stroke oldStroke = g2d.getStroke();
    final Color oldColor = g2d.getColor();

    /*
     * newClip is an overall clipping region made up of the Map itself and a
     * border of edgeDisplayBuffer pixels. No drawing at all outside this area.
     * mapRect is made of the Map and a edgePointBuffer pixel border. Trail
     * points are not drawn outside this area.
     */
    final Dimension mapsize = map.mapSize();
    final int mapHeight = mapsize.height;
    final int mapWidth = mapsize.width;

    final int edgeHeight =
      Integer.parseInt(map.getAttributeValueString(Map.EDGE_HEIGHT));
    final int edgeWidth =
      Integer.parseInt(map.getAttributeValueString(Map.EDGE_WIDTH));

    final int edgeClipHeight = Math.min(edgeHeight, edgeDisplayBuffer);
    final int edgeClipWidth = Math.min(edgeWidth, edgeDisplayBuffer);

    final int clipX = edgeWidth - edgeClipWidth;
    final int clipY = edgeHeight - edgeClipHeight;
    final int width = mapWidth - 2*(edgeWidth + edgeClipWidth);
    final int height = mapHeight - 2*(edgeHeight + edgeClipHeight);

    Rectangle newClip = new Rectangle(
      (int) (clipX * zoom),
      (int) (clipY * zoom),
      (int) (width * zoom),
      (int) (height * zoom)
    );

    final Rectangle visibleRect =
      map.componentToDrawing(map.getView().getVisibleRect(), os_scale);

    final Shape oldClip = g2d.getClip();

    newClip = newClip.intersection(visibleRect);
    if (oldClip != null) {
      newClip = oldClip.getBounds().intersection(newClip);
    }
    g2d.setClip(newClip);

    g2d.setComposite(
      AlphaComposite.getInstance(AlphaComposite.SRC_OVER, transparency));
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

    final float thickness = Math.max(1.0f, (float)(zoom*lineWidth));
    g2d.setStroke(new BasicStroke(thickness));
    g2d.setColor(lineColor);

    final Rectangle circleRect = new Rectangle(
      edgeWidth - edgePointBuffer,
      edgeHeight - edgePointBuffer,
      mapWidth + 2 * edgePointBuffer,
      mapHeight + 2 * edgePointBuffer
    );

    /*
     * Draw the tracks between trail points
     */
    int x1, y1, x2, y2;
    final Iterator<Point> i = pointList.iterator();
    Point cur = i.next(), next;
    while (i.hasNext()) {
      next = i.next();

      x1 = (int)(cur.x * zoom);
      y1 = (int)(cur.y * zoom);
      x2 = (int)(next.x * zoom);
      y2 = (int)(next.y * zoom);

      drawTrack(g, x1, y1, x2, y2, zoom);

      cur = next;
    }

    final Point here = getPosition();
    if (!here.equals(cur)) {
      x1 = (int)(cur.x * zoom);
      y1 = (int)(cur.y * zoom);
      x2 = (int)(here.x * zoom);
      y2 = (int)(here.y * zoom);

      drawTrack(g, x1, y1, x2, y2, zoom);
    }

    /*
     * And draw the points themselves.
     */
    int elementCount = -1;
    for (final Point p : pointList) {
      ++elementCount;

      if (circleRect.contains(p) && !p.equals(here)) {
        drawPoint(g, p, zoom, elementCount);

        // Is there an Icon to draw in the circle?
        Image image = getTrailImage(elementCount);
        x1 = (int)((p.x - circleRadius) * zoom);
        y1 = (int)((p.y - circleRadius) * zoom);
        if (selected && image != null) {
          if (zoom == 1.0) {
            g.drawImage(image, x1, y1, obs);
          }
          else {
            Image scaled =
              ImageUtils.transform((BufferedImage) image, zoom, 0.0);
            g.drawImage(scaled, x1, y1, obs);
          }
        }

        // Or some text?
        final String text = getTrailText(elementCount);
        if (selected && text != null) {
          if (font == null || lastZoom != zoom) {
            x1 = (int)(p.x * zoom);
            y1 = (int)(p.y * zoom);
            final Font font =
              new Font("Dialog", Font.PLAIN, (int)(circleRadius * 1.4 * zoom));
            Labeler.drawLabel(g, text, x1, y1, font, Labeler.CENTER,
                              Labeler.CENTER, lineColor, null, null);

          }
          lastZoom = zoom;
        }
      }
    }

    g2d.setComposite(oldComposite);
    g2d.setStroke(oldStroke);
    g2d.setColor(oldColor);
    g.setClip(oldClip);
  }

  /**
   * Draw a Circle at the given point.
   * Override this method to do something different (eg. display an Icon)
   */
  protected void drawPoint(Graphics g, Point p, double zoom, int elementCount) {
    final int x = (int)((p.x - circleRadius) * zoom);
    final int y = (int)((p.y - circleRadius) * zoom);
    final int radius = (int)(2 * circleRadius * zoom);
    g.setColor(fillColor);
    g.fillOval(x, y, radius, radius);
    g.setColor(lineColor);
    g.drawOval(x, y, radius, radius);
  }

  /**
   * Draw a track from one Point to another.
   * Don't draw under the circle as it shows
   * through with transparency turned on.
   */
  protected void drawTrack(Graphics g, int x1, int y1, int x2, int y2, double zoom) {
    double lastSqrt = -1;
    int lastDistSq = -1;

    int distSq = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1);
    if (distSq != lastDistSq) {
      lastDistSq = distSq;
      lastSqrt = Math.sqrt(distSq);
    }

    final int xDiff = (int) ((circleRadius * (x2 - x1) * zoom) / lastSqrt);
    final int yDiff = (int) ((circleRadius * (y2 - y1) * zoom) / lastSqrt);

    g.drawLine(x1 + xDiff, y1 + yDiff, x2 - xDiff, y2 - yDiff);
  }

  /**
   * Override this method to return an Image to display within each trail circle
   */
  protected Image getTrailImage(int elementCount) {
    return null;
  }

  /**
   * Override this method to return text to display within each trail circle.
   * Note, there will normally be only room for 1 character.
   */
  protected String getTrailText(int elementCount) {
    return null;
  }

  /**
   * Global Visibility means all players see the same trail
   * Local Visibility means each player controls their own trail visibility
   */
  protected boolean isTrailVisible() {
    if (globallyVisible) {
      return globalVisibility || trailKey == null;
    }
    else {
      return localVisibility || trailKey == null;
    }
  }

  /**
   * Return a bounding box covering the whole trail if it is visible, otherwise
   * just return the standard piece bounding box
   */
  @Override
  public Rectangle boundingBox() {
    return isTrailVisible() && getMap() != null ?
      new Rectangle(getMyBoundingBox()) : piece.boundingBox();
  }

  /**
   * Return the boundingBox including the trail
   */
  public Rectangle getMyBoundingBox() {
    if (myBoundingBox == null) {
      final Rectangle bb = piece.boundingBox();
      final Point pos = piece.getPosition();

      bb.x += pos.x;
      bb.y += pos.y;

      final int circleDiameter = 2*circleRadius;
      final Rectangle pr = new Rectangle();

      for (final Point p: pointList) {
        pr.setBounds(
          p.x - circleRadius, p.y - circleRadius, circleDiameter, circleDiameter
        );
        bb.add(pr);
      }

      bb.x -= pos.x;
      bb.y -= pos.y;

      myBoundingBox = bb;
    }
    return myBoundingBox;
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      if (trailKey != null && ! trailKey.isNull()) {
        showTrailCommand = new KeyCommand(menuCommand, trailKey, Decorator.getOutermost(this), this);
      }

      // Key commands to force trails to specific states
      if (trailKeyOn != null && !trailKeyOn.isNull( )) {
        showTrailCommandOn = new KeyCommand("", trailKeyOn, Decorator.getOutermost(this), this);
      }
      if (trailKeyOff != null && !trailKeyOff.isNull( )) {
        showTrailCommandOff = new KeyCommand("", trailKeyOff, Decorator.getOutermost(this), this);
      }
      if (trailKeyClear != null && !trailKeyClear.isNull()) {
        showTrailCommandClear = new KeyCommand("", trailKeyClear, Decorator.getOutermost(this), this);
      }

      if (showTrailCommand != null
          && menuCommand.length() > 0) {
        commands = new KeyCommand[]{showTrailCommand};
      }
      else {
        commands = new KeyCommand[0];
      }
    }
    if (showTrailCommand != null) {
      showTrailCommand.setEnabled(getMap() != null);
    }
    return commands;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (showTrailCommand != null
        && showTrailCommand.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      initialized = true;
      if (globallyVisible) {
        globalVisibility = !globalVisibility;
      }
      else {
        localVisibility = !localVisibility;
      }
      redraw();
      return tracker.getChangeCommand();
    }

    // These two if blocks allow forcing the trails to a specified on or off state - much easier for a "global trails" function to keep track of.
    if (showTrailCommandOn != null && showTrailCommandOn.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      initialized = true;
      if (globallyVisible) {
        globalVisibility = true;
      }
      else {
        localVisibility = true;
      }
      redraw();
      return tracker.getChangeCommand();
    }

    if (showTrailCommandOff != null && showTrailCommandOff.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      initialized = true;
      if (globallyVisible) {
        globalVisibility = false;
      }
      else {
        localVisibility = false;
      }
      redraw();
      return tracker.getChangeCommand();
    }

    // Clears the movement trail history (without being forced to do any other stuff)
    if (showTrailCommandClear != null && showTrailCommandClear.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      clearTrail();
      return tracker.getChangeCommand();
    }

    return null;
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    final PieceI18nData data = super.getI18nData();
    data.add(menuCommand, "Show Movement Trail command");
    return data;
  }

  /**
   * Key Command Global Visibility Circle Radius Fill Color Line Color Selected
   * Transparency Unselected Transparency Edge Buffer Display Limit Edge Buffer
   * Point Limit
   */
  protected static class Ed implements PieceEditor {
    private NamedHotKeyConfigurer trailKeyInput;
    private NamedHotKeyConfigurer trailKeyOn;
    private NamedHotKeyConfigurer trailKeyOff;
    private NamedHotKeyConfigurer trailKeyClear;
    private JPanel controls;
    private StringConfigurer mc;
    private BooleanConfigurer iv;
    private BooleanConfigurer gv;
    private IntConfigurer cr;
    private ColorConfigurer fc;
    private ColorConfigurer lc;
    private IntConfigurer st;
    private IntConfigurer ut;
    private IntConfigurer pb;
    private IntConfigurer db;
    private DoubleConfigurer lw;

    public Ed(Footprint p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      Box b;
      trailKeyInput = new NamedHotKeyConfigurer(null, "Key Command:  ", p.trailKey);
      controls.add(trailKeyInput.getControls());

      mc = new StringConfigurer(null, "Menu Command:  ", p.menuCommand);
      controls.add(mc.getControls());

      // Trail forcing commands
      trailKeyOn = new NamedHotKeyConfigurer(null, "TURN ON Key Command:  ", p.trailKeyOn);
      controls.add(trailKeyOn.getControls());
      trailKeyOff = new NamedHotKeyConfigurer(null, "TURN OFF Key Command:  ", p.trailKeyOff);
      controls.add(trailKeyOff.getControls());
      trailKeyClear = new NamedHotKeyConfigurer(null, "CLEAR TRAIL Key Command:  ", p.trailKeyClear);
      controls.add(trailKeyClear.getControls());

      iv = new BooleanConfigurer(null, "Trails start visible?",
                                 Boolean.valueOf(p.initiallyVisible));
      controls.add(iv.getControls());

      gv = new BooleanConfigurer(null, "Trails are visible to all players?",
                                 Boolean.valueOf(p.globallyVisible));
      controls.add(gv.getControls());

      cr = new IntConfigurer(null, "Circle Radius:  ", p.circleRadius);
      controls.add(cr.getControls());

      fc = new ColorConfigurer(null, "Circle Fill Color:  ", p.fillColor);
      controls.add(fc.getControls());

      lc = new ColorConfigurer(null, "Line Color:  ", p.lineColor);
      controls.add(lc.getControls());

      lw = new DoubleConfigurer(null,"Line thickness:  ", p.lineWidth);
      controls.add(lw.getControls());

      st = new IntConfigurer(null, "Selected Unit Trail Transparency (0-100):  ", p.selectedTransparency);
      controls.add(st.getControls());

      ut = new IntConfigurer(null, "Unselected Unit Trail Transparency (0-100):  ", p.unSelectedTransparency);
      controls.add(ut.getControls());

      b = Box.createHorizontalBox();
      pb = new IntConfigurer(null, "Display Trail Points Off-map for ", p.edgePointBuffer);
      b.add(pb.getControls());
      b.add(new JLabel("pixels"));
      controls.add(b);

      b = Box.createHorizontalBox();
      db = new IntConfigurer(null, "Display Trails Off-map for  ", p.edgeDisplayBuffer);
      b.add(db.getControls());
      b.add(new JLabel("pixels"));
      controls.add(b);
    }

    @Override
    public String getState() {
      return String.valueOf(gv.booleanValue().booleanValue());
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(ID)
        .append(trailKeyInput.getValueString())
        .append(mc.getValueString())
        .append(iv.getValueString())
        .append(gv.getValueString())
        .append(cr.getValueString())
        .append(fc.getValueString())
        .append(lc.getValueString())
        .append(st.getValueString())
        .append(ut.getValueString())
        .append(pb.getValueString())
        .append(db.getValueString())
        .append(lw.getValueString())
        .append(trailKeyOn.getValueString())
        .append(trailKeyOff.getValueString())
        .append(trailKeyClear.getValueString());
      return se.getValue();
    }

    @Override
    public Component getControls() {
      return controls;
    }
  }
}
