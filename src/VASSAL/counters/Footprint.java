/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney, Brent Easton
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.DoubleConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.tools.ImageUtils;
import VASSAL.tools.SequenceEncoder;

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
  protected List<Point> pointList = new ArrayList<Point>();

  // Type Variables (Configured in Ed)
  protected KeyStroke trailKey;                // Control Key to invoke
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
  protected int minX, minY, maxX, maxY;
  protected Rectangle myBoundingBox;
  protected Font font;
  protected double lastZoom;
  protected boolean localVisibility;

  protected double lineWidth;
  private KeyCommand showTrailCommand;

  public Footprint() {
    super(Footprint.ID, null);
  }

  public Footprint(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  /** @deprecated Use {@link #pointList} directly. */
  @Deprecated
  protected Enumeration getPointList() {
    return Collections.enumeration(pointList);
  }

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
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();

    trailKey = st.nextKeyStroke(DEFAULT_TRAIL_KEY);
    menuCommand = st.nextToken(DEFAULT_MENU_COMMAND);
    initiallyVisible = st.nextBoolean(DEFAULT_INITIALLY_VISIBLE.booleanValue());
    globallyVisible = st.nextBoolean(DEFAULT_GLOBALLY_VISIBLE.booleanValue());
    circleRadius = st.nextInt(DEFAULT_CIRCLE_RADIUS);
    fillColor = st.nextColor(DEFAULT_FILL_COLOR);
    lineColor = st.nextColor(DEFAULT_LINE_COLOR);
    selectedTransparency = st.nextInt(DEFAULT_SELECTED_TRANSPARENCY);
    unSelectedTransparency = st.nextInt(DEFULT_UNSELECTED_TRANSPARENCY);
    edgePointBuffer = st.nextInt(DEFAULT_EDGE_POINT_BUFFER);
    edgeDisplayBuffer = st.nextInt(DEFAULT_EDGE_DISPLAY_BUFFER);
    lineWidth = st.nextDouble(LINE_WIDTH);

    commands = null;
    showTrailCommand = null;

    if (initiallyVisible) {
      localVisibility = true;
      globalVisibility = true;
    }

  }

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
      .append(lineWidth);
    return ID + se.getValue();
  }

  public void setProperty(Object key, Object val) {
    if (Properties.MOVED.equals(key)) {
      setMoved(Boolean.TRUE.equals(val));
      piece.setProperty(key, val); // Pass on to MovementMarkable
    }
    else {
      super.setProperty(key, val);
    }
  }

  public Object getProperty(Object key) {
    /* If this piece has a real MovementMarkable trait,
     * use it to store the MOVED status
     */
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
  
  public void setMoved(boolean justMoved) {
    if (justMoved) {
      recordCurrentPosition();
      startMapId = getMap().getId();
    }
    else {
      clearTrail();
      myBoundingBox = null;
    }
    redraw();
  }

  protected void recordCurrentPosition() {
    final Point where = this.getPosition();
    if (pointList.isEmpty()) {
      addPoint(where);
    }
    else {
      final Point last = pointList.get(pointList.size()-1);
      if (!last.equals(where)) { // Don't add the same point twice
        addPoint(where);
      }
    }
  }

  protected void clearTrail() {
    pointList.clear();
    addPoint(getPosition());
    myBoundingBox = null;
    localVisibility = initiallyVisible;
    globalVisibility = initiallyVisible;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MovementTrail.htm");
  }

  /**
   * Add Point to list and adjust the overall boundingBox to encompass the
   * trail.
   */
  protected void addPoint(Point p) {
    pointList.add(p);

    getMyBoundingBox();

    if (p.x + circleRadius > maxX) maxX = p.x + circleRadius;
    if (p.x - circleRadius < minX) minX = p.x - circleRadius;
    if (p.y + circleRadius > maxY) maxY = p.y + circleRadius;
    if (p.y - circleRadius < minY) minY = p.y - circleRadius;

    myBoundingBox = new Rectangle(minX, minY, maxX - minX, maxY - minY);
  }

  public void redraw() {
    piece.getMap().repaint(getMyBoundingBox());
  }

  public String getDescription() {
    return "Movement trail";
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
// FIXME: This method is inefficient.
    int x1, y1, x2, y2;
    piece.draw(g, x, y, obs, zoom);
    if (getMap() != null) {
      recordCurrentPosition();
    }

    /*
     * No map, or trail not visible, do nothing!
     */
    if (getMap() == null
        || !isTrailVisible()
        || getMap().getView() != obs) {
      return;
    }

    /*
     * If we have changed Maps, then start a new trail. Note that this check is
     * here because setMoved is called before the piece has been moved.
     */
    String currentMap = getMap().getId();
    if (!currentMap.equals(startMapId)) {
      startMapId = currentMap;
      clearTrail();
      return;
    }

    /*
     * If we are asked to be drawn at a different zoom from the current map zoom
     * setting, then don't draw the trail as it will be in the wrong place.
     * (i.e. Mouse-over viewer)
     */
    double mapZoom = zoom;
    if (this.getMap() != null) {
      mapZoom = this.getMap().getZoom();
    }
    
    if (zoom != mapZoom) {
      return;
    }

    currentMap = getMap().getId();
    final Graphics2D g2d = (Graphics2D) g;
    final boolean selected = Boolean.TRUE.equals(
      Decorator.getOutermost(this).getProperty(Properties.SELECTED));
    final int transparencyPercent = Math.min(0, Math.max(100,
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
    final int mapHeight = getMap().mapSize().height;
    final int mapWidth = getMap().mapSize().width;
    final int edgeHeight =
      Integer.parseInt(getMap().getAttributeValueString(Map.EDGE_HEIGHT));
    final int edgeWidth =
      Integer.parseInt(getMap().getAttributeValueString(Map.EDGE_WIDTH));

    final int edgeClipHeight =
      (edgeHeight < edgeDisplayBuffer) ? edgeHeight : edgeDisplayBuffer;
    final int edgeClipWidth =
      (edgeWidth < edgeDisplayBuffer) ? edgeWidth : edgeDisplayBuffer;

    final int clipX = edgeWidth - edgeClipWidth;
    final int clipY = edgeHeight - edgeClipHeight;
    final int width = mapWidth - (2 * edgeWidth) + 2 * edgeClipWidth;
    final int height = mapHeight - (2 * edgeHeight) + 2 * edgeClipHeight;

    final Rectangle newClip = new Rectangle((int) (clipX * zoom),
                                            (int) (clipY * zoom),
                                            (int) (width * zoom),
                                            (int) (height * zoom));
    final Rectangle circleRect =
      new Rectangle(edgeWidth - edgePointBuffer,
                    edgeHeight - edgePointBuffer,
                    mapWidth + 2 * edgePointBuffer,
                    mapHeight + 2 * edgePointBuffer);
    final Rectangle visibleRect = getMap().getView().getVisibleRect();

    final Shape oldClip = g2d.getClip();
    g2d.setClip(newClip.intersection(visibleRect));
    if (oldClip != null) {
      g2d.setClip(oldClip.getBounds().intersection(g.getClipBounds()));
    }

    g2d.setComposite(
      AlphaComposite.getInstance(AlphaComposite.SRC_OVER, transparency));
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

    final float thickness = Math.max(1.0f,(float)(zoom*lineWidth));
    g2d.setStroke(new BasicStroke(thickness));
    g2d.setColor(lineColor);

    Point lastP = null;
    final Point here = getPosition();

    /*
     * Draw the tracks between trail points
     */
    for (Point p : pointList) {
      if (lastP != null) {
        x1 = (int) (lastP.x * zoom);
        y1 = (int) (lastP.y * zoom);
        x2 = (int) (p.x * zoom);
        y2 = (int) (p.y * zoom);

        drawTrack(g, x1, y1, x2, y2, zoom);
      }
      lastP = p;
    }

    if (lastP != null) {
      x1 = (int) (lastP.x * zoom);
      y1 = (int) (lastP.y * zoom);
      x2 = (int) (here.x * zoom);
      y2 = (int) (here.y * zoom);

      drawTrack(g, x1, y1, x2, y2, zoom);
    }

    /*
     * And draw the points themselves.
     */
    int elementCount = -1;
    for (Point p : pointList) {
      elementCount++;

      if (circleRect.contains(p) && !p.equals(here)) {
        drawPoint(g, p, zoom, elementCount);

        // Is there an Icon to draw in the circle?
        Image image = getTrailImage(elementCount);
        x1 = (int) ((p.x - circleRadius) * zoom);
        y1 = (int) ((p.y - circleRadius) * zoom);
        if (selected && image != null) {
          if (zoom == 1.0) {
            g.drawImage(image, x1, y1, obs);
          }
          else {
            Image scaled =
              ImageUtils.transform((BufferedImage) image, 0.0, zoom);
            g.drawImage(scaled, x1, y1, obs);
          }
        }

        // Or some text?
        final String text = getTrailText(elementCount);
        if (selected && text != null) {
          if (font == null || lastZoom != mapZoom) {
            x1 = (int) (p.x * zoom);
            y1 = (int) (p.y * zoom);
            final Font font =
              new Font("Dialog", Font.PLAIN, (int) (circleRadius * 1.4 * zoom));
            Labeler.drawLabel(g, text, x1, y1, font, Labeler.CENTER,
                              Labeler.CENTER, lineColor, null, null);

          }
          lastZoom = mapZoom;
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
    final int x = (int) ((p.x - circleRadius) * zoom);
    final int y = (int) ((p.y - circleRadius) * zoom);
    final int radius = (int) (circleRadius * 2 * zoom);
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
  public Rectangle boundingBox() {
    if (isTrailVisible() && getMap() != null) {
      return piece.boundingBox().union(getMyBoundingBox());
    }
    else
      return piece.boundingBox();
  }

  /**
   * Return the boundingBox including the trail
   */
  public Rectangle getMyBoundingBox() {
    if (myBoundingBox == null) {
      final Point p = piece.getPosition();
      myBoundingBox = new Rectangle(p.x, p.y, 60, 60);
      minX = myBoundingBox.x;
      minY = myBoundingBox.y;
      maxX = myBoundingBox.x + myBoundingBox.width;
      maxY = myBoundingBox.y + myBoundingBox.height;
    }
    return myBoundingBox;
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getName() {
    return piece.getName();
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      if (trailKey != null) {
        showTrailCommand = new KeyCommand(menuCommand, trailKey, Decorator.getOutermost(this), this);
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

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (showTrailCommand != null
        && showTrailCommand.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      if (globallyVisible) {
        globalVisibility = !globalVisibility;
      }
      else {
        localVisibility = !localVisibility;
      }
      redraw();
      return tracker.getChangeCommand();
    }
    return null;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }
  
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
    private HotKeyConfigurer trailKeyInput;
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
      trailKeyInput = new HotKeyConfigurer(null, "Key Command:  ", p.trailKey);
      controls.add(trailKeyInput.getControls());

      mc = new StringConfigurer(null, "Menu Command:  ", p.menuCommand);
      controls.add(mc.getControls());

      iv = new BooleanConfigurer(null, "Trails start visible?",
                                 Boolean.valueOf(p.initiallyVisible));
      controls.add(iv.getControls());

      gv = new BooleanConfigurer(null, "Trails are visible to all players?",
                                 Boolean.valueOf(p.globallyVisible));
      controls.add(gv.getControls());

      cr = new IntConfigurer(null, "Circle Radius:  ", new Integer(p.circleRadius));
      controls.add(cr.getControls());

      fc = new ColorConfigurer(null, "Circle Fill Color:  ", p.fillColor);
      controls.add(fc.getControls());

      lc = new ColorConfigurer(null, "Line Color:  ", p.lineColor);
      controls.add(lc.getControls());

      lw = new DoubleConfigurer(null,"Line thickness:  ",new Double(p.lineWidth));
      controls.add(lw.getControls());

      st = new IntConfigurer(null, "Selected Unit Trail Transparency (0-100):  ", new Integer(p.selectedTransparency));
      controls.add(st.getControls());

      ut = new IntConfigurer(null, "Unselected Unit Trail Transparency (0-100):  ", new Integer(p.unSelectedTransparency));
      controls.add(ut.getControls());

      b = Box.createHorizontalBox();
      pb = new IntConfigurer(null, "Display Trail Points Off-map for ", new Integer(p.edgePointBuffer));
      b.add(pb.getControls());
      b.add(new JLabel("pixels"));
      controls.add(b);

      b = Box.createHorizontalBox();
      db = new IntConfigurer(null, "Display Trails Off-map for  ", new Integer(p.edgeDisplayBuffer));
      b.add(db.getControls());
      b.add(new JLabel("pixels"));
      controls.add(b);
    }

    public String getState() {
      return String.valueOf(gv.booleanValue().booleanValue());
    }

    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(ID)
        .append((KeyStroke) trailKeyInput.getValue())
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
        .append(lw.getValueString());
      return se.getValue();
    }

    public Component getControls() {
      return controls;
    }
  }
}
