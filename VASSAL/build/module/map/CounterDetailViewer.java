/*
 * $Id$
 *
 * Copyright (c) 2003-2006 by David Sullivan, Rodney Kinney and Brent Easton.
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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.SumProperties;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Labeler;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;

/**
 * This is a {@link Drawable}class that draws the counters horizontally when
 * the mouse is held over a stack with the control key down.
 * 
 * @author David Sullivan
 * @version 1.0
 */
public class CounterDetailViewer extends AbstractConfigurable implements Drawable, MouseMotionListener, MouseListener, Runnable, KeyListener {

  public static final String LATEST_VERSION = "2";
  public static final String USE_KEYBOARD = "ShowCounterDetails";
  public static final String PREFERRED_DELAY = "PreferredDelay";

  public static final String DELAY = "delay";
  public static final String ALWAYS_SHOW_LOC = "alwaysshowloc";
  public static final String DRAW_PIECES = "showgraph";
  public static final String GRAPH_SINGLE_DEPRECATED = "showgraphsingle";
  public static final String MINIMUM_DISPLAYABLE = "minDisplayPieces";
  public static final String HOTKEY = "hotkey";

  public static final String SHOW_TEXT = "showtext";
  public static final String SHOW_TEXT_SINGLE_DEPRECATED = "showtextsingle";
  public static final String ZOOM_LEVEL = "zoomlevel";
  public static final String DRAW_PIECES_AT_ZOOM = "graphicsZoom";
  public static final String BORDER_WIDTH = "borderWidth";
  public static final String SHOW_NOSTACK = "showNoStack";
  public static final String SHOW_DECK = "showDeck";
  public static final String UNROTATE_PIECES = "unrotatePieces";
  public static final String DISPLAY = "display";
  public static final String LAYER_LIST = "layerList";
  public static final String SUMMARY_REPORT_FORMAT = "summaryReportFormat";
  public static final String COUNTER_REPORT_FORMAT = "counterReportFormat";
  public static final String EMPTY_HEX_REPORT_FORMAT = "emptyHexReportForma";
  public static final String VERSION = "version";
  public static final String FG_COLOR = "fgColor";
  public static final String BG_COLOR = "bgColor";
  public static final String FONT_SIZE = "fontSize";
  public static final String PROPERTY_FILTER = "propertyFilter";

  public static final String TOP_LAYER = "from top-most layer only";
  public static final String ALL_LAYERS = "from all layers";
  public static final String INC_LAYERS = "from listed layers only";
  public static final String EXC_LAYERS = "from layers other than those listed";
  public static final String FILTER = "by using a property filter";

  public static final String SUM = "sum(propertyName)";

  protected KeyStroke hotkey = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, InputEvent.CTRL_MASK);
  protected Map map;
  protected Thread delayThread;
  protected int delay = 700;
  protected long expirationTime;
  protected boolean graphicsVisible = false;
  protected boolean textVisible = false;
  protected MouseEvent currentMousePosition;

  protected int minimumDisplayablePieces = 2;
  protected boolean alwaysShowLoc = false;
  protected boolean drawPieces = true;
  protected boolean drawSingleDeprecated = false;
  protected boolean showText = false;
  protected boolean showTextSingleDeprecated = false;
  protected boolean unrotatePieces = false;
  protected boolean showDeck = false;
  protected double zoomLevel = 1.0;
  protected double graphicsZoomLevel = 1.0;
  protected int borderWidth = 0;
  protected boolean showNoStack = false;
  protected String displayWhat = TOP_LAYER;
  protected String[] displayLayers = new String[0];
  protected FormattedString summaryReportFormat = new FormattedString("$" + BasicPiece.LOCATION_NAME + "$");
  protected FormattedString counterReportFormat = new FormattedString("");
  protected FormattedString emptyHexReportFormat = new FormattedString("$" + BasicPiece.LOCATION_NAME + "$");
  protected String version = "";
  protected Color fgColor = Color.black;
  protected Color bgColor = Color.white;
  protected int fontSize = 9;
  protected PropertyExpression propertyFilter = new PropertyExpression();

  protected Rectangle bounds;
  protected boolean mouseInView = true;
  protected List displayablePieces = null;

  public CounterDetailViewer() {
  }

  public void addTo(Buildable b) {
    map = (Map) b;
    validator = new SingleChildInstance(map, getClass());
    map.addDrawComponent(this);
    GameModule.getGameModule().getPrefs().addOption(Resources.getString("Prefs.general_tab"),
        new BooleanConfigurer(USE_KEYBOARD, Resources.getString("CounterDetailViewer.use_prompt", HotKeyConfigurer.getString(hotkey)), Boolean.FALSE));
    GameModule.getGameModule().getPrefs().addOption(Resources.getString("Prefs.general_tab"),
        new IntConfigurer(PREFERRED_DELAY, Resources.getString("CounterDetailViewer.delay_prompt"), new Integer(delay)));

    map.getView().addMouseMotionListener(this);
    map.getView().addMouseListener(this);
    map.getView().addKeyListener(this);
  }

  public void draw(Graphics g, Map map) {
    if (currentMousePosition != null && map.getView().getVisibleRect().contains(currentMousePosition.getPoint())) {
      draw(g, currentMousePosition.getPoint(), map.getView());
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }

  public void draw(Graphics g, Point pt, JComponent comp) {

    if (!graphicsVisible && !textVisible) {
      return;
    }

    bounds = new Rectangle(pt.x, pt.y, 0, 0);

    if (graphicsVisible) {
      drawGraphics(g, pt, comp, displayablePieces);
    }

    if (textVisible) {
      drawText(g, pt, comp, displayablePieces);
    }
  }

  // Required for backward compatibility
  protected void drawGraphics(Graphics g, Point pt, JComponent comp, PieceIterator pi) {
    ArrayList a = new ArrayList();
    while (pi.hasMoreElements()) {
      a.add(pi.nextPiece());
    }
    drawGraphics(g, pt, comp, pi);
  }

  protected void drawGraphics(Graphics g, Point pt, JComponent comp, List pieces) {

    for (int i = 0; i < pieces.size(); i++) {
      GamePiece piece = (GamePiece) pieces.get(i);
      if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.TRUE);
      Rectangle pieceBounds = piece.getShape().getBounds();
      if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.FALSE);
      bounds.width += (int) (pieceBounds.width * graphicsZoomLevel) + borderWidth;
      bounds.height = Math.max(bounds.height, (int) (pieceBounds.height * graphicsZoomLevel) + borderWidth * 2);
    }
    bounds.width += borderWidth;
    bounds.y -= bounds.height;

    if (bounds.width > 0) {

      Rectangle visibleRect = comp.getVisibleRect();
      bounds.x = Math.min(bounds.x, visibleRect.x + visibleRect.width - bounds.width);
      if (bounds.x < visibleRect.x)
        bounds.x = visibleRect.x;
      bounds.y = Math.min(bounds.y, visibleRect.y + visibleRect.height - bounds.height) - (isTextUnderCounters() ? 15 : 0);
      int minY = visibleRect.y + (textVisible ? g.getFontMetrics().getHeight() + 6 : 0);
      if (bounds.y < minY)
        bounds.y = minY;

      if (bgColor != null) {
        g.setColor(bgColor);
        g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
      }
      if (fgColor != null) {
        g.setColor(fgColor);
        g.drawRect(bounds.x - 1, bounds.y - 1, bounds.width + 1, bounds.height + 1);
        g.drawRect(bounds.x - 2, bounds.y - 2, bounds.width + 3, bounds.height + 3);
      }
      Shape oldClip = g.getClip();

      int borderOffset = borderWidth;
      double graphicsZoom = graphicsZoomLevel;
      for (int i = 0; i < pieces.size(); i++) {
        // Draw the next piece
        // pt is the location of the left edge of the piece
        GamePiece piece = (GamePiece) pieces.get(i);
        if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.TRUE);
        Rectangle pieceBounds = piece.getShape().getBounds();
        g.setClip(bounds.x - 3, bounds.y - 3, bounds.width + 5, bounds.height + 5);
        piece.draw(g, bounds.x - (int) (pieceBounds.x * graphicsZoom) + borderOffset, bounds.y - (int) (pieceBounds.y * graphicsZoom) + borderWidth, comp,
            graphicsZoom);
        if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.FALSE);
        g.setClip(oldClip);

        if (isTextUnderCounters()) {
          String text = counterReportFormat.getText(piece);
          int x = bounds.x - (int) (pieceBounds.x * graphicsZoom) + borderOffset;
          int y = bounds.y + bounds.height + 10;
          drawLabel(g, new Point(x, y), text, Labeler.CENTER, Labeler.CENTER);
        }

        bounds.translate((int) (pieceBounds.width * graphicsZoom), 0);
        borderOffset += borderWidth;
      }

    }
  }

  protected boolean isTextUnderCounters() {
    return textVisible && counterReportFormat.getFormat().length() > 0;
  }

  // Required for backward compatibility
  protected void drawText(Graphics g, Point pt, JComponent comp, PieceIterator pi) {
    ArrayList a = new ArrayList();
    while (pi.hasMoreElements()) {
      a.add(pi.nextPiece());
    }
    drawText(g, pt, comp, pi);
  }

  protected void drawText(Graphics g, Point pt, JComponent comp, List pieces) {
    /*
     * Label with the location If the counter viewer is being displayed, then
     * place the location name just above the left hand end of the counters. If
     * no counter viewer (i.e. single piece or expanded stack), then place the
     * location name above the centre of the first piece in the stack.
     */
    String report = "";
    int x = bounds.x - bounds.width;
    int y = bounds.y - 5;

    if (displayablePieces.size() == 0) {
      Point mapPt = map.mapCoordinates(currentMousePosition.getPoint());
      Point snapPt = map.snapTo(mapPt);
      String locationName = map.locationName(snapPt);
      emptyHexReportFormat.setProperty(BasicPiece.LOCATION_NAME, locationName.equals("offboard") ? "" : locationName);
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_MAP, map.getMapName());
      Board b = map.findBoard(snapPt);
      String boardName = (b == null) ? "" : b.getName();
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_BOARD, boardName);
      Zone z = map.findZone(snapPt);
      String zone = (z == null) ? "" : z.getName();
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_ZONE, zone);
      report = emptyHexReportFormat.getText();
      x -= g.getFontMetrics().stringWidth(report) / 2;
    }
    else {
      GamePiece topPiece = (GamePiece) displayablePieces.get(0);
      String locationName = (String) topPiece.getProperty(BasicPiece.LOCATION_NAME);
      emptyHexReportFormat.setProperty(BasicPiece.LOCATION_NAME, locationName.equals("offboard") ? "" : locationName);
      report = summaryReportFormat.getText(new SumProperties(displayablePieces));
      x += borderWidth * pieces.size() + 2;
    }

    if (report.length() > 0) {
      drawLabel(g, new Point(x, y), report, Labeler.RIGHT, Labeler.BOTTOM);
    }
  }

  // Required for backward compatibility
  protected void drawLabel(Graphics g, Point pt, String label) {
    drawLabel(g, pt, label, Labeler.RIGHT, Labeler.BOTTOM);
  }

  protected void drawLabel(Graphics g, Point pt, String label, int hAlign, int vAlign) {

    if (label != null) {
      Color labelFgColor = fgColor == null ? Color.black : fgColor;
      Graphics2D g2d = ((Graphics2D) g);
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
      Labeler.drawLabel(g, label, pt.x, pt.y, new Font("Dialog", Font.PLAIN, fontSize), hAlign, vAlign, labelFgColor, bgColor, labelFgColor);
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    }
  }

  /*
   * Thread code running in background to show the view after the mouse has been
   * stationery for the specified time.
   */
  public void run() {

    while (System.currentTimeMillis() < expirationTime) {
      try {
        Thread.sleep(Math.max(0, expirationTime - System.currentTimeMillis()));
      }
      catch (InterruptedException e) {
      }
    }
    /*
     * Show the viewer only if the mouse is still on the map
     */
    if (mouseInView) {
      showDetails();
    }
  }

  protected void showDetails() {

    displayablePieces = getDisplayablePieces();

    /*
     * Visibility Rules: Stack - Depends on setting of showGraphics/showText
     * Single Unit - Depends on setting of showGraphics/showText and
     * showGraphicsSingle/showTextSingle and stack must not be expanded. Empty
     * space - Depends on setting of
     */
    
    double zoom = getZoom();
    if (displayablePieces.size() < minimumDisplayablePieces) {
      if (displayablePieces.size() > 0) {
        graphicsVisible = zoom < zoomLevel;
        textVisible = zoom < zoomLevel && (summaryReportFormat.getFormat().length() > 0 || counterReportFormat.getFormat().length() > 0);
      }
      else {
        textVisible = (minimumDisplayablePieces==0 && emptyHexReportFormat.getFormat().length() > 0);
        graphicsVisible = false;
      }
    }
    else {
      graphicsVisible = drawPieces;
      textVisible = showText && (summaryReportFormat.getFormat().length() > 0 || counterReportFormat.getFormat().length() > 0);
    }
    map.repaint();
  }

  protected double getZoom() {
    return map.getZoom();
  }

  /*
   * Build an ArrayList of pieces to be displayed in order from bottom up, based
   * on selection criteria setup in config.
   */
  protected List getDisplayablePieces() {

    GamePiece[] allPieces = map.getPieces(); // All pieces from bottom up

    Visitor visitor = new Visitor(new Filter(), map, map.mapCoordinates(currentMousePosition.getPoint()));
    DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);

    /*
     * Process pieces from the top down to make it easier to check for top layer
     * only.
     */
    for (int i = allPieces.length - 1; i >= 0; i--) {
      dispatcher.accept(allPieces[i]);
    }

    return visitor.getPieces();
  }

  /*
   * Utility class to select the pieces we wish to view.
   */
  protected class Filter implements PieceFilter {

    protected int topLayer;

    public Filter() {
      topLayer = -1;
    }

    public boolean accept(GamePiece piece) {
      return accept(piece, 0, "");
    }

    public boolean accept(GamePiece piece, int layer, String layerName) {

      // Is it visible to us?
      if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
        return false;
      }

      if (Boolean.TRUE.equals(piece.getProperty(Properties.TERRAIN))) {
        return false;
      }

      // If it Does Not Stack, do we want to see it?
      if (Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK)) && !showNoStack) {
        return false;
      }

      // Deck?
      if (piece.getParent() instanceof Deck && !showDeck) {
        return false;
      }

      // Select by property filter
      if (displayWhat.equals(FILTER)) {
        return propertyFilter.accept(piece);
      }

      // Looking at All Layers accepts anything.
      else if (displayWhat.equals(ALL_LAYERS)) {
        return true;
      }
      else {

        if (topLayer < 0) {
          topLayer = layer;
        }

        // Pieces are passed to us top down, so only display the top-most layer
        if (displayWhat.equals(TOP_LAYER)) {
          return layer == topLayer;
        }

        // Include pieces on named layers only
        else if (displayWhat.equals(INC_LAYERS)) {
          for (int i = 0; i < displayLayers.length; i++) {
            if (layerName.equals(displayLayers[i])) {
              return true;
            }
          }
        }

        // Exclude pieces from named layers.
        else if (displayWhat.equals(EXC_LAYERS)) {
          for (int i = 0; i < displayLayers.length; i++) {
            if (layerName.equals(displayLayers[i])) {
              return false;
            }
          }
          return true;
        }
      }

      // Ignore anything else
      return false;
    }

  }

  /*
   * Utility class to visit Map pieces, apply the filter and return a list of
   * pieces we are interested in.
   */
  protected static class Visitor extends PieceFinder.Movable {
    protected ArrayList pieces;
    protected Filter filter = null;
    protected CompoundPieceCollection collection;
    protected int lastLayer = -1;
    protected int insertPos = 0;
    protected Point foundPieceAt;

    public Visitor(Filter filter, Map map, Point pt) {
      super(map,pt);
      if (map.getPieceCollection() instanceof CompoundPieceCollection) {
        collection = (CompoundPieceCollection) map.getPieceCollection();
      }
      pieces = new ArrayList();
      this.filter = filter;
    }

    public Object visitDeck(Deck d) {
      if (foundPieceAt == null) {
        GamePiece top = d.topPiece();
        if (top != null && !Boolean.TRUE.equals(top.getProperty(Properties.OBSCURED_TO_ME))) {
          Rectangle r = (Rectangle) d.getShape();
          r.x += d.getPosition().x;
          r.y += d.getPosition().y;
          if (r.contains(pt)) {
            apply(top);
          }
        }
      }
      return null;
    }

    public Object visitStack(Stack s) {
      boolean addContents = foundPieceAt == null ? super.visitStack(s) != null : foundPieceAt.equals(s.getPosition());
      if (addContents) {
        for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
          apply((GamePiece) e.nextElement());
        }
      }
      return null;
    }

    public Object visitDefault(GamePiece p) {
      if (foundPieceAt == null ? super.visitDefault(p) != null : foundPieceAt.equals(p.getPosition())) {
        apply(p);
      }
      return null;
    }

    /*
     * Insert accepted pieces into the start of the array since we are being
     * passed pieces from the top down.
     */
    protected void apply(GamePiece p) {

      int layer = 0;
      String layerName = "";

      layer = collection.getLayerForPiece(p);
      layerName = collection.getLayerNameForPiece(p);

      if (filter == null || filter.accept(p, layer, layerName)) {

        if (layer != lastLayer) {
          insertPos = 0;
          lastLayer = layer;
        }

        if (foundPieceAt == null) {
          foundPieceAt = p.getPosition();
        }

        pieces.add(insertPos++, p);
      }
    }

    public List getPieces() {
      return pieces;
    }

  }

  public void mouseMoved(MouseEvent e) {

    // clear details when mouse moved
    if (graphicsVisible || textVisible) {
      hideDetails();
    }
    else {
      // set the timer
      currentMousePosition = e;
      // quit if not active
      if (Boolean.FALSE.equals(GameModule.getGameModule().getPrefs().getValue(USE_KEYBOARD))) {
        restartDelay();
        // Reset thread
        // timer
        if (delayThread == null || !delayThread.isAlive()) {
          delayThread = new Thread(this);
          delayThread.start();
        }
      }
    }
  }

  protected void restartDelay() {
    expirationTime = System.currentTimeMillis() + getPreferredDelay();
  }

  protected int getPreferredDelay() {
    return ((Integer) GameModule.getGameModule().getPrefs().getValue(PREFERRED_DELAY)).intValue();
  }

  public void mouseDragged(MouseEvent e) {
    mouseMoved(e);
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
    mouseInView = true;
  }

  public void mouseExited(MouseEvent e) {
    mouseInView = false;
  }

  public void mousePressed(MouseEvent e) {
    restartDelay();
  }

  public void mouseReleased(MouseEvent e) {
    mouseInView = true;
    restartDelay();
  }

  public void keyTyped(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    if (hotkey != null && Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(USE_KEYBOARD))) {
      if (hotkey.equals(KeyStroke.getKeyStrokeForEvent(e))) {
        showDetails();
      }
      else {
        hideDetails();
      }
    }
  }

  public void keyReleased(KeyEvent e) {
  }

  protected void hideDetails() {
    graphicsVisible = false;
    textVisible = false;
    map.repaint();
  }

  /*
   * Compatibility. If this component has not yet been saved by this version of
   * vassal, convert the old-style options to new and update the version.
   */
  public Configurer getConfigurer() {

    // New version 2 viewer being created
    if (map == null) {
      version = LATEST_VERSION;
    }
    // Previous version needing upgrading?
    else if (!version.equals(LATEST_VERSION)) {
      upgrade();
    }
    return super.getConfigurer();
  }

  protected void upgrade() {

    if (!drawPieces && !showText) {
      minimumDisplayablePieces = Integer.MAX_VALUE;
    }
    else if (drawSingleDeprecated) {
      minimumDisplayablePieces = 1;
    }
    else {
      minimumDisplayablePieces = 2;
    }

    fgColor = map.getHighlighter() instanceof ColoredBorder ? ((ColoredBorder) map.getHighlighter()).getColor() : Color.black;

    bgColor = new Color(255 - fgColor.getRed(), 255 - fgColor.getGreen(), 255 - fgColor.getBlue());

    version = LATEST_VERSION;
  }

  public String[] getAttributeNames() {
    return new String[] {VERSION, DELAY, HOTKEY,  

    BG_COLOR, FG_COLOR,

    MINIMUM_DISPLAYABLE, ZOOM_LEVEL, DRAW_PIECES, DRAW_PIECES_AT_ZOOM, GRAPH_SINGLE_DEPRECATED, BORDER_WIDTH,

    SHOW_TEXT, SHOW_TEXT_SINGLE_DEPRECATED, FONT_SIZE, SUMMARY_REPORT_FORMAT, COUNTER_REPORT_FORMAT, EMPTY_HEX_REPORT_FORMAT,

    DISPLAY, LAYER_LIST, PROPERTY_FILTER,SHOW_NOSTACK, UNROTATE_PIECES, SHOW_DECK};
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Version", // Not displayed
        "Recommended Delay before display (ms):  ", "Keyboard shortcut to display:  ",  

        "Background color:  ","Border/text color:  ", 

        "Display when at least this many pieces will be included:  ", "Always display when zoom level less than:  ","Draw pieces?", "Draw pieces using zoom factor:  ","Display unit graphics for single counter?", // Obsolete
        "Width of gap between pieces:  ",

        "Display text?", "Display text report for single counter?",// Obsolete
        "Font size:  ", "Summary text above pieces:  ", "Text below each piece:  ","Text for empty location:  ",

        "Include individual pieces:  ", "Listed layers",
        "Piece selection property filter:  ","Include non-stacking pieces (if movable)?", "Show pieces in un-rotated state?", "Include top piece in Deck?"};

  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, Integer.class, KeyStroke.class,

    Color.class, Color.class,

    MinConfig.class, Double.class, Boolean.class, Double.class, Boolean.class, Integer.class,

    Boolean.class, Boolean.class, Integer.class, ReportFormatConfig.class, CounterFormatConfig.class,EmptyFormatConfig.class,

    DisplayConfig.class, String[].class, PropertyExpression.class,Boolean.class, Boolean.class, Boolean.class};
  }

  public static class DisplayConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {TOP_LAYER, ALL_LAYERS, INC_LAYERS, EXC_LAYERS, FILTER};
    }
  }

  public static class MinConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {"0", "1", "2"};
    }
  }

  public static class EmptyFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {BasicPiece.LOCATION_NAME, BasicPiece.CURRENT_MAP, BasicPiece.CURRENT_BOARD,
          BasicPiece.CURRENT_ZONE});
    }
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {BasicPiece.LOCATION_NAME, BasicPiece.CURRENT_MAP, BasicPiece.CURRENT_BOARD,
          BasicPiece.CURRENT_ZONE, SUM});
    }
  }

  public static class CounterFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {BasicPiece.PIECE_NAME});
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "StackViewer");
  }

  public void removeFrom(Buildable parent) {
    map.removeDrawComponent(this);
    map.getView().removeMouseMotionListener(this);
  }

  public void setAttribute(String name, Object value) {
    if (DELAY.equals(name)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      if (value != null) {
        delay = ((Integer) value).intValue();
      }
    }
    else if (HOTKEY.equals(name)) {
      if (value instanceof String) {
        hotkey = HotKeyConfigurer.decode((String)value);
      }
      else {
        hotkey = (KeyStroke)value;
      }
    }
    else if (DRAW_PIECES.equals(name)) {
      if (value instanceof Boolean) {
        drawPieces = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        drawPieces = "true".equals(value);
      }
    }
    else if (GRAPH_SINGLE_DEPRECATED.equals(name)) {
      if (value instanceof Boolean) {
        drawSingleDeprecated = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        drawSingleDeprecated = "true".equals(value);
      }
    }
    else if (SHOW_TEXT.equals(name)) {
      if (value instanceof Boolean) {
        showText = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showText = "true".equals(value);
      }

    }
    else if (SHOW_TEXT_SINGLE_DEPRECATED.equals(name)) {
      if (value instanceof Boolean) {
        showTextSingleDeprecated = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showTextSingleDeprecated = "true".equals(value);
      }
    }
    else if (ZOOM_LEVEL.equals(name)) {
      if (value instanceof String) {
        value = new Double((String) value);
      }
      zoomLevel = ((Double) value).doubleValue();
    }
    else if (DRAW_PIECES_AT_ZOOM.equals(name)) {
      if (value instanceof String) {
        value = new Double((String) value);
      }
      graphicsZoomLevel = ((Double) value).doubleValue();
    }
    else if (BORDER_WIDTH.equals(name)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      borderWidth = ((Integer) value).intValue();
    }
    else if (SHOW_NOSTACK.equals(name)) {
      if (value instanceof Boolean) {
        showNoStack = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showNoStack = "true".equals(value);
      }
    }
    else if (SHOW_DECK.equals(name)) {
      if (value instanceof Boolean) {
        showDeck = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showDeck = "true".equals(value);
      }
    }
    else if (UNROTATE_PIECES.equals(name)) {
        if (value instanceof Boolean) {
          unrotatePieces = ((Boolean) value).booleanValue();
        }
        else if (value instanceof String) {
          unrotatePieces = "true".equals(value);
        }
      }
    else if (DISPLAY.equals(name)) {
      displayWhat = (String) value;
    }
    else if (LAYER_LIST.equals(name)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      displayLayers = (String[]) value;
    }
    else if (EMPTY_HEX_REPORT_FORMAT.equals(name)) {
      emptyHexReportFormat.setFormat((String) value);
    }
    else if (SUMMARY_REPORT_FORMAT.equals(name)) {
      summaryReportFormat.setFormat((String) value);
    }
    else if (COUNTER_REPORT_FORMAT.equals(name)) {
      counterReportFormat.setFormat((String) value);
    }
    else if (MINIMUM_DISPLAYABLE.equals(name)) {
      try {
        minimumDisplayablePieces = Integer.parseInt((String) value);
      }
      catch (NumberFormatException e) {
      }
    }
    else if (VERSION.equals(name)) {
      version = (String) value;
    }
    else if (FG_COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      fgColor = (Color) value;
    }
    else if (BG_COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      bgColor = (Color) value;
    }
    else if (FONT_SIZE.equals(name)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      if (value != null) {
        fontSize = ((Integer) value).intValue();
      }
    }
    else if (PROPERTY_FILTER.equals(name)) {
      propertyFilter.setExpression((String) value);
    }
  }

  public String getAttributeValueString(String name) {
    if (DELAY.equals(name)) {
      return String.valueOf(delay);
    }
    else if (HOTKEY.equals(name)) {
      return HotKeyConfigurer.encode(hotkey);
    }
    else if (DRAW_PIECES.equals(name)) {
      return String.valueOf(drawPieces);
    }
    else if (GRAPH_SINGLE_DEPRECATED.equals(name)) {
      return String.valueOf(drawSingleDeprecated);
    }
    else if (SHOW_TEXT.equals(name)) {
      return String.valueOf(showText);
    }
    else if (SHOW_TEXT_SINGLE_DEPRECATED.equals(name)) {
      return String.valueOf(showTextSingleDeprecated);
    }
    else if (ZOOM_LEVEL.equals(name)) {
      return String.valueOf(zoomLevel);
    }
    else if (DRAW_PIECES_AT_ZOOM.equals(name)) {
      return String.valueOf(graphicsZoomLevel);
    }
    else if (BORDER_WIDTH.equals(name)) {
      return String.valueOf(borderWidth);
    }
    else if (SHOW_NOSTACK.equals(name)) {
      return String.valueOf(showNoStack);
    }
    else if (SHOW_DECK.equals(name)) {
      return String.valueOf(showDeck);
    }
    else if (UNROTATE_PIECES.equals(name)) {
        return String.valueOf(unrotatePieces);
    }
    else if (DISPLAY.equals(name)) {
      return displayWhat;
    }
    else if (LAYER_LIST.equals(name)) {
      return StringArrayConfigurer.arrayToString(displayLayers);
    }
    else if (EMPTY_HEX_REPORT_FORMAT.equals(name)) {
      return emptyHexReportFormat.getFormat();
    }
    else if (SUMMARY_REPORT_FORMAT.equals(name)) {
      return summaryReportFormat.getFormat();
    }
    else if (COUNTER_REPORT_FORMAT.equals(name)) {
      return counterReportFormat.getFormat();
    }
    else if (MINIMUM_DISPLAYABLE.equals(name)) {
      return String.valueOf(minimumDisplayablePieces);
    }
    else if (VERSION.equals(name)) {
      return version;
    }
    else if (FG_COLOR.equals(name)) {
      return ColorConfigurer.colorToString(fgColor);
    }
    else if (BG_COLOR.equals(name)) {
      return ColorConfigurer.colorToString(bgColor);
    }
    else if (FONT_SIZE.equals(name)) {
      return String.valueOf(fontSize);
    }
    else if (PROPERTY_FILTER.equals(name)) {
      return propertyFilter.getExpression();
    }
    else
      return null;
  }

  public static String getConfigureTypeName() {
    return "Mouse-over Stack Viewer";
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (BORDER_WIDTH.equals(name) || DRAW_PIECES_AT_ZOOM.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return drawPieces;
        }
      };
    }
    else if (FONT_SIZE.equals(name) || SUMMARY_REPORT_FORMAT.equals(name) || COUNTER_REPORT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return showText;
        }
      };
    }
    else if (DRAW_PIECES.equals(name) || SHOW_TEXT.equals(name) || SHOW_NOSTACK.equals(name) || SHOW_DECK.equals(name) || DISPLAY.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return true;
        }
      };
    }
    else if (LAYER_LIST.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return (displayWhat.equals(INC_LAYERS) || displayWhat.equals(EXC_LAYERS));
        }
      };
    }
    else if (PROPERTY_FILTER.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return displayWhat.equals(FILTER);
        }
      };
    }
    else if (EMPTY_HEX_REPORT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return showText && minimumDisplayablePieces == 0;
        }
      };
    }
    /*
     * The following fields are not to be displayed. They are either obsolete
     * or maintained for backward compatibility
     */
    else if (VERSION.equals(name) || SHOW_TEXT_SINGLE_DEPRECATED.equals(name) || GRAPH_SINGLE_DEPRECATED.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return false;
        }
      };
    }
    return null;
  }

}
