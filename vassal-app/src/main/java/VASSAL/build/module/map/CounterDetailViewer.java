/*
 *
 * Copyright (c) 2003-2013 by David Sullivan, Rodney Kinney,
 * Brent Easton, and Joel Uckelman.
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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceMotionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.Timer;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
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
 * This is a {@link Drawable} class that draws the counters horizontally when
 * the mouse is held over a stack with the control key down.
 *
 * @author David Sullivan
 * @version 1.0
 */
public class CounterDetailViewer extends AbstractConfigurable implements Drawable, DragSourceMotionListener, MouseMotionListener, MouseListener, KeyListener {

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
  public static final String SHOW_MOVE_SELECTED = "showMoveSelectde";
  public static final String SHOW_NON_MOVABLE = "showNonMovable";
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

  protected KeyStroke hotkey =
    KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, InputEvent.CTRL_DOWN_MASK);
  protected Map map;
  protected int delay = 700;
  protected Timer delayTimer;

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
  protected boolean showMoveSelected = false;
  protected boolean showNonMovable = false;
  protected String displayWhat = TOP_LAYER;
  protected String[] displayLayers = new String[0];
  protected FormattedString summaryReportFormat = new FormattedString("$" + BasicPiece.LOCATION_NAME + "$");
  protected FormattedString counterReportFormat = new FormattedString("");
  protected FormattedString emptyHexReportFormat = new FormattedString("$" + BasicPiece.LOCATION_NAME + "$");
  protected String version = "";
  protected Color fgColor = Color.black;
  protected Color bgColor;
  protected int fontSize = 9;
  protected Font font = new Font("Dialog", Font.PLAIN, fontSize);
  protected PropertyExpression propertyFilter = new PropertyExpression();

  protected Rectangle bounds = new Rectangle();
  protected boolean mouseInView = true;
  protected List<GamePiece> displayablePieces = null;

  /** the JComponent which is repainted when the detail viewer changes */
  protected JComponent view;

  public CounterDetailViewer() {
    // Set up the timer; this isn't the real delay---we always check the
    // preferences for that.
    delayTimer = new Timer(delay, new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        if (mouseInView) showDetails();
      }
    });

    delayTimer.setRepeats(false);
  }

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    view = map.getView();
    validator = new SingleChildInstance(map, getClass());
    map.addDrawComponent(this);
    String keyDesc = hotkey == null ? "" : "(" + HotKeyConfigurer.getString(hotkey) + ")";
    GameModule.getGameModule().getPrefs().addOption(Resources.getString("Prefs.general_tab"),
        new BooleanConfigurer(USE_KEYBOARD, Resources.getString("CounterDetailViewer.use_prompt", keyDesc), Boolean.FALSE));
    GameModule.getGameModule().getPrefs().addOption(Resources.getString("Prefs.general_tab"),
        new IntConfigurer(PREFERRED_DELAY, Resources.getString("CounterDetailViewer.delay_prompt"), delay));

    view.addMouseMotionListener(this);
    view.addMouseListener(this);
    view.addKeyListener(this);
    DragSource.getDefaultDragSource().addDragSourceMotionListener(this);

    setAttributeTranslatable(VERSION, false);
    setAttributeTranslatable(SUMMARY_REPORT_FORMAT, true);
    setAttributeTranslatable(COUNTER_REPORT_FORMAT, true);
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (currentMousePosition != null &&
        view.getVisibleRect().contains(currentMousePosition.getPoint())) {
      draw(g, currentMousePosition.getPoint(), view);
    }
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  public void draw(Graphics g, Point pt, JComponent comp) {
    if (!graphicsVisible && !textVisible) {
      return;
    }

    bounds.x = pt.x;
    bounds.y = pt.y;
    bounds.width = 0;
    bounds.height = 0;

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    g2d.setFont(font.deriveFont((float)(fontSize * os_scale)));

    if (graphicsVisible) {
      drawGraphics(g, pt, comp, displayablePieces);
    }

    if (textVisible) {
      drawText(g, pt, comp, displayablePieces);
    }
  }

  @Deprecated // Required for backward compatibility
  protected void drawGraphics(Graphics g, Point pt, JComponent comp,
                              PieceIterator pi) {
    ArrayList<GamePiece> a = new ArrayList<>();
    while (pi.hasMoreElements()) {
      a.add(pi.nextPiece());
    }
    drawGraphics(g, pt, comp, a);
  }

  protected void drawGraphics(Graphics g, Point pt, JComponent comp, List<GamePiece> pieces) {
    fixBounds(pieces);

    if (bounds.width <= 0) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    final Rectangle dbounds = new Rectangle(bounds);
    dbounds.x *= os_scale;
    dbounds.y *= os_scale;
    dbounds.width *= os_scale;
    dbounds.height *= os_scale;

    final Rectangle visibleRect = comp.getVisibleRect();
    visibleRect.x *= os_scale;
    visibleRect.y *= os_scale;
    visibleRect.width *= os_scale;
    visibleRect.height *= os_scale;

    dbounds.x = Math.min(dbounds.x, visibleRect.x + visibleRect.width - dbounds.width);
    if (dbounds.x < visibleRect.x)
      dbounds.x = visibleRect.x;
    dbounds.y = Math.min(dbounds.y, visibleRect.y + visibleRect.height - dbounds.height) - (isTextUnderCounters() ? 15 : 0);
    int minY = visibleRect.y + (textVisible ? g.getFontMetrics().getHeight() + 6 : 0);
    if (dbounds.y < minY)
      dbounds.y = minY;

    if (bgColor != null) {
      g.setColor(bgColor);
      g.fillRect(dbounds.x, dbounds.y, dbounds.width, dbounds.height);
    }

    if (fgColor != null) {
      g.setColor(fgColor);
      g.drawRect(dbounds.x - 1, dbounds.y - 1, dbounds.width + 1, dbounds.height + 1);
      g.drawRect(dbounds.x - 2, dbounds.y - 2, dbounds.width + 3, dbounds.height + 3);
    }

    Shape oldClip = g.getClip();

    Object owner = null;
    int borderOffset = borderWidth;
    double graphicsZoom = graphicsZoomLevel;
    for (GamePiece piece : pieces) {
      // Draw the next piece
      // pt is the location of the left edge of the piece
      Rectangle pieceBounds = getBounds(piece);
      if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.TRUE);
      g.setClip(dbounds.x - 3, dbounds.y - 3, dbounds.width + 5, dbounds.height + 5);
      final Stack parent = piece.getParent();
      if (parent instanceof Deck) {
        owner = piece.getProperty(Properties.OBSCURED_BY);
        final boolean faceDown = ((Deck) parent).isFaceDown();
        piece.setProperty(Properties.OBSCURED_BY, faceDown ? Deck.NO_USER : null);
      }

      piece.draw(
        g,
        dbounds.x - (int) (pieceBounds.x * graphicsZoom * os_scale) + (int)(borderOffset * os_scale),
        dbounds.y - (int) (pieceBounds.y * graphicsZoom * os_scale) + (int)(borderWidth * os_scale),
        comp,
        graphicsZoom * os_scale
      );

      if (parent instanceof Deck) piece.setProperty(Properties.OBSCURED_BY, owner);
      if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.FALSE);
      g.setClip(oldClip);

      if (isTextUnderCounters()) {
        String text = counterReportFormat.getLocalizedText(piece);
        if (text.length() > 0) {
          int x = dbounds.x - (int) (pieceBounds.x * graphicsZoom * os_scale) + (int)(borderOffset * os_scale);
          int y = dbounds.y + dbounds.height + 10;
          drawLabel(g, new Point(x, y), text, Labeler.CENTER, Labeler.CENTER);
        }
      }

      dbounds.translate((int) (pieceBounds.width * graphicsZoom * os_scale), 0);
      borderOffset += borderWidth;
    }

    bounds.x = (int)(dbounds.x / os_scale);
    bounds.y = (int)(dbounds.y / os_scale);
    bounds.width = (int)(dbounds.width / os_scale);
    bounds.height = (int)(dbounds.height / os_scale);
  }

  /** Set the bounds field large enough to accommodate the given set of pieces */
  protected void fixBounds(List<GamePiece> pieces) {
    for (GamePiece piece : pieces) {
      final Dimension pieceBounds = getBounds(piece).getSize();
      bounds.width += (int) Math.round(pieceBounds.width * graphicsZoomLevel) + borderWidth;
      bounds.height = Math.max(bounds.height, (int) Math.round(pieceBounds.height * graphicsZoomLevel) + borderWidth * 2);
    }

    bounds.width += borderWidth;
    bounds.y -= bounds.height;
  }

  protected Rectangle getBounds(GamePiece piece) {
    if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.TRUE);
    Rectangle pieceBounds = piece.getShape().getBounds();
    if (unrotatePieces) piece.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.FALSE);
    return pieceBounds;
  }

  protected boolean isTextUnderCounters() {
    return textVisible && counterReportFormat.getFormat().length() > 0;
  }

  @Deprecated // Required for backward compatibility
  protected void drawText(Graphics g, Point pt,
                          JComponent comp, PieceIterator pi) {
    ArrayList<GamePiece> a = new ArrayList<>();
    while (pi.hasMoreElements()) {
      a.add(pi.nextPiece());
    }
    drawText(g, pt, comp, a);
  }

  protected void drawText(Graphics g, Point pt, JComponent comp, List<GamePiece> pieces) {
    /*
     * Label with the location If the counter viewer is being displayed, then
     * place the location name just above the left hand end of the counters. If
     * no counter viewer (i.e. single piece or expanded stack), then place the
     * location name above the centre of the first piece in the stack.
     */
    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    String report = "";
    int x = (int)((bounds.x - bounds.width) * os_scale);
    int y = (int)((bounds.y - 5) * os_scale);
    String offboard = Resources.getString("Map.offboard");  //$NON-NLS-1$

    if (displayablePieces.isEmpty()) {
      Point mapPt = map.componentToMap(currentMousePosition.getPoint());
      Point snapPt = map.snapTo(mapPt);
      String locationName = map.localizedLocationName(snapPt);
      emptyHexReportFormat.setProperty(BasicPiece.LOCATION_NAME, locationName.equals(offboard) ? "" : locationName);
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_MAP, map.getLocalizedMapName());
      Board b = map.findBoard(snapPt);
      String boardName = (b == null) ? "" : b.getLocalizedName();
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_BOARD, boardName);
      Zone z = map.findZone(snapPt);
      String zone = (z == null) ? "" : z.getLocalizedName();
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_ZONE, zone);
      report = emptyHexReportFormat.getLocalizedText();
      x -= g.getFontMetrics().stringWidth(report) / 2;
    }
    else {
      GamePiece topPiece = displayablePieces.get(0);
      String locationName = (String) topPiece.getLocalizedProperty(BasicPiece.LOCATION_NAME);
      emptyHexReportFormat.setProperty(BasicPiece.LOCATION_NAME, locationName.equals(offboard) ? "" : locationName);
      report = summaryReportFormat.getLocalizedText(new SumProperties(displayablePieces));
      x += borderWidth * os_scale * pieces.size() + 2;
    }

    if (report.length() > 0) {
      drawLabel(g, new Point(x, y), report, Labeler.RIGHT, Labeler.BOTTOM);
    }
  }

  @Deprecated // Required for backward compatibility
  protected void drawLabel(Graphics g, Point pt, String label) {
    drawLabel(g, pt, label, Labeler.RIGHT, Labeler.BOTTOM);
  }

  protected void drawLabel(Graphics g, Point pt, String label, int hAlign, int vAlign) {
    if (label != null) {
      Color labelFgColor = fgColor == null ? Color.black : fgColor;
      Graphics2D g2d = (Graphics2D) g;
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
      Labeler.drawLabel(g, label, pt.x, pt.y, g.getFont(), hAlign, vAlign, labelFgColor, bgColor, labelFgColor);
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
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

  /**
   * Build an ArrayList of pieces to be displayed in order from bottom up, based
   * on selection criteria setup in config.
   */
  protected List<GamePiece> getDisplayablePieces() {
    GamePiece[] allPieces = map.getPieces(); // All pieces from bottom up

    Visitor visitor = new Visitor(new Filter(), map,
      map.componentToMap(currentMousePosition.getPoint()));
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

  /**
   * Utility class to select the pieces we wish to view.
   */
  protected class Filter implements PieceFilter {

    protected int topLayer;

    public Filter() {
      topLayer = -1;
    }

    @Override
    public boolean accept(GamePiece piece) {
      return accept(piece, 0, "");
    }

    public boolean accept(GamePiece piece, int layer, String layerName) {

      // Is it visible to us?
      if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
        return false;
      }

      // If it Does Not Stack, do we want to see it?
      if (Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK)) && !showNoStack) {
        return false;
      }

      if (Boolean.TRUE.equals(piece.getProperty(Properties.NON_MOVABLE)) && !showNonMovable) {
        return false;
      }

      if (Boolean.TRUE.equals(piece.getProperty(Properties.TERRAIN)) && !showMoveSelected) {
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
          for (String displayLayer : displayLayers) {
            if (layerName.equals(displayLayer)) {
              return true;
            }
          }
        }

        // Exclude pieces from named layers.
        else if (displayWhat.equals(EXC_LAYERS)) {
          for (String displayLayer : displayLayers) {
            if (layerName.equals(displayLayer)) {
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
    protected List<GamePiece> pieces;
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
      pieces = new ArrayList<>();
      this.filter = filter;
    }

    @Override
    public Object visitDeck(Deck d) {
      if (foundPieceAt == null) {
        GamePiece top = d.topPiece();
        if (top != null &&
            !Boolean.TRUE.equals(top.getProperty(Properties.OBSCURED_TO_ME))) {
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

    @Override
    public Object visitStack(Stack s) {
      boolean addContents = foundPieceAt == null ?
        super.visitStack(s) != null : foundPieceAt.equals(s.getPosition());
      if (addContents) {
        s.asList().forEach(this::apply);
      }
      return null;
    }

    @Override
    public Object visitDefault(GamePiece p) {
      if (foundPieceAt == null ? super.visitDefault(p) != null
                               : foundPieceAt.equals(p.getPosition())) {
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

    public List<GamePiece> getPieces() {
      return pieces;
    }
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    // clear details when mouse moved
    if (graphicsVisible || textVisible) {
      hideDetails();
    }
    else {
      currentMousePosition = e;

      if (Boolean.FALSE.equals(
            GameModule.getGameModule().getPrefs().getValue(USE_KEYBOARD))) {

        // Restart timer
        if (delayTimer.isRunning()) delayTimer.stop();
        delayTimer.setInitialDelay(getPreferredDelay());
        delayTimer.start();
      }
    }
  }

  protected int getPreferredDelay() {
    return (Integer)
      GameModule.getGameModule().getPrefs().getValue(PREFERRED_DELAY);
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    mouseMoved(e);
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  @Override
  public void mouseEntered(MouseEvent e) {
    mouseInView = true;
  }

  @Override
  public void mouseExited(MouseEvent e) {
    mouseInView = false;
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (delayTimer.isRunning()) delayTimer.stop();
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    mouseInView = true;
    if (delayTimer.isRunning()) delayTimer.stop();
  }

  @Override
  public void dragMouseMoved(DragSourceDragEvent e) {
    // This prevents the viewer from popping up during piece drags.
    if (delayTimer.isRunning()) delayTimer.stop();
  }

  @Override
  public void keyTyped(KeyEvent e) {
  }

  @Override
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

  @Override
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
  @Override
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

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      VERSION,
      DELAY,
      HOTKEY,
      BG_COLOR,
      FG_COLOR,
      MINIMUM_DISPLAYABLE,
      ZOOM_LEVEL,
      DRAW_PIECES,
      DRAW_PIECES_AT_ZOOM,
      GRAPH_SINGLE_DEPRECATED,
      BORDER_WIDTH,
      SHOW_TEXT,
      SHOW_TEXT_SINGLE_DEPRECATED,
      FONT_SIZE,
      SUMMARY_REPORT_FORMAT,
      COUNTER_REPORT_FORMAT,
      EMPTY_HEX_REPORT_FORMAT,
      DISPLAY,
      LAYER_LIST,
      PROPERTY_FILTER,SHOW_NOSTACK,
      SHOW_MOVE_SELECTED,
      SHOW_NON_MOVABLE,
      UNROTATE_PIECES,
      SHOW_DECK
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
       Resources.getString("Editor.MouseOverStackViewer.version"), //$NON-NLS-1$ not displayed
       Resources.getString("Editor.MouseOverStackViewer.recommend_delay"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.keyboard_shortcut"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.bg_color"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.text_color"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.display_pieces"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.display_zoom"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.draw_pieces"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.draw_zoom"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.display_graphics_obselete"), //$NON-NLS-1$ Obsolete
       Resources.getString("Editor.MouseOverStackViewer.piece_gap"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.display_text"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.display_text_obsolete"), //$NON-NLS-1$ Obsolete
       Resources.getString("Editor.MouseOverStackViewer.font_size"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.summary_text"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.text_below"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.text_empty"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.include_pieces"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.listed_layers"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.piece_filter"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.non_stacking"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.move_selected"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.non_moveable"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.unrotated_state"), //$NON-NLS-1$
       Resources.getString("Editor.MouseOverStackViewer.top_deck"), //$NON-NLS-1$
      };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      Integer.class,
      KeyStroke.class,
      Color.class,
      Color.class,
      MinConfig.class,
      Double.class,
      Boolean.class,
      Double.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      Boolean.class,
      Integer.class,
      ReportFormatConfig.class,
      CounterFormatConfig.class,
      EmptyFormatConfig.class,
      DisplayConfig.class,
      String[].class,
      PropertyExpression.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Boolean.class
    };
  }

  public static class DisplayConfig extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {TOP_LAYER, ALL_LAYERS, INC_LAYERS, EXC_LAYERS, FILTER};
    }
  }

  public static class MinConfig extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {"0", "1", "2"};
    }
  }

  public static class EmptyFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {
        BasicPiece.LOCATION_NAME,
        BasicPiece.CURRENT_MAP,
        BasicPiece.CURRENT_BOARD,
        BasicPiece.CURRENT_ZONE});
    }
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {
        BasicPiece.LOCATION_NAME,
        BasicPiece.CURRENT_MAP,
        BasicPiece.CURRENT_BOARD,
        BasicPiece.CURRENT_ZONE, SUM});
    }
  }

  public static class CounterFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {BasicPiece.PIECE_NAME});
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "StackViewer");
  }

  @Override
  public void removeFrom(Buildable parent) {
    map.removeDrawComponent(this);
    view.removeMouseMotionListener(this);
  }

  @Override
  public void setAttribute(String name, Object value) {
    if (DELAY.equals(name)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      if (value != null) {
        delay = (Integer) value;
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
        drawPieces = (Boolean) value;
      }
      else if (value instanceof String) {
        drawPieces = "true".equals(value);
      }
    }
    else if (GRAPH_SINGLE_DEPRECATED.equals(name)) {
      if (value instanceof Boolean) {
        drawSingleDeprecated = (Boolean) value;
      }
      else if (value instanceof String) {
        drawSingleDeprecated = "true".equals(value);
      }
    }
    else if (SHOW_TEXT.equals(name)) {
      if (value instanceof Boolean) {
        showText = (Boolean) value;
      }
      else if (value instanceof String) {
        showText = "true".equals(value);
      }

    }
    else if (SHOW_TEXT_SINGLE_DEPRECATED.equals(name)) {
      if (value instanceof Boolean) {
        showTextSingleDeprecated = (Boolean) value;
      }
      else if (value instanceof String) {
        showTextSingleDeprecated = "true".equals(value);
      }
    }
    else if (ZOOM_LEVEL.equals(name)) {
      if (value instanceof String) {
        value = Double.valueOf((String) value);
      }
      zoomLevel = (Double) value;
    }
    else if (DRAW_PIECES_AT_ZOOM.equals(name)) {
      if (value instanceof String) {
        value = Double.valueOf((String) value);
      }
      graphicsZoomLevel = (Double) value;
    }
    else if (BORDER_WIDTH.equals(name)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      borderWidth = (Integer) value;
    }
    else if (SHOW_NOSTACK.equals(name)) {
      if (value instanceof Boolean) {
        showNoStack = (Boolean) value;
      }
      else if (value instanceof String) {
        showNoStack = "true".equals(value);
      }
    }
    else if (SHOW_MOVE_SELECTED.equals(name)) {
      if (value instanceof Boolean) {
        showMoveSelected = (Boolean) value;
      }
      else if (value instanceof String) {
        showMoveSelected = "true".equals(value);
      }
    }
    else if (SHOW_NON_MOVABLE.equals(name)) {
      if (value instanceof Boolean) {
        showNonMovable = (Boolean) value;
      }
      else if (value instanceof String) {
        showNonMovable = "true".equals(value);
      }
    }
    else if (SHOW_DECK.equals(name)) {
      if (value instanceof Boolean) {
        showDeck = (Boolean) value;
      }
      else if (value instanceof String) {
        showDeck = "true".equals(value);
      }
    }
    else if (UNROTATE_PIECES.equals(name)) {
      if (value instanceof Boolean) {
        unrotatePieces = (Boolean) value;
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
        throw new IllegalBuildException(e);
      }
    }
    else if (VERSION.equals(name)) {
      version = (String) value;
    }
    else if (FG_COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      fgColor = value == null ? Color.black : (Color) value;
    }
    else if (BG_COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      bgColor = (Color) value;
    }
    else if (FONT_SIZE.equals(name)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      if (value != null) {
        fontSize = (Integer) value;
        font = font.deriveFont(fontSize);
      }
    }
    else if (PROPERTY_FILTER.equals(name)) {
      propertyFilter.setExpression((String) value);
    }
  }

  @Override
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
    else if (SHOW_MOVE_SELECTED.equals(name)) {
      return String.valueOf(showMoveSelected);
    }
    else if (SHOW_NON_MOVABLE.equals(name)) {
      return String.valueOf(showNonMovable);
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
    return Resources.getString("Editor.MouseOverStackViewer.component_type"); //$NON-NLS-1$
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (BORDER_WIDTH.equals(name) || DRAW_PIECES_AT_ZOOM.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return drawPieces;
        }
      };
    }
    else if (List.of(FONT_SIZE, SUMMARY_REPORT_FORMAT, COUNTER_REPORT_FORMAT).contains(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return showText;
        }
      };
    }
    else if (List.of(DRAW_PIECES, SHOW_TEXT, SHOW_NOSTACK, SHOW_DECK, DISPLAY).contains(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return true;
        }
      };
    }
    else if (LAYER_LIST.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return (displayWhat.equals(INC_LAYERS) || displayWhat.equals(EXC_LAYERS));
        }
      };
    }
    else if (PROPERTY_FILTER.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return displayWhat.equals(FILTER);
        }
      };
    }
    else if (EMPTY_HEX_REPORT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return showText && minimumDisplayablePieces == 0;
        }
      };
    }
    else if (SHOW_MOVE_SELECTED.equals(name) || SHOW_NON_MOVABLE.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return showNoStack;
        }
      };
    }
    /*
     * The following fields are not to be displayed. They are either obsolete
     * or maintained for backward compatibility
     */
    else if (List.of(VERSION, SHOW_TEXT_SINGLE_DEPRECATED, GRAPH_SINGLE_DEPRECATED).contains(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return false;
        }
      };
    }
    return null;
  }
}
