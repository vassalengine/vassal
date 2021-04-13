/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Joel Uckelman, Brent Easton
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

import static java.lang.Math.round;
import java.awt.AWTEventMulticaster;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.OverlayLayout;
import javax.swing.RootPaneContainer;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import VASSAL.launch.PlayerWindow;
import VASSAL.preferences.GlobalPrefs;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.SystemUtils;

import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.TimingTargetAdapter;

import org.w3c.dom.Element;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.DefaultPieceCollection;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.Drawable;
import VASSAL.build.module.map.Flare;
import VASSAL.build.module.map.ForwardToChatter;
import VASSAL.build.module.map.ForwardToKeyBuffer;
import VASSAL.build.module.map.GlobalMap;
import VASSAL.build.module.map.HidePiecesButton;
import VASSAL.build.module.map.HighlightLastMoved;
import VASSAL.build.module.map.ImageSaver;
import VASSAL.build.module.map.KeyBufferer;
import VASSAL.build.module.map.LOS_Thread;
import VASSAL.build.module.map.LayeredPieceCollection;
import VASSAL.build.module.map.MapCenterer;
import VASSAL.build.module.map.MapShader;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.build.module.map.MenuDisplayer;
import VASSAL.build.module.map.PieceCollection;
import VASSAL.build.module.map.PieceMover;
import VASSAL.build.module.map.PieceRecenterer;
import VASSAL.build.module.map.Scroller;
import VASSAL.build.module.map.SelectionHighlighters;
import VASSAL.build.module.map.SetupStack;
import VASSAL.build.module.map.StackExpander;
import VASSAL.build.module.map.StackMetrics;
import VASSAL.build.module.map.TextSaver;
import VASSAL.build.module.map.Zoomer;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.Region;
import VASSAL.build.module.map.boardPicker.board.RegionGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.map.boardPicker.board.mapgrid.ZoneHighlight;
import VASSAL.build.module.properties.ChangePropertyCommandEncoder;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.widget.MapWidget;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.CompoundValidityChecker;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.MandatoryComponent;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.Highlighter;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.ReportState;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.Prefs;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.ToolBarComponent;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.WrapLayout;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SplitPane;
import VASSAL.tools.swing.SwingUtils;

import static VASSAL.preferences.Prefs.MAIN_WINDOW_HEIGHT;
import static VASSAL.preferences.Prefs.MAIN_WINDOW_WIDTH;
import static VASSAL.preferences.Prefs.MAIN_WINDOW_REMEMBER;

/**
 * The Map is the main component for displaying and containing {@link GamePiece}s during play. Pieces are displayed on
 * the map's {@link Board}(s) and moved by clicking and dragging. Keyboard events are forwarded to selected pieces. Multiple
 * map windows are supported in a single game, with dragging between windows allowed.
 *
 * To a map's {@link Board} subcomponent(s), various forms of grid can be added: ({@link ZonedGrid} (aka Multi-zone Grid),
 * {@link HexGrid}, {@link SquareGrid} (aka Rectangular Grid), and {@link RegionGrid} (aka Irregular Grid). These can be used
 * to determine where pieces are allowed to move, and also for filling properties (e.g. LocationName, CurrentZone, CurrentBoard,
 * CurrentMap) to allow the module to keep track of pieces and react to their movements.
 *
 * A Map may contain many different {@link Buildable} subcomponents. Components which are addable <i>uniquely</i> to a Map are
 * contained in the <code>VASSAL.build.module.map</code> package. Some of the most common Map subcomponents include
 * {@link Zoomer} for Zoom Capability, {@link CounterDetailViewer} aka Mouse-over Stack Viewer, {@link HidePiecesButton},
 * and {@link SelectionHighlighters}.
 *
 * Map also contain several critical subcomponents which are automatically added and are not configurable at the module level.
 * These include {@link PieceMover} which handles dragging and dropping of pieces, {@link KeyBufferer} which tracks which pieces
 * are currently "selected" and forwards key commands to them, {@link MenuDisplayer} which listens for "right clicks" and provides
 * "context menu" services, and {@link StackMetrics} which handles the "stacking" of game pieces.
 */
public class Map extends AbstractToolbarItem implements GameComponent, MouseListener, MouseMotionListener, DropTargetListener, Configurable,
    UniqueIdManager.Identifyable, ToolBarComponent, MutablePropertiesContainer, PropertySource, PlayerRoster.SideChangeListener {
  protected static boolean changeReportingEnabled = true;
  protected String mapID = ""; //$NON-NLS-1$
  protected String mapName = ""; //$NON-NLS-1$
  protected static final UniqueIdManager idMgr = new UniqueIdManager("Map"); //$NON-NLS-1$
  protected JPanel theMap;  // Our main visual interface component
  protected ArrayList<Drawable> drawComponents = new ArrayList<>(); //NOPMD
  protected JLayeredPane layeredPane = new JLayeredPane();
  protected JScrollPane scroll;

  @SuppressWarnings("removal")
  @Deprecated(since = "2020-11-05", forRemoval = true)
  protected VASSAL.tools.ComponentSplitter.SplitPane mainWindowDock;

  protected SplitPane splitPane;

  protected BoardPicker picker;
  protected JToolBar toolBar = new JToolBar();
  protected Zoomer zoom;
  protected StackMetrics metrics;
  protected Dimension edgeBuffer = new Dimension(0, 0);
  protected Color bgColor = Color.white;

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launchButton;

  protected boolean useLaunchButton = false;
  protected boolean useLaunchButtonEdit = false;
  protected String markMovedOption = GlobalOptions.ALWAYS;
  protected String markUnmovedIcon = "/images/unmoved.gif"; //$NON-NLS-1$
  protected String markUnmovedText = ""; //$NON-NLS-1$
  protected String markUnmovedTooltip = Resources.getString("Map.mark_unmoved"); //$NON-NLS-1$
  protected MouseListener multicaster = null;
  protected ArrayList<MouseListener> mouseListenerStack = new ArrayList<>(); //NOPMD
  protected List<Board> boards = new CopyOnWriteArrayList<>();
  protected int[][] boardWidths; // Cache of board widths by row/column
  protected int[][] boardHeights; // Cache of board heights by row/column
  protected PieceCollection pieces = new DefaultPieceCollection(); // All the pieces on the map, but sorted into visual layers. Will be replaced by a LayeredPieceCollection if Map has a "Game Piece Layers" Component.
  protected Highlighter highlighter = new ColoredBorder();
  protected ArrayList<Highlighter> highlighters = new ArrayList<>(); //NOPMD
  protected boolean clearFirst = false; // Whether to clear the display before
  // drawing the map
  protected boolean hideCounters = false; // Option to hide counters to see
  // map
  protected float pieceOpacity = 1.0f;
  protected boolean allowMultiple = false;
  protected VisibilityCondition visibilityCondition;
  protected DragGestureListener dragGestureListener;
  protected String moveWithinFormat;
  protected String moveToFormat;
  protected String createFormat;
  protected String changeFormat = "$" + MESSAGE + "$"; //$NON-NLS-1$ //$NON-NLS-2$
  protected NamedKeyStroke moveKey;
  protected String tooltip = ""; //$NON-NLS-1$
  protected MutablePropertiesContainer propsContainer = new MutablePropertiesContainer.Impl();
  protected PropertyChangeListener repaintOnPropertyChange = evt -> repaint();
  protected PieceMover pieceMover;
  protected KeyListener[] saveKeyListeners = null;

  private IntConfigurer preferredScrollConfig;

  public Map() {
    getView();
    theMap.addMouseListener(this);
    if (shouldDockIntoMainWindow()) {
      final String constraints =
        (SystemUtils.IS_OS_MAC ? "ins 1 0 1 0" : "ins 0") +   //NON-NLS
        ",gapx 0,hidemode 3";                                     //NON-NLS
      toolBar.setLayout(new MigLayout(constraints));
    }
    else {
      toolBar.setLayout(new WrapLayout(WrapLayout.LEFT, 0, 0));
    }
    toolBar.setAlignmentX(0.0F);
    toolBar.setFloatable(false);
  }

  /**
   * @return Map's main visual interface swing component (its JPanel)
   */
  @Override
  public Component getComponent() {
    return theMap;
  }

  /**
   * Global Change Reporting control - used by Global Key Commands (see {@link GlobalCommand}) to
   * temporarily disable reporting while they run, if their "Suppress individual reports" option is selected.
   * @param b true to turn global change reporting on, false to turn it off.
   */
  public static void setChangeReportingEnabled(boolean b) {
    changeReportingEnabled = b;
  }

  /**
   * @return true if change reporting is currently enabled, false if it is presently being suppressed by a Global Key Command.
   */
  public static boolean isChangeReportingEnabled() {
    return changeReportingEnabled;
  }

  public static final String NAME = "mapName"; //$NON-NLS-1$
  public static final String MARK_MOVED = "markMoved"; //$NON-NLS-1$
  public static final String MARK_UNMOVED_ICON = "markUnmovedIcon"; //$NON-NLS-1$
  public static final String MARK_UNMOVED_TEXT = "markUnmovedText"; //$NON-NLS-1$
  public static final String MARK_UNMOVED_TOOLTIP = "markUnmovedTooltip"; //$NON-NLS-1$
  public static final String EDGE_WIDTH = "edgeWidth"; //$NON-NLS-1$
  public static final String EDGE_HEIGHT = "edgeHeight"; //$NON-NLS-1$
  public static final String BACKGROUND_COLOR = "backgroundcolor"; //NON-NLS
  public static final String HIGHLIGHT_COLOR = "color"; //$NON-NLS-1$
  public static final String HIGHLIGHT_THICKNESS = "thickness"; //$NON-NLS-1$
  public static final String ALLOW_MULTIPLE = "allowMultiple"; //$NON-NLS-1$
  public static final String USE_LAUNCH_BUTTON = "launch"; //$NON-NLS-1$
  public static final String BUTTON_NAME = "buttonName"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String SUPPRESS_AUTO = "suppressAuto"; //$NON-NLS-1$
  public static final String MOVE_WITHIN_FORMAT = "moveWithinFormat"; //$NON-NLS-1$
  public static final String MOVE_TO_FORMAT = "moveToFormat"; //$NON-NLS-1$
  public static final String CREATE_FORMAT = "createFormat"; //$NON-NLS-1$
  public static final String CHANGE_FORMAT = "changeFormat"; //$NON-NLS-1$
  public static final String MOVE_KEY = "moveKey"; //$NON-NLS-1$
  public static final String MOVING_STACKS_PICKUP_UNITS = "movingStacksPickupUnits"; //$NON-NLS-1$

  /**
   * Sets a buildFile (XML) attribute value for this component.
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @param value If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   *              <code>key</code>. Since Map extends {@link AbstractConfigurable}, then <code>value</code> can also be an instance of
   *              the corresponding Class listed in {@link #getAttributeTypes}.
   */
  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setMapName((String) value);
    }
    else if (MARK_MOVED.equals(key)) {
      markMovedOption = (String) value;
    }
    else if (MARK_UNMOVED_ICON.equals(key)) {
      markUnmovedIcon = (String) value;
      if (pieceMover != null) {
        pieceMover.setAttribute(key, value);
      }
    }
    else if (MARK_UNMOVED_TEXT.equals(key)) {
      markUnmovedText = (String) value;
      if (pieceMover != null) {
        pieceMover.setAttribute(key, value);
      }
    }
    else if (MARK_UNMOVED_TOOLTIP.equals(key)) {
      markUnmovedTooltip = (String) value;
    }
    else if ("edge".equals(key)) { // Backward-compatible //$NON-NLS-1$
      final String s = (String) value;
      final int i = s.indexOf(','); //$NON-NLS-1$
      if (i > 0) {
        edgeBuffer = new Dimension(Integer.parseInt(s.substring(0, i)), Integer.parseInt(s.substring(i + 1)));
      }
    }
    else if (EDGE_WIDTH.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      try {
        edgeBuffer = new Dimension((Integer) value, edgeBuffer.height);
      }
      catch (NumberFormatException ex) {
        throw new IllegalBuildException(ex);
      }
    }
    else if (EDGE_HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      try {
        edgeBuffer = new Dimension(edgeBuffer.width, (Integer) value);
      }
      catch (NumberFormatException ex) {
        throw new IllegalBuildException(ex);
      }
    }
    else if (BACKGROUND_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String)value);
      }
      bgColor = (Color)value;
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      allowMultiple = (Boolean) value;
      if (picker != null) {
        picker.setAllowMultiple(allowMultiple);
      }
    }
    else if (HIGHLIGHT_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      if (value != null) {
        ((ColoredBorder) highlighter).setColor((Color) value);
      }
    }
    else if (HIGHLIGHT_THICKNESS.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      if (highlighter instanceof ColoredBorder) {
        ((ColoredBorder) highlighter).setThickness((Integer) value);
      }
    }
    else if (USE_LAUNCH_BUTTON.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      useLaunchButtonEdit = (Boolean) value;
      getLaunchButton().setVisible(useLaunchButton);
    }
    else if (SUPPRESS_AUTO.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      if (Boolean.TRUE.equals(value)) {
        moveWithinFormat = ""; //$NON-NLS-1$
      }
    }
    else if (MOVE_WITHIN_FORMAT.equals(key)) {
      moveWithinFormat = (String) value;
    }
    else if (MOVE_TO_FORMAT.equals(key)) {
      moveToFormat = (String) value;
    }
    else if (CREATE_FORMAT.equals(key)) {
      createFormat = (String) value;
    }
    else if (CHANGE_FORMAT.equals(key)) {
      changeFormat = (String) value;
    }
    else if (MOVE_KEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      moveKey = (NamedKeyStroke) value;
    }
    else if (TOOLTIP.equals(key)) {
      tooltip = (String) value;
      getLaunchButton().setAttribute(key, value);
    }
    else {
      super.setAttribute(key, value);
    }
  }

  /**
   * @return a String representation of the XML buildFile attribute with the given name. When initializing a module,
   * this String value will loaded from the XML and passed to {@link #setAttribute}. It is also frequently used for
   * checking the current value of an attribute.
   *
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   */
  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getMapName();
    }
    else if (MARK_MOVED.equals(key)) {
      return markMovedOption;
    }
    else if (MARK_UNMOVED_ICON.equals(key)) {
      return markUnmovedIcon;
    }
    else if (MARK_UNMOVED_TEXT.equals(key)) {
      return markUnmovedText;
    }
    else if (MARK_UNMOVED_TOOLTIP.equals(key)) {
      return markUnmovedTooltip;
    }
    else if (EDGE_WIDTH.equals(key)) {
      return String.valueOf(edgeBuffer.width); //$NON-NLS-1$
    }
    else if (EDGE_HEIGHT.equals(key)) {
      return String.valueOf(edgeBuffer.height); //$NON-NLS-1$
    }
    else if (BACKGROUND_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(bgColor);
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      return String.valueOf(picker.isAllowMultiple()); //$NON-NLS-1$
    }
    else if (HIGHLIGHT_COLOR.equals(key)) {
      if (highlighter instanceof ColoredBorder) {
        return ColorConfigurer.colorToString(
          ((ColoredBorder) highlighter).getColor());
      }
      else {
        return null;
      }
    }
    else if (HIGHLIGHT_THICKNESS.equals(key)) {
      if (highlighter instanceof ColoredBorder) {
        return String.valueOf(
          ((ColoredBorder) highlighter).getThickness()); //$NON-NLS-1$
      }
      else {
        return null;
      }
    }
    else if (USE_LAUNCH_BUTTON.equals(key)) {
      return String.valueOf(useLaunchButtonEdit);
    }
    else if (MOVE_WITHIN_FORMAT.equals(key)) {
      return getMoveWithinFormat();
    }
    else if (MOVE_TO_FORMAT.equals(key)) {
      return getMoveToFormat();
    }
    else if (CREATE_FORMAT.equals(key)) {
      return getCreateFormat();
    }
    else if (CHANGE_FORMAT.equals(key)) {
      return getChangeFormat();
    }
    else if (MOVE_KEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(moveKey);
    }
    else if (TOOLTIP.equals(key)) {
      return (tooltip == null || tooltip.length() == 0)
        ? getLaunchButton().getAttributeValueString(name) : tooltip;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  /**
   * Builds the map's component hierarchy from a given XML element, or a null one is given initializes
   * a brand new default "new map" hierarchy.
   * @param e XML element to build from, or null to build the default hierarchy for a brand new Map
   */
  @Override
  public void build(Element e) {
    setButtonTextKey(BUTTON_NAME); // Uses non-standard "button text" key

    setLaunchButton(makeLaunchButton(
      "", //NON-NLS
      Resources.getString("Editor.Map.map"), //NON-NLS
      "/images/map.gif", //NON-NLS
      evt -> {
        if (splitPane == null && getLaunchButton().isEnabled()) {
          final Container tla = theMap.getTopLevelAncestor();
          if (tla != null) {
            tla.setVisible(!tla.isVisible());
          }
        }
      }
    ));
    launchButton = getLaunchButton(); // for compatibility

    getLaunchButton().setEnabled(false);
    getLaunchButton().setVisible(false);

    if (e != null) {
      super.build(e);
      getBoardPicker();
      getStackMetrics();
    }
    else {
      getBoardPicker();
      getStackMetrics();
      addChild(new ForwardToKeyBuffer());
      addChild(new Scroller());
      addChild(new ForwardToChatter());
      addChild(new MenuDisplayer());
      addChild(new MapCenterer());
      addChild(new StackExpander());
      addChild(new PieceMover());
      addChild(new KeyBufferer());
      addChild(new ImageSaver());
      addChild(new CounterDetailViewer());
      addChild(new Flare());
      setMapName(Resources.getString("Map.main_map"));
    }

    if (getComponentsOf(GlobalProperties.class).isEmpty()) {
      addChild(new GlobalProperties());
    }

    if (getComponentsOf(SelectionHighlighters.class).isEmpty()) {
      addChild(new SelectionHighlighters());
    }

    if (getComponentsOf(HighlightLastMoved.class).isEmpty()) {
      addChild(new HighlightLastMoved());
    }

    if (getComponentsOf(Flare.class).isEmpty()) {
      addChild(new Flare());
    }

    setup(false);
  }

  /**
   * Adds a child component to this map. Used by {@link #build} to create "default components" for a new map object
   * @param b {@link Buildable} component to add
   */
  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
  }

  /**
   * Every map must include a single {@link BoardPicker} as one of its build components. This will contain
   * the map's {@link Board} (or Boards), which will in turn contain any grids, zones, and location
   * information.
   * @param picker BoardPicker to register to the map. This method unregisters any previous BoardPicker.
   */
  public void setBoardPicker(BoardPicker picker) {
    if (this.picker != null) {
      GameModule.getGameModule().removeCommandEncoder(picker);
      GameModule.getGameModule().getGameState().addGameComponent(picker);
    }
    this.picker = picker;
    if (picker != null) {
      picker.setAllowMultiple(allowMultiple);
      GameModule.getGameModule().addCommandEncoder(picker);
      GameModule.getGameModule().getGameState().addGameComponent(picker);
    }
  }

  /**
   * Every map must include a {@link BoardPicker} as one of its build components. This contains
   * the map's {@link Board} (or Boards), which will in turn contain any grids, zones, and location
   * information.
   * @return the BoardPicker for this map (if none exist, this method will add one and return it)
   */
  public BoardPicker getBoardPicker() {
    if (picker == null) {
      picker = new BoardPicker();
      picker.build(null);
      add(picker);
      picker.addTo(this);
    }
    return picker;
  }

  /**
   * A map may include a single {@link Zoomer} as one of its build components. This adds zoom in/out capability to the map.
   * @param z {@link Zoomer} to register
   */
  public void setZoomer(Zoomer z) {
    zoom = z;
  }

  /**
   * A map may include a {@link Zoomer} as one of its build components. This adds zoom in/out capability to the map.
   * @return the Zoomer for this map, if one is registered, or null if none.
   */
  public Zoomer getZoomer() {
    return zoom;
  }

  /**
   * If the map has a {@link Zoomer} (see {@link #setZoomer}), then returns the Zoomer's current zoom factor. If no
   * Zoomer exists, returns 1.0 as the zoom factor.
   * @return the current zoom factor for the map
   */
  public double getZoom() {
    return zoom == null ? 1.0 : zoom.getZoomFactor();
  }

  /**
   * Every map must include a single {@link StackMetrics} as one of its build components, which governs the visuals
   * of stacking of GamePieces on the map.
   * @param sm {@link StackMetrics} component to register
   */
  public void setStackMetrics(StackMetrics sm) {
    metrics = sm;
  }

  /**
   * Every map must include a single {@link StackMetrics} object as one of its build
   * components, which governs the visuals of stacking of GamePieces on the map
   *
   * @return the StackMetrics for this map
   */
  public StackMetrics getStackMetrics() {
    if (metrics == null) {
      metrics = new StackMetrics();
      metrics.build(null);
      add(metrics);
      metrics.addTo(this);
    }
    return metrics;
  }

  /**
   * Every map must include a single {@link PieceMover} component as one of its build
   * components, which handles drag-and-drop behavior for the map.
   * @param mover {@link PieceMover} component to register
   */
  public void setPieceMover(PieceMover mover) {
    pieceMover = mover;
  }


  /**
   * Every map window has a toolbar, and this method returns swing toolbar component for this map.
   * @return the swing toolbar component ({@link JToolBar} for this map's window.
   */
  @Override
  public JToolBar getToolBar() {
    return toolBar;
  }

  /**
   * Registers a {@link Drawable} component to this map. Components can implement the {@link Drawable} interface (and register
   * themselves here) if they have a graphical component that should be drawn whenever the Map is drawn. Standard examples
   * include {@link CounterDetailViewer}s (aka Mouse-over Stack Viewers), {@link GlobalMap}s (aka Overview Maps), {@link LOS_Thread}s,
   * {@link MapShader}s, and the {@link KeyBufferer} (to show which pieces are selected).
   */
  public void addDrawComponent(Drawable theComponent) {
    drawComponents.add(theComponent);
  }

  /**
   * Unregister a {@link Drawable} component from this map
   */
  public void removeDrawComponent(Drawable theComponent) {
    drawComponents.remove(theComponent);
  }

  /**
   * Registers this Map as a child of another buildable component, usually the {@link GameModule}. Determines a unique id for
   * this Map. Registers itself as {@link KeyStrokeSource}. Registers itself as a {@link GameComponent}. Registers itself as
   * a drop target and drag source. If the map is to be removed or otherwise shutdown, it can be deregistered, reversing this
   * process, by {@link #removeFrom}
   *
   * @see #getId
   * @see DragBuffer
   */
  @Override
  public void addTo(Buildable b) {
    useLaunchButton = useLaunchButtonEdit;
    idMgr.add(this);

    final GameModule g = GameModule.getGameModule();
    g.addCommandEncoder(new ChangePropertyCommandEncoder(this));

    validator = new CompoundValidityChecker(
      new MandatoryComponent(this, BoardPicker.class),
      new MandatoryComponent(this, StackMetrics.class)).append(idMgr);

    final DragGestureListener dgl = dge -> {
      if (dragGestureListener != null &&
          mouseListenerStack.isEmpty() &&
          SwingUtils.isDragTrigger(dge)) {
        dragGestureListener.dragGestureRecognized(dge);
      }
    };

    DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(
      theMap, DnDConstants.ACTION_MOVE, dgl);
    theMap.setDropTarget(PieceMover.DragHandler.makeDropTarget(
      theMap, DnDConstants.ACTION_MOVE, this));
    g.getGameState().addGameComponent(this);
    g.getToolBar().add(getLaunchButton());

    if (shouldDockIntoMainWindow()) {
      final Component controlPanel = g.getControlPanel();
      final Container cppar = controlPanel.getParent();
      final int i = SwingUtils.getIndexInParent(controlPanel, cppar);

      splitPane = new SplitPane(SplitPane.VERTICAL_SPLIT, controlPanel, layeredPane);
      splitPane.setResizeWeight(0.0);
      splitPane.hideBottom();

      cppar.add(splitPane, i);

      g.addKeyStrokeSource(new KeyStrokeSource(theMap, JComponent.WHEN_FOCUSED));
    }
    else {
      g.addKeyStrokeSource(
        new KeyStrokeSource(theMap, JComponent.WHEN_IN_FOCUSED_WINDOW));
    }

    // Fix for bug 1630993: toolbar buttons not appearing
    toolBar.addHierarchyListener(new HierarchyListener() {
      @Override
      public void hierarchyChanged(HierarchyEvent e) {
        final Window w;
        if ((w = SwingUtilities.getWindowAncestor(toolBar)) != null) {
          w.validate();
        }
        if (toolBar.getSize().width > 0) {
          toolBar.removeHierarchyListener(this);
        }
      }
    });

    g.getPrefs().addOption(
      Resources.getString("Prefs.general_tab"), //$NON-NLS-1$
      new IntConfigurer(
        PREFERRED_EDGE_DELAY,
        Resources.getString("Map.scroll_delay_preference"), //$NON-NLS-1$
        PREFERRED_EDGE_SCROLL_DELAY
      )
    );

    g.addSideChangeListenerToPlayerRoster(this);

    // Create the Configurer for the Pref
    preferredScrollConfig = new IntConfigurer(
      PREFERRED_SCROLL_ZONE,
      Resources.getString("Map.scroll_zone_preference"), //$NON-NLS-1$
      SCROLL_ZONE
    );

    // Register the Pref, which copies any existing pref value into the configurer
    g.getPrefs().addOption(
      Resources.getString("Prefs.general_tab"), //$NON-NLS-1$
      preferredScrollConfig
    );

    // Read the current value of the pref
    SCROLL_ZONE = preferredScrollConfig.getIntValue(60);

    // Listen for any changes to the pref
    preferredScrollConfig.addPropertyChangeListener(evt -> SCROLL_ZONE = preferredScrollConfig.getIntValue(60));

    g.getPrefs().addOption(
      Resources.getString("Prefs.compatibility_tab"), //$NON-NLS-1$
      new BooleanConfigurer(
        MOVING_STACKS_PICKUP_UNITS,
        Resources.getString("Map.moving_stacks_preference"), //$NON-NLS-1$
        Boolean.FALSE
      )
    );
  }

  /**
   * Unregisters this Map from its {@link Buildable} parent (usually a {@link GameModule}), reversing
   * the process of {@link #addTo}.
   * @param b parent {@link Buildable} to deregister from
   */
  @Override
  public void removeFrom(Buildable b) {
    final GameModule g = GameModule.getGameModule();
    g.getGameState().removeGameComponent(this);

    final Window w = SwingUtilities.getWindowAncestor(theMap);
    if (w != null) {
      w.dispose();
    }

    g.getToolBar().remove(getLaunchButton());
    idMgr.remove(this);
    if (picker != null) {
      g.removeCommandEncoder(picker);
      g.getGameState().addGameComponent(picker);
    }
    g.addSideChangeListenerToPlayerRoster(this);
  }

  /**
   * Takes action when the local player has switched sides. Because Map implements {@link PlayerRoster.SideChangeListener},
   * this method will automatically be called whenever the local player switches sides.
   * @param oldSide side the local player is switching away from
   * @param newSide side the local player is switching to
   */
  @Override
  public void sideChanged(String oldSide, String newSide) {
    repaint();
  }

  /**
   * Set the boards for this map. Each map may contain more than one
   * {@link Board}.
   * @param c Collection of Boards to be used.
   */
  public synchronized void setBoards(Collection<Board> c) {
    boards.clear();
    for (final Board b : c) {
      b.setMap(this);
      boards.add(b);
    }
    setBoardBoundaries();
  }

  /**
   * Set the boards for this map. Each map may contain more than one
   * {@link Board}.
   * @deprecated Use {@link #setBoards(Collection)} instead.
   */
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public synchronized void setBoards(Enumeration<Board> boardList) {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
    setBoards(Collections.list(boardList));
  }

  /**
   * Since a map can have multiple boards in use at once (laid out above and beside each other), this
   * method accepts a {@link Point} in the map's coordinate space and will return the {@link Board} which
   * contains that point, or null if none.
   * @return the {@link Board} on this map containing the argument point
   */
  public Board findBoard(Point p) {
    for (final Board b : boards) {
      if (b.bounds().contains(p)) {
        return b;
      }
    }
    return null;
  }

  /**
   * If the given point in the map's coordinate space is within a {@link Zone} on a board with a
   * {@link ZonedGrid} (aka Multi-zoned Grid), returns the Zone. Otherwise returns null.
   * @return the {@link Zone} on this map containing the argument point
   */
  public Zone findZone(Point p) {
    final Board b = findBoard(p);
    if (b != null) {
      final MapGrid grid = b.getGrid();
      if (grid instanceof ZonedGrid) {
        final Rectangle r = b.bounds();
        final Point pos = new Point(p);
        pos.translate(-r.x, -r.y);  // Translate to Board co-ords
        return ((ZonedGrid) grid).findZone(pos);
      }
    }
    return null;
  }

  /**
   * Search on all boards for a Zone with the given name
   * @param name Zone Name
   * @return Located zone, or null if not found
   */
  public Zone findZone(String name) {
    for (final Board b : boards) {
      for (final ZonedGrid zg : b.getAllDescendantComponentsOf(ZonedGrid.class)) {
        final Zone z = zg.findZone(name);
        if (z != null) {
          return z;
        }
      }
    }
    return null;
  }

  /**
   * Search on all boards for a Region (location on an Irregular Grid) with the given name
   * @param name Region name
   * @return Located region, or null if none
   */
  public Region findRegion(String name) {
    for (final Board b : boards) {
      for (final RegionGrid rg : b.getAllDescendantComponentsOf(RegionGrid.class)) {
        final Region r = rg.findRegion(name);
        if (r != null) {
          return r;
        }
      }
    }
    return null;
  }

  /**
   * Searches our list of boards for one with the given name
   * @param name Board Name
   * @return located board, or null if no such board found
   */
  public Board getBoardByName(String name) {
    if (name != null) {
      for (final Board b : boards) {
        if (name.equals(b.getName())) {
          return b;
        }
      }
    }
    return null;
  }

  /**
   * @return Dimension for map window's "preferred size"
   */
  public Dimension getPreferredSize() {
    final Dimension size = mapSize();
    size.width *= getZoom();
    size.height *= getZoom();
    return size;
  }

  /**
   * @return the size of the map in pixels at 100% zoom,
   * including the edge buffer
   */
// FIXME: why synchronized?
  public synchronized Dimension mapSize() {
    final Rectangle r = new Rectangle(0, 0);
    for (final Board b : boards) r.add(b.bounds());
    r.width += edgeBuffer.width;
    r.height += edgeBuffer.height;
    return r.getSize();
  }

  /**
   * @return true if the given point may not be a legal location. I.e., if this grid will attempt to snap it to the
   *         nearest grid location
   */
  public boolean isLocationRestricted(Point p) {
    final Board b = findBoard(p);
    if (b != null) {
      final Rectangle r = b.bounds();
      final Point snap = new Point(p);
      snap.translate(-r.x, -r.y);
      return b.isLocationRestricted(snap);
    }
    else {
      return false;
    }
  }

  /**
   * @return the nearest allowable point according to the {@link VASSAL.build.module.map.boardPicker.board.MapGrid} on
   *         the {@link Board} at this point
   *
   * @see Board#snapTo
   * @see VASSAL.build.module.map.boardPicker.board.MapGrid#snapTo
   */
  public Point snapTo(Point p) {
    Point snap = new Point(p);

    final Board b = findBoard(p);
    if (b == null) return snap;

    final Rectangle r = b.bounds();
    snap.translate(-r.x, -r.y);
    snap = b.snapTo(snap);
    snap.translate(r.x, r.y);

    //CC bugfix13409
    // If we snapped to a point outside the board b, call sanpTo again with the board we landed into
    final Board bSnappedTo = findBoard(snap);
    if (bSnappedTo != null && !b.equals(bSnappedTo)) {
      final Rectangle rSnappedTo = bSnappedTo.bounds();
      snap.translate(-rSnappedTo.x, -rSnappedTo.y);
      snap = bSnappedTo.snapTo(snap);
      snap.translate(rSnappedTo.x, rSnappedTo.y);
    }
    // RFE 882378
    // If we have snapped to a point 1 pixel off the edge of the map, move
    // back
    // onto the map.
    if (findBoard(snap) == null) {
      snap.translate(-r.x, -r.y);
      if (snap.x == r.width) {
        snap.x = r.width - 1;
      }
      else if (snap.x == -1) {
        snap.x = 0;
      }
      if (snap.y == r.height) {
        snap.y = r.height - 1;
      }
      else if (snap.y == -1) {
        snap.y = 0;
      }
      snap.translate(r.x, r.y);
    }
    return snap;
  }

  /**
   * @return The buffer of empty space around the boards in the Map window,
   * in component coordinates at 100% zoom
   */
  public Dimension getEdgeBuffer() {
    return new Dimension(edgeBuffer);
  }

  /**
   * Translate a point from component coordinates (i.e., x,y position on
   * the JPanel) to map coordinates (i.e., accounting for zoom factor).
   *
   * @see #componentCoordinates
   * @deprecated Use {@link #componentToMap(Point)}
   */
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public Point mapCoordinates(Point p) {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
    return componentToMap(p);
  }

  /** @deprecated Use {@link #componentToMap(Rectangle)} */
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public Rectangle mapRectangle(Rectangle r) {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
    return componentToMap(r);
  }

  /**
   * Translate a point from map coordinates to component coordinates
   *
   * @see #mapCoordinates
   * @deprecated {@link #mapToComponent(Point)}
   */
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public Point componentCoordinates(Point p) {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
    return mapToComponent(p);
  }

  /** @deprecated  Use {@link #mapToComponent(Rectangle)} */
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public Rectangle componentRectangle(Rectangle r) {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
    return mapToComponent(r);
  }

  /**
   * Scales an integer value to a zoom factor
   * @param c value to be scaled
   * @param zoom zoom factor
   * @return scaled value result
   */
  protected int scale(int c, double zoom) {
    return (int)(c * zoom);
  }

  /**
   * Scales a point to a zoom factor
   * @param p point to be scaled
   * @param zoom zoom factor
   * @return scaled point result
   */
  protected Point scale(Point p, double zoom) {
    return new Point((int)(p.x * zoom), (int)(p.y * zoom));
  }

  /**
   * Scales a Rectangle to a zoom factor
   * @param r Rectangle to be zoomed
   * @param zoom zoom factor
   * @return scaled Rectangle result
   */
  protected Rectangle scale(Rectangle r, double zoom) {
    return new Rectangle(
      (int)(r.x * zoom),
      (int)(r.y * zoom),
      (int)(r.width * zoom),
      (int)(r.height * zoom)
    );
  }

  /**
   * Converts an integer value from the Map's coordinate system to Drawing coordinates for rendering. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors) as well as the Map's zoom
   * factor. Although Drawing coordinates may <i>sometimes</i> have the traditional 1-to-1 relationship with component
   * coordinates, on HiDPI monitors it will not.
   *
   * Examples: Drawing a line between two points on a map (see {@link LOS_Thread#draw}. Drawing a piece on the map
   *           (see {@link StackMetrics#draw}.
   *
   * @param c value in Map coordinate space to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled value in Drawing coordinate space
   */
  public int mapToDrawing(int c, double os_scale) {
    return scale(c, getZoom() * os_scale);
  }

  /**
   * Converts a point from the Map's coordinate system to Drawing coordinates for rendering. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors) as well as the Map's zoom
   * factor. Although Drawing coordinates may <i>sometimes</i> have the traditional 1-to-1 relationship with component
   * coordinates, on HiDPI monitors it will not.
   *
   * Examples: Drawing a line between two points on a map (see {@link LOS_Thread#draw}. Drawing a piece on the map
   *           (see {@link StackMetrics#draw}.
   *
   * @param p point in Map coordinates to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled point in Drawing coordinates
   */
  public Point mapToDrawing(Point p, double os_scale) {
    return scale(p, getZoom() * os_scale);
  }

  /**
   * Converts a rectangle from the Map's coordinate system to Drawing coordinates for rendering. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors) as well as the Map's zoom
   * factor. Although Drawing coordinates may <i>sometimes</i> have the traditional 1-to-1 relationship with component
   * coordinates, on HiDPI monitors it will not.
   *
   * Examples: Drawing a line between two points on a map (see {@link LOS_Thread#draw}. Drawing a piece on the map
   *           (see {@link StackMetrics#draw}.
   *
   * @param r rectangle in Map coordinates to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled rectangle in Drawing coordinates
   */
  public Rectangle mapToDrawing(Rectangle r, double os_scale) {
    return scale(r, getZoom() * os_scale);
  }

  /**
   * Converts an integer value from the Map's coordinate system to Component coordinates used for interactions between
   * swing components. Basically this scales by the map's zoom factor. Note that although drawing coordinates may
   * sometimes have the traditional 1-to-1 relationship with component coordinates, on HiDPI monitors it will not.
   *
   * Examples: activating a popup menu at a piece's location on a map (see MenuDisplayer#maybePopup). Drag and
   * drop operations (see dragGestureRecognizedPrep in {@link PieceMover}).
   *
   * @param c value in Map coordinate system to scale
   * @return scaled value in Component coordinate system
   */
  public int mapToComponent(int c) {
    return scale(c, getZoom());
  }

  /**
   * Converts a Point from the Map's coordinate system to Component coordinates used for interactions between
   * swing components. Basically this scales by the map's zoom factor. Note that although drawing coordinates may
   * sometimes have the traditional 1-to-1 relationship with component coordinates, on HiDPI monitors it will not.
   *
   * Examples: activating a popup menu at a piece's location on a map (see MenuDisplayer#maybePopup). Drag and
   * drop operations (see dragGestureRecognizedPrep in {@link PieceMover}).
   *
   * @param p Point in Map coordinates to scale
   * @return scaled Point in Component coordinates
   */
  public Point mapToComponent(Point p) {
    return scale(p, getZoom());
  }

  /**
   * Converts a Rectangle from the Map's coordinate system to Component coordinates used for interactions between
   * swing components. Basically this scales by the map's zoom factor. Note that although drawing coordinates may
   * sometimes have the traditional 1-to-1 relationship with component coordinates, on HiDPI monitors it will not.
   *
   * Examples: activating a popup menu at a piece's location on a map (see MenuDisplayer#maybePopup). Drag and
   * drop operations (see dragGestureRecognizedPrep in {@link PieceMover}).
   *
   * @param r Rectangle in Map coordinates to scale
   * @return scaled Rectangle in Component coordinates
   */
  public Rectangle mapToComponent(Rectangle r) {
    return scale(r, getZoom());
  }

  /**
   * Converts an integer value from Component coordinates system to Drawing coordinates for rendering. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors), which accounts entirely for
   * the difference in these two coordinate systems. Although Drawing coordinates may <i>sometimes</i> have the
   * traditional 1-to-1 relationship with Component coordinates, on HiDPI monitors it will not.
   *
   * Examples: see {@link VASSAL.counters.Footprint#draw} - checking a map component's "visible" clipping rect, and
   * using it in the context of drawing move trails.
   *
   * @param c value in Component coordinate space to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled value in Drawing coordinate space
   */
  public int componentToDrawing(int c, double os_scale) {
    return scale(c, os_scale);
  }

  /**
   * Converts a Point from Component coordinates to Drawing coordinates for rendering. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors), which accounts entirely for
   * the difference in these two coordinate systems. Although Drawing coordinates may <i>sometimes</i> have the
   * traditional 1-to-1 relationship with Component coordinates, on HiDPI monitors it will not.
   *
   * Examples: see {@link VASSAL.counters.Footprint#draw} - checking a map component's "visible" clipping rect, and
   * using it in the context of drawing move trails.
   *
   * @param p Point in Component coordinate space to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled Point in Drawing coordinate space
   */
  public Point componentToDrawing(Point p, double os_scale) {
    return scale(p, os_scale);
  }

  /**
   * Converts a Rectangle from Component coordinates to Drawing coordinates for rendering. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors), which accounts entirely for
   * the difference in these two coordinate systems. Although Drawing coordinates may <i>sometimes</i> have the
   * traditional 1-to-1 relationship with Component coordinates, on HiDPI monitors it will not.
   *
   * Examples: see {@link VASSAL.counters.Footprint#draw} - checking a map component's "visible" clipping rect, and
   * using it in the context of drawing move trails.
   *
   * @param r Rectangle in Component coordinate space to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled Rectangle in Drawing coordinate space
   */
  public Rectangle componentToDrawing(Rectangle r, double os_scale) {
    return scale(r, os_scale);
  }

  /**
   * Converts an integer value from swing Component coordinates to the Map's coordinate system. Basically this scales by the
   * inverse of the map's zoom factor. Note that although drawing coordinates may sometimes have the traditional 1-to-1 relationship
   * with component coordinates, on HiDPI monitors it will not.
   *
   * Examples: Checking if the mouse is currently overlapping a game piece {@link KeyBufferer#mouseReleased(MouseEvent)},
   * CounterDetailViewer#getDisplayablePieces.
   *
   * @param c value in Component coordinates to scale
   * @return scaled value in Map coordinates
   */
  public int componentToMap(int c) {
    return scale(c, 1.0 / getZoom());
  }

  /**
   * Converts a Point from swing Component coordinates to the Map's coordinate system. Basically this scales by the
   * inverse of the map's zoom factor. Note that although drawing coordinates may sometimes have the traditional 1-to-1 relationship
   * with component coordinates, on HiDPI monitors it will not.
   *
   * Examples: Checking if the mouse is currently overlapping a game piece {@link KeyBufferer#mouseReleased(MouseEvent)},
   * CounterDetailViewer#getDisplayablePieces.
   *
   * @param p Point in Component coordinates to scale
   * @return scaled Point in Map coordinates
   */
  public Point componentToMap(Point p) {
    return scale(p, 1.0 / getZoom());
  }

  /**
   * Converts a Rectangle from swing Component coordinates to the Map's coordinate system. Basically this scales by the
   * inverse of the map's zoom factor. Note that although drawing coordinates may sometimes have the traditional 1-to-1 relationship
   * with component coordinates, on HiDPI monitors it will not.
   *
   * Examples: Checking if the mouse is currently overlapping a game piece {@link KeyBufferer#mouseReleased(MouseEvent)},
   * CounterDetailViewer#getDisplayablePieces.
   *
   * @param r Rectangle in Component coordinates to scale
   * @return scaled Rectangle in Map coordinates
   */
  public Rectangle componentToMap(Rectangle r) {
    return scale(r, 1.0 / getZoom());
  }

  /**
   * Converts an integer value from Drawing coordinates to the Map's coordinate system. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors) as well as the Map's zoom
   * factor, scaling by the inverse of both of these scale factors. Although Drawing coordinates may <i>sometimes</i>
   * have the traditional 1-to-1 relationship with component coordinates, on HiDPI monitors it will not.
   *
   * @param c value in Drawing coordinate space to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled value in Map coordinates
   */
  @SuppressWarnings("unused")
  public int drawingToMap(int c, double os_scale) {
    return scale(c, 1.0 / (getZoom() * os_scale));
  }

  /**
   * Converts a Point from Drawing coordinates to the Map's coordinate system. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors) as well as the Map's zoom
   * factor, scaling by the inverse of both of these scale factors. Although Drawing coordinates may <i>sometimes</i>
   * have the traditional 1-to-1 relationship with component coordinates, on HiDPI monitors it will not.
   *
   * @param p Point in Drawing coordinate space to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled point in Map coordinates
   */
  public Point drawingToMap(Point p, double os_scale) {
    return scale(p, 1.0 / (getZoom() * os_scale));
  }

  /**
   * Converts a Rectangle from Drawing coordinates to the Map's coordinate system. Takes into
   * account the operating system's scale factor (needed to deal with HiDPI monitors) as well as the Map's zoom
   * factor, scaling by the inverse of both of these scale factors. Although Drawing coordinates may <i>sometimes</i>
   * have the traditional 1-to-1 relationship with component coordinates, on HiDPI monitors it will not.
   *
   * @param r Rectangle in Drawing coordinate space to be scaled
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled Rectangle in Map coordinates
   */
  public Rectangle drawingToMap(Rectangle r, double os_scale) {
    return scale(r, 1.0 / (getZoom() * os_scale));
  }

  /**
   * Converts an integer value from Drawing coordinates to swing Component coordinates. Takes into account the inverse
   * of the operating system's scale factor (needed to deal with HiDPI monitors), which accounts entirely for the
   * difference in these two coordinate systems. Although Drawing coordinates may <i>sometimes</i> have the traditional
   * 1-to-1 relationship with Component coordinates, on HiDPI monitors it will not.
   *
   * @param c value in Drawing coordinates
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled value in Component coordinates
   */
  public int drawingToComponent(int c, double os_scale) {
    return scale(c, 1.0 / os_scale);
  }

  /**
   * Converts a Point from Drawing coordinates to swing Component coordinates. Takes into account the inverse
   * of the operating system's scale factor (needed to deal with HiDPI monitors), which accounts entirely for the
   * difference in these two coordinate systems. Although Drawing coordinates may <i>sometimes</i> have the traditional
   * 1-to-1 relationship with Component coordinates, on HiDPI monitors it will not.
   *
   * @param p Point in Drawing coordinates
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled Point in Component coordinates
   */
  public Point drawingToComponent(Point p, double os_scale) {
    return scale(p, 1.0 / os_scale);
  }

  /**
   * Converts a Rectangle from Drawing coordinates to swing Component coordinates. Takes into account the inverse
   * of the operating system's scale factor (needed to deal with HiDPI monitors), which accounts entirely for the
   * difference in these two coordinate systems. Although Drawing coordinates may <i>sometimes</i> have the traditional
   * 1-to-1 relationship with Component coordinates, on HiDPI monitors it will not.
   *
   * @param r Rectangle in Drawing coordinates
   * @param os_scale Operating system's scale factor, (obtained from {@link Graphics2D#getDeviceConfiguration().getDefaultTransform().getScaleX()})
   * @return scaled Rectangle in Component coordinates
   */
  public Rectangle drawingToComponent(Rectangle r, double os_scale) {
    return scale(r, 1.0 / os_scale);
  }

  /**
   * @return a String name for the given location on the map. Checks first for a {@link Deck}, then for a {@link Board} that
   * is able to provide a name from one of its grids. If no matches, returns "offboard" string.
   *
   * @see Board#locationName
   */
  public String locationName(Point p) {
    String loc = getDeckNameAt(p);
    if (loc == null) {
      final Board b = findBoard(p);
      if (b != null) {
        loc = b.locationName(new Point(p.x - b.bounds().x, p.y - b.bounds().y));
      }
    }
    if (loc == null) {
      loc = Resources.getString("Map.offboard"); //$NON-NLS-1$
    }
    return loc;
  }

  /**
   * @return a translated-if-available String name for the given location on the map. Checks first for a {@link Deck},
   * then for a {@link Board} that is able to provide a name from one of its grids. If no matches, returns "offboard" string.
   *
   * @see Board#locationName
   */
  public String localizedLocationName(Point p) {
    String loc = getLocalizedDeckNameAt(p);
    if (loc == null) {
      final Board b = findBoard(p);
      if (b != null) {
        loc = b.localizedLocationName(new Point(p.x - b.bounds().x, p.y - b.bounds().y));
      }
    }
    if (loc == null) {
      loc = Resources.getString("Map.offboard"); //$NON-NLS-1$
    }
    return loc;
  }

  /**
   * Is this map visible to all players?
   * @return true if this map either (a) isn't a {@link PrivateMap} or (b) does have its visible-to-all flag set
   */
  @SuppressWarnings("unused")
  public boolean isVisibleToAll() {
    return !(this instanceof PrivateMap) || getAttributeValueString(PrivateMap.VISIBLE).equals("true"); //$NON-NLS-1$
  }

  /**
   * @return the name of the {@link Deck} whose bounding box contains point p
   */
  @SuppressWarnings("unused")
  public String getDeckNameContaining(Point p) {
    String deck = null;
    if (p != null) {
      for (final DrawPile d : getComponentsOf(DrawPile.class)) {
        final Rectangle box = d.boundingBox();
        if (box != null && box.contains(p)) {
          deck = d.getConfigureName();
          break;
        }
      }
    }
    return deck;
  }

  /**
   * Return the name of the {@link Deck} whose position is precisely p
   *
   * @param p Point to look for Deck
   * @return Name of {@link Deck whose position is precisely p
   */
  public String getDeckNameAt(Point p) {
    String deck = null;
    if (p != null) {
      for (final DrawPile d : getComponentsOf(DrawPile.class)) {
        if (d.getPosition().equals(p)) {
          deck = d.getConfigureName();
          break;
        }
      }
    }
    return deck;
  }

  /**
   * Return the localized name of the {@link Deck} whose position is precisely p
   *
   * @param p Point to look for Deck
   * @return Name of {@link Deck whose position is precisely p
   */
  public String getLocalizedDeckNameAt(Point p) {
    String deck = null;
    if (p != null) {
      for (final DrawPile d : getComponentsOf(DrawPile.class)) {
        if (d.getPosition().equals(p)) {
          deck = d.getLocalizedConfigureName();
          break;
        }
      }
    }
    return deck;
  }

  /**
   * Because MouseEvents are received in Component coordinates, it is
   * inconvenient for MouseListeners on the map to have to translate to Map
   * coordinates. MouseListeners added with this method will receive mouse
   * events with points already translated into Map coordinates.
   * addLocalMouseListenerFirst inserts the new listener at the start of the
   * chain.
   * @param l MouseListener to add
   */
  public void addLocalMouseListener(MouseListener l) {
    multicaster = AWTEventMulticaster.add(multicaster, l);
  }

  /**
   * Because MouseEvents are received in Component coordinates, it is
   * inconvenient for MouseListeners on the map to have to translate to Map
   * coordinates. MouseListeners added with this method will receive mouse
   * events with points already translated into Map coordinates.
   * addLocalMouseListenerFirst inserts the new listener at the start of the
   * chain.
   * @param l MouseListener to add
   */
  public void addLocalMouseListenerFirst(MouseListener l) {
    multicaster = AWTEventMulticaster.add(l, multicaster);
  }

  /**
   * Because MouseEvents are received in Component coordinates, it is
   * inconvenient for MouseListeners on the map to have to translate to Map
   * coordinates. MouseListeners added with this method will receive mouse
   * events with points already translated into Map coordinates.
   * addLocalMouseListenerFirst inserts the new listener at the start of the
   * chain.
   * @param l MouseListener to add
   */
  public void removeLocalMouseListener(MouseListener l) {
    multicaster = AWTEventMulticaster.remove(multicaster, l);
  }

  /**
   * MouseListeners on a map may be pushed and popped onto a stack.
   * Only the top listener on the stack receives mouse events.
   * @param l MouseListener to push onto stack.
   */
  public void pushMouseListener(MouseListener l) {
    mouseListenerStack.add(l);
  }

  /**
   * MouseListeners on a map may be pushed and popped onto a stack. Only the top listener on the stack receives mouse
   * events. The pop method removes the most recently pushed mouse listener.
   */
  public void popMouseListener() {
    mouseListenerStack.remove(mouseListenerStack.size() - 1);
  }

  /**
   * Remove the most recently pushed mouse listener only if it is the supplied listener
   * @param l Listener
   */
  public void popMouseListener(MouseListener l) {
    if (!mouseListenerStack.isEmpty() && mouseListenerStack.get(mouseListenerStack.size() - 1).equals(l)) {
      popMouseListener();
    }
  }
  /**
   * @param e MouseEvent
   */
  @Override
  public void mouseEntered(MouseEvent e) {
  }

  /**
   * @param e MouseEvent
   */
  @Override
  public void mouseExited(MouseEvent e) {
  }


  /**
   * Because MouseEvents are received in Component coordinates, it is
   * inconvenient for MouseListeners on the map to have to translate to Map
   * coordinates. MouseListeners added with this method will receive mouse
   * events with points already translated into Map coordinates.
   * addLocalMouseListenerFirst inserts the new listener at the start of the
   * chain.
   * @param e MouseEvent in Component coordinates
   * @return MouseEvent translated into Map coordinates
   */
  public MouseEvent translateEvent(MouseEvent e) {
    // don't write over Java's mouse event
    final MouseEvent mapEvent = new MouseEvent(
      e.getComponent(), e.getID(), e.getWhen(), e.getModifiersEx(),
      e.getX(), e.getY(), e.getXOnScreen(), e.getYOnScreen(),
      e.getClickCount(), e.isPopupTrigger(), e.getButton()
    );
    final Point p = componentToMap(mapEvent.getPoint());
    mapEvent.translatePoint(p.x - mapEvent.getX(), p.y - mapEvent.getY());
    return mapEvent;
  }

  /**
   * Mouse events are first translated into map coordinates. Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   * @param e MouseEvent from system
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
   */
  @Override
  public void mouseClicked(MouseEvent e) {
    if (!mouseListenerStack.isEmpty()) {
      mouseListenerStack.get(mouseListenerStack.size() - 1).mouseClicked(translateEvent(e));
    }
    else if (multicaster != null) {
      multicaster.mouseClicked(translateEvent(e));
    }
  }


  public static Map activeMap = null;

  /**
   * Marks an ActiveMap for certain drag and drop operations, so that the map can be repainted when the operation is
   * complete.
   * @param m Map to be considered active.
   */
  public static void setActiveMap(Map m) {
    activeMap = m;
  }

  /**
   * Repaints the current ActiveMap (see {@link #setActiveMap}) and unmarks it.
   */
  public static void clearActiveMap() {
    if (activeMap != null) {
      activeMap.repaint();
      setActiveMap(null);
    }
  }

  /**
   * Mouse events are first translated into map coordinates. Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   * @param e MouseEvent from system
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
  */
  @Override
  public void mousePressed(MouseEvent e) {
    // Deselect any counters on the last Map with focus
    if (!this.equals(activeMap)) {
      boolean dirty = false;

      final KeyBuffer kbuf = KeyBuffer.getBuffer();
      final ArrayList<GamePiece> l = new ArrayList<>(kbuf.asList());

      for (final GamePiece p : l) {
        if (p.getMap() == activeMap) {
          kbuf.remove(p);
          dirty = true;
        }
      }

      if (dirty && activeMap != null) {
        activeMap.repaint();
      }
    }
    setActiveMap(this);

    if (!mouseListenerStack.isEmpty()) {
      mouseListenerStack.get(mouseListenerStack.size() - 1).mousePressed(translateEvent(e));
    }
    else if (multicaster != null) {
      multicaster.mousePressed(translateEvent(e));
    }
  }

  /**
   * Mouse events are first translated into map coordinates. Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   * @param e MouseEvent from system
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
   */
  @Override
  public void mouseReleased(MouseEvent e) {
    // don't write over Java's mouse event
    final Point p = e.getPoint();
    p.translate(theMap.getX(), theMap.getY());
    if (theMap.getBounds().contains(p)) {
      if (!mouseListenerStack.isEmpty()) {
        mouseListenerStack.get(mouseListenerStack.size() - 1).mouseReleased(translateEvent(e));
      }
      else if (multicaster != null) {
        multicaster.mouseReleased(translateEvent(e));
      }
      // Request Focus so that keyboard input will be recognized
      theMap.requestFocus();
    }
    // Clicking with mouse always repaints the map
    clearFirst = true;
    theMap.repaint();
    activeMap = this;
  }

  /**
   * Save all current Key Listeners and remove them from the
   * map. Used by Traits that need to prevent Key Commands
   * at certain times.
   */
  public void enableKeyListeners() {
    if (saveKeyListeners == null) return;

    for (final KeyListener kl : saveKeyListeners) {
      theMap.addKeyListener(kl);
    }

    saveKeyListeners = null;
  }

  /**
   * Restore the previously disabled KeyListeners
   */
  public void disableKeyListeners() {
    if (saveKeyListeners != null) return;

    saveKeyListeners = theMap.getKeyListeners();
    for (final KeyListener kl : saveKeyListeners) {
      theMap.removeKeyListener(kl);
    }
  }

  /**
   * This listener will be notified when a drag event is initiated, assuming
   * that no MouseListeners are on the stack.
   *
   * @see #pushMouseListener
   * @param dragGestureListener Listener
   */
  public void setDragGestureListener(DragGestureListener dragGestureListener) {
    this.dragGestureListener = dragGestureListener;
  }

  /**
   * @return current dragGestureListener that handles normal drag events (assuming no MouseListeners are on the stack)
   * @see #pushMouseListener
   */
  public DragGestureListener getDragGestureListener() {
    return dragGestureListener;
  }

  /**
   * @param dtde DropTargetDragEvent
   */
  @Override
  public void dragEnter(DropTargetDragEvent dtde) {
  }

  /**
   * Handles scrolling when dragging an gamepiece to the edge of the window
   * @param dtde DropTargetDragEvent
   */
  @Override
  public void dragOver(DropTargetDragEvent dtde) {
    scrollAtEdge(dtde.getLocation(), SCROLL_ZONE);
  }

  /**
   * @param dtde DropTargetDragEvent
   */
  @Override
  public void dropActionChanged(DropTargetDragEvent dtde) {
  }

  /*
   * Cancel final scroll and repaint map
   * @param dtde DropTargetDragEvent
   */
  @Override
  public void dragExit(DropTargetEvent dte) {
    if (scroller.isRunning()) scroller.stop();
    repaint();
  }

  /**
   * We put the "drop" in drag-n-drop!
   * @param dtde DropTargetDragEvent
   */
  @Override
  public void drop(DropTargetDropEvent dtde) {
    if (dtde.getDropTargetContext().getComponent() == theMap) {
      final MouseEvent evt = new MouseEvent(
        theMap,
        MouseEvent.MOUSE_RELEASED,
        System.currentTimeMillis(),
        0,
        dtde.getLocation().x,
        dtde.getLocation().y,
        1,
        false,
        MouseEvent.NOBUTTON
      );
      theMap.dispatchEvent(evt);
      dtde.dropComplete(true);
    }

    if (scroller.isRunning()) scroller.stop();
  }

  /**
   * Mouse motion events are not forwarded to LocalMouseListeners or to listeners on the stack
   * @param e MouseEvent from system
   */
  @Override
  public void mouseMoved(MouseEvent e) {
  }

  /**
   * Mouse motion events are not forwarded to LocalMouseListeners or to
   * listeners on the stack.
   *
   * The map scrolls when dragging the mouse near the edge.
   * @param e MouseEvent from system
   */
  @Override
  public void mouseDragged(MouseEvent e) {
    if (!SwingUtils.isContextMouseButtonDown(e)) {
      scrollAtEdge(e.getPoint(), SCROLL_ZONE);
    }
    else {
      if (scroller.isRunning()) scroller.stop();
    }
  }

  /*
   * Delay before starting scroll at edge
   */
  public static final int PREFERRED_EDGE_SCROLL_DELAY = 200;
  public static final String PREFERRED_EDGE_DELAY = "PreferredEdgeDelay"; //$NON-NLS-1$

  /** The width of the hot zone for triggering autoscrolling. */
  public static int SCROLL_ZONE = 60;
  public static final String PREFERRED_SCROLL_ZONE = "PreferredScrollZone"; //$NON-NLS-1$

  /** The horizontal component of the autoscrolling vector, -1, 0, or 1. */
  protected int sx;
  /** The vertical component of the autoscrolling vector, -1, 0, or 1. */
  protected int sy;

  protected int dx, dy;

  /**
   * Begin autoscrolling the map if the given point is within the given
   * distance from a viewport edge.
   *
   * @param evtPt Point to check
   * @param dist Distance to check
   */
  public void scrollAtEdge(Point evtPt, int dist) {
    final Rectangle vrect = scroll.getViewport().getViewRect();

    final int px = evtPt.x - vrect.x;
    final int py = evtPt.y - vrect.y;

    // determine scroll vector
    sx = 0;
    if (px < dist && px >= 0) {
      sx = -1;
      dx = dist - px;
    }
    else if (px < vrect.width && px >= vrect.width - dist) {
      sx = 1;
      dx = dist - (vrect.width - px);
    }

    sy = 0;
    if (py < dist && py >= 0) {
      sy = -1;
      dy = dist - py;
    }
    else if (py < vrect.height && py >= vrect.height - dist) {
      sy = 1;
      dy = dist - (vrect.height - py);
    }

    dx /= 2;
    dy /= 2;

    // start autoscrolling if we have a nonzero scroll vector
    if (sx != 0 || sy != 0) {
      if (!scroller.isRunning()) {
        scroller.setStartDelay((Integer)
          GameModule.getGameModule().getPrefs().getValue(PREFERRED_EDGE_DELAY));
        scroller.start();
      }
    }
    else {
      if (scroller.isRunning()) scroller.stop();
    }
  }

  /** The animator which controls autoscrolling. */
  protected Animator scroller = new Animator(Animator.INFINITE,
    new TimingTargetAdapter() {

      private long t0;

      /**
       * Continue to scroll the map as animator instructs us
       * @param fraction not used
       */
      @Override
      public void timingEvent(float fraction) {
        // Constant velocity along each axis, 0.5px/ms
        final long t1 = System.currentTimeMillis();
        final int dt = (int)((t1 - t0) / 2);
        t0 = t1;

        scroll(sx * dt, sy * dt);

        // Check whether we have hit an edge
        final Rectangle vrect = scroll.getViewport().getViewRect();

        if ((sx == -1 && vrect.x == 0) ||
            (sx ==  1 && vrect.x + vrect.width >= theMap.getWidth())) sx = 0;

        if ((sy == -1 && vrect.y == 0) ||
            (sy ==  1 && vrect.y + vrect.height >= theMap.getHeight())) sy = 0;

        // Stop if the scroll vector is zero
        if (sx == 0 && sy == 0) scroller.stop();
      }

      /**
       * Get ready to scroll
       */
      @Override
      public void begin() {
        t0 = System.currentTimeMillis();
      }
    }
  );

  /**
   * Repaints the map. Accepts parameter about whether to clear the display first.
   * @param cf true if display should be cleared before drawing the map
   */
  public void repaint(boolean cf) {
    clearFirst = cf;
    theMap.repaint();
  }

  /**
   * Repaints the map.
   */
  public void repaint() {
    if (!GameModule.getGameModule().isLoadOverSemaphore()) {
      theMap.repaint();
    }
  }

  /**
   * Paints a specific region of the map, denoted by a Rectangle.
   * @param g Graphics object where map should be painted
   * @param visibleRect region of map to repaint
   */
  public void paintRegion(Graphics g, Rectangle visibleRect) {
    paintRegion(g, visibleRect, theMap);
  }

  /**
   * Paints a specific region of the map, denoted by a Rectangle.
   * @param g Graphics object where map should be painted
   * @param visibleRect region of map to repaint
   * @param c observer component
   */
  public void paintRegion(Graphics g, Rectangle visibleRect, Component c) {
    clearMapBorder(g); // To avoid ghost pieces around the edge
    drawBoardsInRegion(g, visibleRect, c);
    drawDrawable(g, false);
    drawPiecesInRegion(g, visibleRect, c);
    drawDrawable(g, true);
  }

  /**
   * For each Board overlapping the given region, update the appropriate section of the board image.
   * @param g Graphics object where map should be painted
   * @param visibleRect region of map to repaint
   * @param c observer component
   */
  public void drawBoardsInRegion(Graphics g,
                                 Rectangle visibleRect,
                                 Component c) {
    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final double dzoom = getZoom() * os_scale;
    for (final Board b : boards) {
      b.drawRegion(g, getLocation(b, dzoom), visibleRect, dzoom, c);
    }
  }

  /**
   * For each Board overlapping the given region, update the appropriate section of the board image.
   * @param g Graphics object where map should be painted
   * @param visibleRect region of map to repaint
   */
  public void drawBoardsInRegion(Graphics g, Rectangle visibleRect) {
    drawBoardsInRegion(g, visibleRect, theMap);
  }

  /**
   * Draws all pieces visible in a rectangular area of the map
   * @param g Graphics object where map should be painted
   * @param visibleRect region of map to repaint
   * @param c observer component
   */
  public void drawPiecesInRegion(Graphics g,
                                 Rectangle visibleRect,
                                 Component c) {
    if (hideCounters) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final double dzoom = getZoom() * os_scale;

    final Composite oldComposite = g2d.getComposite();
    g2d.setComposite(
      AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));
    final GamePiece[] stack = pieces.getPieces(); // Gets map pieces, sorted by visual layer
    for (final GamePiece gamePiece : stack) {
      final Point pt = mapToDrawing(gamePiece.getPosition(), os_scale);
      if (gamePiece.getClass() == Stack.class) {
        getStackMetrics().draw(
          (Stack) gamePiece, pt, g, this, dzoom, visibleRect
        );
      }
      else {
        gamePiece.draw(g, pt.x, pt.y, c, dzoom);
        if (Boolean.TRUE.equals(gamePiece.getProperty(Properties.SELECTED))) {
          highlighter.draw(gamePiece, g, pt.x, pt.y, c, dzoom);
        }
      }
/*
        // draw bounding box for debugging
        final Rectangle bb = stack[i].boundingBox();
        g.drawRect(pt.x + bb.x, pt.y + bb.y, bb.width, bb.height);
*/
    }
    g2d.setComposite(oldComposite);
  }

  /**
   * Draws all pieces visible in a rectangular area of the map
   * @param g Graphics object where map should be painted
   * @param visibleRect region of map to repaint
   */
  public void drawPiecesInRegion(Graphics g, Rectangle visibleRect) {
    drawPiecesInRegion(g, visibleRect, theMap);
  }

  /**
   * Draws the map pieces at a given offset
   * @param g Target graphics object
   * @param xOffset x offset
   * @param yOffset y offset
   */
  public void drawPieces(Graphics g, int xOffset, int yOffset) {
    if (hideCounters) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final Composite oldComposite = g2d.getComposite();
    g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));
    final GamePiece[] stack = pieces.getPieces(); // Gets map pieces, sorted by visual layer
    for (final GamePiece gamePiece : stack) {
      final Point pt = mapToDrawing(gamePiece.getPosition(), os_scale);
      gamePiece.draw(g, pt.x + xOffset, pt.y + yOffset, theMap, getZoom());
      if (Boolean.TRUE.equals(gamePiece.getProperty(Properties.SELECTED))) {
        highlighter.draw(gamePiece, g, pt.x - xOffset, pt.y - yOffset, theMap, getZoom());
      }
    }
    g2d.setComposite(oldComposite);
  }

  /**
   * Draws all of our "Drawable" components. Standard examples include {@link CounterDetailViewer}s (aka Mouse-over Stack Viewers),
   * {@link GlobalMap}s (aka Overview Maps), {@link LOS_Thread}s, {@link MapShader}s, and the {@link KeyBufferer} (to show which
   * pieces are selected).
   * @param g target graphics object
   * @param aboveCounters true means we should draw only the drawables that go above the counters; false means we should draw only the ones that go below
   */
  public void drawDrawable(Graphics g, boolean aboveCounters) {
    for (final Drawable drawable : drawComponents) {
      if (aboveCounters == drawable.drawAboveCounters()) {
        drawable.draw(g, this);
      }
    }
  }

  /**
   * @return Current selection highlighter
   */
  public Highlighter getHighlighter() {
    return highlighter;
  }

  /**
   * @param h selection highlighter to set active
   */
  public void setHighlighter(Highlighter h) {
    highlighter = h;
  }

  /**
   * @param h selection highlighter to add to our list
   */
  public void addHighlighter(Highlighter h) {
    highlighters.add(h);
  }

  /**
   * @param h selection highlighter to remove from our list
   */
  public void removeHighlighter(Highlighter h) {
    highlighters.remove(h);
  }

  /**
   * @return an Iterator for all of our highlighters
   */
  public Iterator<Highlighter> getHighlighters() {
    return highlighters.iterator();
  }

  /**
   * @return a Collection of all {@link Board}s on the Map
   */
  public Collection<Board> getBoards() {
    return Collections.unmodifiableCollection(boards);
  }

  /**
   * @return an Enumeration of all {@link Board}s on the map
   * @deprecated Use {@link #getBoards()} instead.
   */
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public Enumeration<Board> getAllBoards() {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
    return Collections.enumeration(boards);
  }

  /**
   * @return number of Boards on this map
   */
  public int getBoardCount() {
    return boards.size();
  }

  /**
   * @return the boundingBox of a GamePiece accounting for the offset of a piece within its parent stack. Return null if
   * this piece is not on the map
   *
   * @see GamePiece#boundingBox
   */
  public Rectangle boundingBoxOf(GamePiece p) {
    Rectangle r = null;
    if (p.getMap() == this) {
      r = p.boundingBox();
      final Point pos = p.getPosition();
      r.translate(pos.x, pos.y);

      if (Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
        r.add(highlighter.boundingBox(p));
        for (final Iterator<Highlighter> i = getHighlighters(); i.hasNext();) {
          r.add(i.next().boundingBox(p));
        }
      }

      if (p.getParent() != null) {
        final Point pt = getStackMetrics().relativePosition(p.getParent(), p);
        r.translate(pt.x, pt.y);
      }
    }
    return r;
  }

  /**
   * @return the selection bounding box of a GamePiece accounting for the offset of a piece within a stack
   *
   * @see GamePiece#getShape
   */
  public Rectangle selectionBoundsOf(GamePiece p) {
    if (p.getMap() != this) {
      throw new IllegalArgumentException(
        Resources.getString("Map.piece_not_on_map")); //$NON-NLS-1$
    }

    final Rectangle r = p.getShape().getBounds();
    r.translate(p.getPosition().x, p.getPosition().y);
    if (p.getParent() != null) {
      final Point pt = getStackMetrics().relativePosition(p.getParent(), p);
      r.translate(pt.x, pt.y);
    }
    return r;
  }

  /**
   * @return the position of a GamePiece accounting for the offset within a parent stack, if any
   */
  public Point positionOf(GamePiece p) {
    if (p.getMap() != this) {
      throw new IllegalArgumentException(
        Resources.getString("Map.piece_not_on_map")); //$NON-NLS-1$
    }

    final Point point = p.getPosition();
    if (p.getParent() != null) {
      final Point pt = getStackMetrics().relativePosition(p.getParent(), p);
      point.translate(pt.x, pt.y);
    }
    return point;
  }

  /**
   * @return an array of all GamePieces on the map, subject to visibility, and sorted in order of
   * visual layers. This is a read-only copy. Altering the array does not alter the pieces on the map.
   */
  public GamePiece[] getPieces() {
    return pieces.getPieces();
  }

  /**
   * @return an array of all GamePieces on the map, regardless of visibility, and sorted
   * in order of visual layer. This is a read-only copy. Altering the array does not alter
   * the pieces on the map.
   */
  public GamePiece[] getAllPieces() {
    return pieces.getAllPieces();
  }

  /**
   * @param pieces Sets the PieceCollection for this map (usually a LayeredPieceCollection a/k/a "Game Piece Layer Control"), which keeps the pieces/stacks/decks sorted by visual layer, and within each layer by back-to-front draw order
   */
  public void setPieceCollection(PieceCollection pieces) {
    this.pieces = pieces;
  }

  /**
   * @return piece collection for this map (a/k/a its LayeredPieceCollection or "Game Piece Layer Control"), which maintains a list of all the pieces/stacks/decks on the map sorted by visual layer, and within each layer by back-to-front draw order
   */
  public PieceCollection getPieceCollection() {
    return pieces;
  }

  /**
   * Clears the map border region, if any. If the {@link #clearFirst} flag is set, wipe the map image too.
   * @param g target graphics object
   */
  protected void clearMapBorder(Graphics g) {
    final Graphics2D g2d = (Graphics2D) g.create();
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    if (clearFirst || boards.isEmpty()) {
      g.setColor(bgColor);
      g.fillRect(
        0,
        0,
        componentToDrawing(theMap.getWidth(), os_scale),
        componentToDrawing(theMap.getHeight(), os_scale)
      );
      clearFirst = false;
    }
    else {
      final int mw = componentToDrawing(theMap.getWidth(), os_scale);
      final int mh = componentToDrawing(theMap.getHeight(), os_scale);

      final int ew = mapToDrawing(edgeBuffer.width, os_scale);
      final int eh = mapToDrawing(edgeBuffer.height, os_scale);

      g.setColor(bgColor);
      g.fillRect(0, 0, ew, mh);
      g.fillRect(0, 0, mw, eh);
      g.fillRect(mw - ew, 0, ew, mh);
      g.fillRect(0, mh - eh, mw, eh);
    }
  }

  /**
   * Adjusts the bounds() rectangle to account for the Board's relative
   * position to other boards. In other words, if Board A is N pixels wide
   * and Board B is to the right of Board A, then the origin of Board B
   * will be adjusted N pixels to the right.
   */
  protected void setBoardBoundaries() {
    int maxX = 0;
    int maxY = 0;
    for (final Board b : boards) {
      final Point relPos = b.relativePosition();
      maxX = Math.max(maxX, relPos.x);
      maxY = Math.max(maxY, relPos.y);
    }
    boardWidths = new int[maxX + 1][maxY + 1];
    boardHeights = new int[maxX + 1][maxY + 1];
    for (final Board b : boards) {
      final Point relPos = b.relativePosition();
      boardWidths[relPos.x][relPos.y] = b.bounds().width;
      boardHeights[relPos.x][relPos.y] = b.bounds().height;
    }
    final Point offset = new Point(edgeBuffer.width, edgeBuffer.height);
    for (final Board b : boards) {
      final Point relPos = b.relativePosition();
      final Point location = getLocation(relPos.x, relPos.y, 1.0);
      b.setLocation(location.x, location.y);
      b.translate(offset.x, offset.y);
    }
    theMap.revalidate();
  }

  /**
   * Gets the location of a board in Map space, based on a passed zoom factor
   * @param b Board to find location
   * @param zoom zoom factor to use
   * @return Relative position of the board at given scale
   */
  protected Point getLocation(Board b, double zoom) {
    final Point p;
    if (zoom == 1.0) {
      p = b.bounds().getLocation();
    }
    else {
      final Point relPos = b.relativePosition();
      p = getLocation(relPos.x, relPos.y, zoom);
      p.translate((int) (zoom * edgeBuffer.width), (int) (zoom * edgeBuffer.height));
    }
    return p;
  }

  /**
   * Finds the location of a board (in Map space) at a particular
   * column and row, based on passed zoom factor
   * @param column number of board to find
   * @param row number of board to find
   * @param zoom zoom factor to use
   * @return location of the board in Map space
   */
  protected Point getLocation(int column, int row, double zoom) {
    final Point p = new Point();
    for (int x = 0; x < column; ++x) {
      p.translate((int) Math.floor(zoom * boardWidths[x][row]), 0);
    }
    for (int y = 0; y < row; ++y) {
      p.translate(0, (int) Math.floor(zoom * boardHeights[column][y]));
    }
    return p;
  }

  /**
   * Draw the boards of the map at the given point and zoom factor onto
   * the given Graphics object
   * @param g target Graphics object
   * @param xoffset x offset to draw at
   * @param yoffset y offset to draw at
   * @param zoom zoom factor for drawing the boards
   * @param obs observer Component
   */
  public void drawBoards(Graphics g, int xoffset, int yoffset, double zoom, Component obs) {
    for (final Board b : boards) {
      final Point p = getLocation(b, zoom);
      p.translate(xoffset, yoffset);
      b.draw(g, p.x, p.y, zoom, obs);
    }
  }

  /**
   * Repaint the given area, specified in map coordinates
   * @param r Rectangle specifying region to repaint in map coordinates
   */
  public void repaint(Rectangle r) {
    r.setLocation(mapToComponent(new Point(r.x, r.y)));
    r.setSize((int) (r.width * getZoom()), (int) (r.height * getZoom()));
    theMap.repaint(r.x, r.y, r.width, r.height);
  }

  /**
   * @param show if true, enable drawing of {@link GamePiece}s. If false, don't draw any {@link GamePiece}s when painting the map
   */
  public void setPiecesVisible(boolean show) {
    hideCounters = !show;
  }

  /**
   * @return true if {@link GamePiece}s should be drawn when painting the map
   */
  public boolean isPiecesVisible() {
    return !hideCounters && pieceOpacity != 0.0;
  }

  /**
   * @return current pieceOpacity for drawing
   */
  public float getPieceOpacity() {
    return pieceOpacity;
  }

  /**
   * @param pieceOpacity sets opacity for piece drawing, 0 to 1.0
   */
  public void setPieceOpacity(float pieceOpacity) {
    this.pieceOpacity = pieceOpacity;
  }

  /**
   * Gets the value of a map-level global property. If a "Global Property" entry is not found at the map
   * level, then module-level properties are checked, which includes identification information for the
   * local player.
   * @param key identifies the global property to be returned
   * @return value of designated global property
   */
  @Override
  public Object getProperty(Object key) {
    final Object value;

    final MutableProperty p = propsContainer.getMutableProperty(String.valueOf(key));
    if (p != null) {
      value = p.getPropertyValue();
    }
    else {
      value = GameModule.getGameModule().getProperty(key);
    }
    return value;
  }

  /**
   * Gets the value of map-level global property, or a module-level one if a map-level one is not found.
   * The localized aspect presently only applies to permanent module-level objects which can be localized (e.g. player sides)
   * @param key Name of the property to get the value of
   * @return Localized/translated name of the named property, if one is available, otherwise returns the non-localized name
   */
  @Override
  public Object getLocalizedProperty(Object key) {
    Object value = null;
    final MutableProperty p = propsContainer.getMutableProperty(String.valueOf(key));
    if (p != null) {
      value = p.getPropertyValue();
    }
    if (value == null) {
      value = GameModule.getGameModule().getLocalizedProperty(key);
    }
    return value;
  }

  /**
   * Return the apply-on-move key. It may be named, so just return
   * the allocated KeyStroke.
   * @return apply-on-move keystroke
   */
  public KeyStroke getMoveKey() {
    return moveKey == null ? null : moveKey.getKeyStroke();
  }

  /**
   * Creates the top-level window for this map. Could be a JDialog or a JFrame depending on whether we are set to
   * use a single window or have our own window.
   * @return the top-level window containing this map
   */
  protected Window createParentFrame() {
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      final JDialog d = new JDialog(GameModule.getGameModule().getPlayerWindow());
      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
      return d;
    }
    else {
      final JFrame d = new JFrame();
      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
      d.setJMenuBar(MenuManager.getInstance().getMenuBarFor(d));
      return d;
    }
  }

  /**
   * If this map shows with its own special button, it does not dock into the main window. Likewise if we have
   * no combined window setting. Otherwise the *first* non-button-launched map we find will be the one we dock.
   * @return whether this map should dock into the main window
   */
  public boolean shouldDockIntoMainWindow() {
    // set to show via a button, or no combined window at all, don't dock
    if (useLaunchButton || !GlobalOptions.getInstance().isUseSingleWindow()) {
      return false;
    }

    // otherwise dock if this map is the first not to show via a button
    for (final Map m : GameModule.getGameModule().getComponentsOf(Map.class)) {
      if (m == this) {
        return true;
      }
      else if (m.shouldDockIntoMainWindow()) {
        return false;
      }
    }
    // Module isn't fully built yet, and/or no maps at all in module yet (perhaps THIS map will soon be the first one)
    return true;
  }

  /**
   * When a game is started, create a top-level window, if none exists.
   * When a game is ended, remove all boards from the map.
   * @param show true if a game is starting, false if a game is ending
   *
   * @see GameComponent
   */
  @Override
  public void setup(boolean show) {
    final GameModule g = GameModule.getGameModule();

    if (show) {
      if (!g.isLoadOverSemaphore()) {
        if (shouldDockIntoMainWindow()) {
          // kludge for modules which still use mainWindowDock
          // remove this when mainWindowDock is removed
          if (mainWindowDock != null && splitPane == null) {
            splitPane = new SplitPane(
              SplitPane.VERTICAL_SPLIT,
              mainWindowDock.getTopComponent(),
              mainWindowDock.getBottomComponent()
            );
            splitPane.setResizeWeight(0.0);

            final Container mwdpar = mainWindowDock.getParent();
            mwdpar.remove(mainWindowDock);
            mwdpar.add(splitPane);
          }

          if (splitPane != null) {
            // If we're docked to the main window, check the various player preferences w/r/t remembering desired window height.
            // The window *width* has already been established, so we don't touch it here.
            final PlayerWindow window = g.getPlayerWindow();
            final Rectangle screen = SwingUtils.getScreenBounds(window);
            final Prefs p = Prefs.getGlobalPrefs();

            final boolean remember = Boolean.TRUE.equals(p.getOption(MAIN_WINDOW_REMEMBER).getValue());
            final int h = remember ? ((Integer) p.getOption(MAIN_WINDOW_HEIGHT).getValue()) : -1;

            window.setSize(window.getWidth(), (h > 0) ? h : screen.height);

            splitPane.showBottom();

            //BR// Force the divider to the Chatter's "preferred height"
            final int divider = g.getChatter().getPreferredSize().height;
            splitPane.setDividerLocation(divider);

            // kludge for modules which still use mainWindowDock
            // remove this when mainWindowDock is removed
            if (mainWindowDock != null) {
              splitPane.setDividerSize(5);
            }

            // ensure that the splitter has the full range of motion
            splitPane.getTopComponent().setMinimumSize(new Dimension(0, 0));
            splitPane.getBottomComponent().setMinimumSize(new Dimension(0, 0));
          }

          if (toolBar.getParent() == null) {
            g.getToolBar().addSeparator();
            g.getToolBar().add(toolBar);
          }
        }
        else {
          if (SwingUtilities.getWindowAncestor(theMap) == null) {
            final Window topWindow = createParentFrame();
            topWindow.addWindowListener(new WindowAdapter() {
              @Override
              public void windowClosing(WindowEvent e) {
                if (useLaunchButton) {
                  topWindow.setVisible(false);
                }
                else {
                  g.getGameState().setup(false);
                }
              }
            });
            ((RootPaneContainer) topWindow).getContentPane().add("North", getToolBar()); //$NON-NLS-1$
            ((RootPaneContainer) topWindow).getContentPane().add("Center", layeredPane); //$NON-NLS-1$
            topWindow.setSize(600, 400);
            final PositionOption option =
              new PositionOption(PositionOption.key + getIdentifier(), topWindow);
            g.getPrefs().addOption(option);
          }
          theMap.getTopLevelAncestor().setVisible(!useLaunchButton);
          theMap.revalidate();
        }

        for (final Board b : boards) {
          if (!b.getAllDescendantComponentsOf(ZoneHighlight.class).isEmpty()) {
            b.setCacheGrid(false);
          }
        }
      }
    }
    else {
      pieces.clear();
      boards.clear();

      if (!g.isLoadOverSemaphore()) {
        if (shouldDockIntoMainWindow()) {
          if (splitPane != null) {
            // If this is a docked-to-main-window map, AND it's presently visible, save our window size preferences
            if (splitPane.isBottomVisible()) {
              final Dimension d = g.getPlayerWindow().getSize();
              final GlobalPrefs p = (GlobalPrefs) Prefs.getGlobalPrefs();
              final PlayerWindow window = g.getPlayerWindow();
              final Rectangle screen = SwingUtils.getScreenBounds(window);
              final int w = window.getWidth();
              final int h;
              if (Boolean.TRUE.equals(p.getOption(MAIN_WINDOW_REMEMBER).getValue())) {
                p.setDisableAutoWrite(true);
                p.getOption(MAIN_WINDOW_HEIGHT).setValue(d.height);
                p.getOption(MAIN_WINDOW_WIDTH).setValue(d.width);
                p.saveGlobal();
                p.setDisableAutoWrite(false);

                h = d.height;
              }
              else {
                h = screen.height;
              }

              // Now we hide the bottom part of the pane (the part where the map normally is)
              splitPane.hideBottom();

              // Now we restore the main window to its "map is offline" state.
              window.setSize(w, h / 3);
            }
          }
        }
        else if (theMap.getTopLevelAncestor() != null) {
          theMap.getTopLevelAncestor().setVisible(false);
        }
      }
    }

    if (show || !g.isLoadOverSemaphore()) {
      toolBar.setVisible(show);
      getLaunchButton().setEnabled(show);
      getLaunchButton().setVisible(useLaunchButton);
      if (g.isLoadOverSemaphore()) {
        theMap.repaint();
      }
    }
  }

  /**
   * As a {@link GameComponent}, Map does not have any action inherently needing to be taken for "restoring" itself for load/save and
   * network play purposes (the locations of pieces, etc, are stored in the pieces, and are restored from {@link GameState} in its
   * {@link GameState#getRestorePiecesCommand} method, which creates an AddPiece command for each piece). Map's interest in GameComponent
   * is entirely for game start/stop purposes (see {@link #setup}, above).
   * @return null since no restore command needed.
   */
  @Override
  public Command getRestoreCommand() {
    return null;
  }


  /**
   * @deprecated use {@link #updateTitleBar()}
   * @param s String to append to title
   */
  @Deprecated(since = "2020-09-16", forRemoval = true)
  @SuppressWarnings("unused")
  public void appendToTitle(String s) {
    // replaced by updateTitleBar()
  }

  /**
   * Updates the title bar of the current window
   */
  public void updateTitleBar() {
    if (splitPane != null) {
      return;
    }

    final Component c = theMap.getTopLevelAncestor();
    if (c instanceof JFrame) {
      ((JFrame) c).setTitle(getDefaultWindowTitle());
    }
    if (c instanceof JDialog) {
      ((JDialog) c).setTitle(getDefaultWindowTitle());
    }
  }

  /**
   * @return The correct current default window title
   */
  protected String getDefaultWindowTitle() {
    if (getLocalizedMapName().length() > 0) {
      return GameModule.getGameModule().getWindowTitleString("Map.window_named", getLocalizedMapName(), false); //$NON-NLS-1$
    }
    else {
      return GameModule.getGameModule().getWindowTitleString("Map.window", GameModule.getGameModule().getLocalizedGameName(), false); //$NON-NLS-1$
    }
  }

  /**
   * Use the provided {@link PieceFinder} instance to locate a visible piece at the given location
   * @param pt Point at which to find visible pieces
   * @param finder PieceFinder to use
   * @return a visible piece at the given location, or null if none.
   */
  public GamePiece findPiece(Point pt, PieceFinder finder) {
    final GamePiece[] stack = pieces.getPieces();
    for (int i = stack.length - 1; i >= 0; --i) {
      final GamePiece p = finder.select(this, stack[i], pt);
      if (p != null) {
        return p;
      }
    }
    return null;
  }

  /**
   * Use the provided {@link PieceFinder} instance to locate any piece at the given location, regardless of whether it
   * is visible or not.
   * @param pt Point at which to find pieces
   * @param finder PieceFinder to use
   * @return a piece at the given location, regardless of visibility, or null if none.
   */
  public GamePiece findAnyPiece(Point pt, PieceFinder finder) {
    final GamePiece[] stack = pieces.getAllPieces();
    // Our piece collection is provided to us in "draw order", in other words "back-to-front", which means
    // that we need to iterate backwards to prioritize checking pieces that are visually "in front of" others.
    for (int i = stack.length - 1; i >= 0; --i) {
      final GamePiece p = finder.select(this, stack[i], pt);
      if (p != null) {
        return p;
      }
    }
    return null;
  }

  /**
   * Place a piece at the destination point. If necessary, remove the piece from its parent Stack or Map
   * @param piece GamePiece to place
   * @param pt location to place the piece
   *
   * @return a {@link Command} that reproduces this action
   */
  public Command placeAt(GamePiece piece, Point pt) {
    final Command c;
    if (GameModule.getGameModule().getGameState().getPieceForId(piece.getId()) == null) {
      piece.setPosition(pt);
      addPiece(piece);
      GameModule.getGameModule().getGameState().addPiece(piece);
      c = new AddPiece(piece);
    }
    else {
      final MoveTracker tracker = new MoveTracker(piece);
      piece.setPosition(pt);
      addPiece(piece);
      c = tracker.getMoveCommand();
    }
    return c;
  }

  /**
   * Attempts to apply the provided {@link PieceVisitorDispatcher} to all pieces on this map, until it finds one
   * that returns a non-null Command.
   * @return the first non-null {@link Command} returned by <code>commandFactory</code>
   *
   * @param commandFactory The PieceVisitorDispatcher to apply
   */
  public Command apply(PieceVisitorDispatcher commandFactory) {
    final GamePiece[] stack = pieces.getPieces();
    Command c = null;
    for (int i = 0; i < stack.length && c == null; ++i) {
      c = (Command) commandFactory.accept(stack[i]);
    }
    return c;
  }

  /**
   * Move a piece to the destination point. If a piece is at the point (i.e. has a location exactly equal to it), merge
   * into a {@link Stack} with the piece by forwarding to {@link StackMetrics#merge}. Otherwise, place by forwarding to placeAt()
   * @param p GamePiece to place/merge
   * @param pt Point location on the map to place/merge the piece.
   * @return a {@link Command} that will duplicate this action on other clients
   *
   * @see StackMetrics#merge
   */
  public Command placeOrMerge(final GamePiece p, final Point pt) {
    Command c = apply(new DeckVisitorDispatcher(new Merger(this, pt, p)));
    if (c == null || c.isNull()) {
      c = placeAt(p, pt);
      // If no piece at destination and this is a stacking piece, create
      // a new Stack containing the piece
      if (!(p instanceof Stack) &&
          !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))) {
        final Stack parent = getStackMetrics().createStack(p);
        if (parent != null) {
          c = c.append(placeAt(parent, pt));
        }
      }
    }
    return c;
  }

  /**
   * Adds a GamePiece to this map's list of pieces. Removes the piece from its parent Stack and from its current map, if different from
   * this map.
   * @param p Game Piece to add
   */
  public void addPiece(GamePiece p) {
    if (indexOf(p) < 0) {
      if (p.getParent() != null) {
        p.getParent().remove(p);
        p.setParent(null);
      }
      if (p.getMap() != null && p.getMap() != this) {
        p.getMap().removePiece(p);
      }
      pieces.add(p);
      p.setMap(this);
      theMap.repaint();
    }
  }

  /**
   * Reorder the argument GamePiece to the new index. When painting the map, pieces are drawn in order of index
   *
   * @deprecated use {@link PieceCollection#moveToFront}
   */
  @SuppressWarnings("unused")
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public void reposition(GamePiece s, int pos) {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
  }

  /**
   * Returns the index of a piece. When painting the map, pieces are drawn in order of index Return -1 if the piece is
   * not on this map
   * @param s GamePiece to find the index of
   * @return index of the piece on the map, or -1 if the piece is not on this map
   */
  public int indexOf(GamePiece s) {
    return pieces.indexOf(s);
  }

  /**
   * Removes a piece from the map
   * @param p GamePiece to remove from map
   */
  public void removePiece(GamePiece p) {
    pieces.remove(p);
    theMap.repaint();
  }

  /**
   * Center the map at given map coordinates within its JScrollPane container
   * @param p Point to center
   */
  public void centerAt(Point p) {
    centerAt(p, 0, 0);
  }

  /**
   * Center the map at the given map coordinates, if the point is not
   * already within (dx,dy) of the center.
   * @param p point to center
   * @param dx x tolerance for nearness to center
   * @param dy y tolerance for nearness to center
   */
  public void centerAt(Point p, int dx, int dy) {
    if (scroll != null) {
      p = mapToComponent(p);

      final Rectangle r = theMap.getVisibleRect();
      r.x = p.x - r.width / 2;
      r.y = p.y - r.height / 2;

      final Dimension d = getPreferredSize();
      if (r.x + r.width > d.width) r.x = d.width - r.width;
      if (r.y + r.height > d.height) r.y = d.height - r.height;

      r.width = dx > r.width ? 0 : r.width - dx;
      r.height = dy > r.height ? 0 : r.height - dy;

      theMap.scrollRectToVisible(r);
    }
  }

  /**
   * Ensure that the given region (in map coordinates) is visible. Uses player preference
   * to determine how sensitive to be about when to re-center.
   * @param r Rectangle demarking region to ensure is visible
   */
  public void ensureVisible(Rectangle r) {
    if (scroll != null) {
      final boolean bTriggerRecenter;
      final Point p = mapToComponent(r.getLocation());
      final Rectangle rCurrent = theMap.getVisibleRect();
      final Rectangle rNorecenter = new Rectangle(0, 0);

      // If r is already visible decide if unit is close enough to
      // border to justify a recenter

      // Close enough means a strip of the window along the edges whose
      // width is a % of the edge to center of the window

      // The % is defined in GlobalOptions.CENTER_ON_MOVE_SENSITIVITY
      final double noRecenterPct = (100.0 - GlobalOptions.getInstance().centerOnOpponentsMoveSensitivity()) / 100.0;

      // if r is within a band of  n%width/height of border, trigger recenter
      rNorecenter.width = (int) round(rCurrent.width * noRecenterPct);
      rNorecenter.height = (int) round(rCurrent.height * noRecenterPct);
      rNorecenter.x = rCurrent.x + round(rCurrent.width - rNorecenter.width) / 2;
      rNorecenter.y = rCurrent.y + round(rCurrent.height - rNorecenter.height) / 2;

      bTriggerRecenter = p.x < rNorecenter.x || p.x > (rNorecenter.x + rNorecenter.width) ||
        p.y < rNorecenter.y || p.y > (rNorecenter.y + rNorecenter.height);

      if (bTriggerRecenter) {
        r.x = p.x - rCurrent.width / 2;
        r.y = p.y - rCurrent.height / 2;
        r.width = rCurrent.width;
        r.height = rCurrent.height;

        final Dimension d = getPreferredSize();
        if (r.x + r.width > d.width) r.x = d.width - r.width;
        if (r.y + r.height > d.height) r.y = d.height - r.height;

        theMap.scrollRectToVisible(r);
      }
    }
  }

  /**
   * Scrolls the map in the containing JScrollPane.
   *
   * @param dx number of pixels to scroll horizontally
   * @param dy number of pixels to scroll vertically
   */
  public void scroll(int dx, int dy) {
    Rectangle r = scroll.getViewport().getViewRect();
    r.translate(dx, dy);
    r = r.intersection(new Rectangle(getPreferredSize()));
    theMap.scrollRectToVisible(r);
  }

  /**
   * Gets the generic name for this type of class across all instances of it. Appears
   * in the Editor window in [..] as e.g. [Map], [Prototype], etc.
   * @return The generic name for this kind of component, i.e. the part appearing [In Brackets] in the Editor's {@link ConfigureTree}.
   */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Map.component_type"); //$NON-NLS-1$
  }

  /**
   * @return the name of this map, for internal purposes
   */
  public String getMapName() {
    return getConfigureName();
  }

  /**
   * @return the localized name of this map, for display purposes
   */
  public String getLocalizedMapName() {
    return getLocalizedConfigureName();
  }

  /**
   * @param s Sets the name of the map.
   */
  public void setMapName(String s) {
    mapName = s;
    setConfigureName(mapName);
    if (tooltip == null || tooltip.length() == 0) {
      getLaunchButton().setToolTipText(s != null ? Resources.getString("Map.show_hide", s) : Resources.getString("Map.show_hide", Resources.getString("Map.map"))); //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$
    }
  }

  /**
   * @return a HelpFile describing how to use and configure this component
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html"); //$NON-NLS-1$
  }

  /**
   * @return an array of Strings describing the buildFile (XML) attributes of this component. These strings are used as prompts in the
   * Properties window for this object, when the component is configured in the Editor. The order of descriptions should
   * be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   * @see AbstractConfigurable
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.Map.map_name"), //$NON-NLS-1$
      Resources.getString("Editor.Map.mark_pieces_moved"), //$NON-NLS-1$
      Resources.getString("Editor.Map.mark_unmoved_button_text"), //$NON-NLS-1$
      Resources.getString("Editor.Map.mark_unmoved_tooltip_text"), //$NON-NLS-1$
      Resources.getString("Editor.Map.mark_unmoved_button_icon"), //$NON-NLS-1$
      Resources.getString("Editor.Map.horizontal"), //$NON-NLS-1$
      Resources.getString("Editor.Map.vertical"), //$NON-NLS-1$
      Resources.getString("Editor.Map.bkgdcolor"), //$NON-NLS-1$
      Resources.getString("Editor.Map.multiboard"), //$NON-NLS-1$
      Resources.getString("Editor.Map.bc_selected_counter"), //$NON-NLS-1$
      Resources.getString("Editor.Map.bt_selected_counter"), //$NON-NLS-1$
      Resources.getString("Editor.Map.show_hide"), //$NON-NLS-1$
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.TOOLTIP_TEXT),
      Resources.getString(Resources.BUTTON_ICON),
      Resources.getString(Resources.HOTKEY_LABEL),
      Resources.getString("Editor.Map.report_move_within"), //$NON-NLS-1$
      Resources.getString("Editor.Map.report_move_to"), //$NON-NLS-1$
      Resources.getString("Editor.Map.report_created"), //$NON-NLS-1$
      Resources.getString("Editor.Map.report_modified"), //$NON-NLS-1$
      Resources.getString("Editor.Map.key_applied_all") //$NON-NLS-1$
    };
  }

  /**
   * Lists all the buildFile (XML) attribute names for this component.
   * If this component is ALSO an {@link AbstractConfigurable}, then this list of attributes determines the appropriate
   * attribute order for {@link AbstractConfigurable#getAttributeDescriptions()} and {@link AbstractConfigurable#getAttributeTypes()}.
   * @return a list of all buildFile (XML) attribute names for this component
   * @see AbstractBuildable
   */
  @Override
  public String[] getAttributeNames() {
    return new String[] {
      NAME,
      MARK_MOVED,
      MARK_UNMOVED_TEXT,
      MARK_UNMOVED_TOOLTIP,
      MARK_UNMOVED_ICON,
      EDGE_WIDTH,
      EDGE_HEIGHT,
      BACKGROUND_COLOR,
      ALLOW_MULTIPLE,
      HIGHLIGHT_COLOR,
      HIGHLIGHT_THICKNESS,
      USE_LAUNCH_BUTTON,
      BUTTON_NAME,
      TOOLTIP,
      ICON,
      HOTKEY,
      MOVE_WITHIN_FORMAT,
      MOVE_TO_FORMAT,
      CREATE_FORMAT,
      CHANGE_FORMAT,
      MOVE_KEY
    };
  }

  /**
   * @return the Class for the buildFile (XML) attributes of this component. Valid classes include: String, Integer, Double, Boolean, Image,
   * Color, and KeyStroke, along with any class for which a Configurer exists in VASSAL.configure. The class determines, among other things,
   * which type of {@link AutoConfigurer} will be used to configure the attribute when the object is configured in the Editor.
   *
   * The order of classes should be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   * @see AbstractConfigurable
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      GlobalOptions.Prompt.class,
      String.class,
      String.class,
      UnmovedIconConfig.class,
      Integer.class,
      Integer.class,
      Color.class,
      Boolean.class,
      Color.class,
      Integer.class,
      Boolean.class,
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class,
      MoveWithinFormatConfig.class,
      MoveToFormatConfig.class,
      CreateFormatConfig.class,
      ChangeFormatConfig.class,
      NamedKeyStroke.class
    };
  }

  public static final String LOCATION = "location"; //$NON-NLS-1$
  public static final String OLD_LOCATION = "previousLocation"; //$NON-NLS-1$
  public static final String OLD_MAP = "previousMap"; //$NON-NLS-1$
  public static final String MAP_NAME = "mapName"; //$NON-NLS-1$
  public static final String PIECE_NAME = "pieceName"; //$NON-NLS-1$
  public static final String MESSAGE = "message"; //$NON-NLS-1$

  /**
   * Autoconfigurer for map's icon used on its launch button
   */
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/map.gif"); //$NON-NLS-1$
    }
  }

  /**
   * Autoconfigurer for mark-unmoved icon
   */
  public static class UnmovedIconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/unmoved.gif"); //$NON-NLS-1$
    }
  }

  /**
   * Report format configurer for "moved within map"
   */
  public static class MoveWithinFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, LOCATION, MAP_NAME, OLD_LOCATION });
    }
  }


  /**
   * Report format configurer for "moved to map"
   */
  public static class MoveToFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, LOCATION, OLD_MAP, MAP_NAME, OLD_LOCATION });
    }
  }

  /**
   * Report format configurer for "created on map"
   */
  public static class CreateFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, MAP_NAME, LOCATION });
    }
  }

  /**
   * Report format configurer for "modified on map"
   */
  public static class ChangeFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] {
        MESSAGE,
        ReportState.COMMAND_NAME,
        ReportState.OLD_UNIT_NAME,
        ReportState.NEW_UNIT_NAME,
        ReportState.MAP_NAME,
        ReportState.LOCATION_NAME });
    }
  }

  /**
   * @return "created on map" format string
   */
  public String getCreateFormat() {
    if (createFormat != null) {
      return createFormat;
    }
    else {
      String val = Resources.getString("Editor.Map.report_created_in_default");
      if (!boards.isEmpty()) {
        final Board b = boards.get(0);
        if (b.getGrid() == null || b.getGrid().getGridNumbering() == null) {
          val = ""; //$NON-NLS-1$
        }
      }
      return val;
    }
  }

  /**
   * @return "changed on map" format string
   */
  public String getChangeFormat() {
    return isChangeReportingEnabled() ? changeFormat : "";
  }

  /**
   * @return "changed on map" format string
   */
  public String getChangeFormat(boolean noSuppress) {
    return (isChangeReportingEnabled() || noSuppress) ? changeFormat : "";
  }


  /**
   * @return "moved to map" format string
   */
  public String getMoveToFormat() {
    if (moveToFormat != null) {
      return moveToFormat;
    }
    else {
      String val = Resources.getString("Editor.Map.report_move_to_default");
      if (!boards.isEmpty()) {
        final Board b = boards.get(0);
        if (b.getGrid() == null || b.getGrid().getGridNumbering() != null) {
          val = ""; //$NON-NLS-1$
        }
      }
      return val;
    }
  }

  /**
   * @return "moved within map" format string
   */
  public String getMoveWithinFormat() {
    if (moveWithinFormat != null) {
      return moveWithinFormat;
    }
    else {
      String val = Resources.getString("Editor.Map.report_move_within_default");
      if (!boards.isEmpty()) {
        final Board b = boards.get(0);
        if (b.getGrid() == null) {
          val = ""; //$NON-NLS-1$
        }
      }
      return val;
    }
  }

  /**
   * List of subcomponents which can be added to a Map.
   *
   * @return a list of valid sub-component Classes.  If a Class
   * appears in this list, then instances of that class may be added
   * to this component from the Editor's {@link ConfigureTree} window by
   * right-clicking on the component and selecting the appropriate "Add"
   * option.
   * @see Configurable
   */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{ GlobalMap.class, LOS_Thread.class, ToolbarMenu.class, MultiActionButton.class, HidePiecesButton.class, Zoomer.class,
      CounterDetailViewer.class, HighlightLastMoved.class, LayeredPieceCollection.class, ImageSaver.class, TextSaver.class, DrawPile.class, SetupStack.class,
      MassKeyCommand.class, MapShader.class, PieceRecenterer.class, Flare.class };
  }

  /**
   * @param name Name (key) of one of this component's attributes
   * @return Visibility condition for the corresponding component
   */
  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (visibilityCondition == null) {
      visibilityCondition = () -> useLaunchButton;
    }
    if (List.of(HOTKEY, BUTTON_NAME, TOOLTIP, ICON).contains(name)) {
      return visibilityCondition;
    }
    else if (List.of(MARK_UNMOVED_TEXT, MARK_UNMOVED_ICON, MARK_UNMOVED_TOOLTIP).contains(name)) {
      return () -> !GlobalOptions.NEVER.equals(markMovedOption);
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  /**
   * Each Map must have a unique String id
   *
   * Sets our unique ID (among Maps), so that e.g. commands sent to different maps don't inadvertently get confused when
   * we send commands to other clients.
   * @param id Sets our unique ID
   */
  @Override
  public void setId(String id) {
    mapID = id;
  }

  /**
   * Each Map must have a unique String id
   *
   * @return the id for this map
   */
  @Override
  public String getId() {
    return mapID;
  }

  /**
   * Find the map that corresponds to a known unique id
   * @param id unique id of the map to find
   * @return Map object corresponding to that unique id
   */
  public static Map getMapById(String id) {
    return (Map) idMgr.findInstance(id);
  }

  /**
   * Utility method to return a {@link List} of all map components (on all maps!) in the
   * module.
   *
   * @return the list of <code>Map</code>s components
   */
  public static List<Map> getMapList() {
    final GameModule g = GameModule.getGameModule();

    final List<Map> l = g.getComponentsOf(Map.class);
    for (final ChartWindow cw : g.getComponentsOf(ChartWindow.class)) {
      for (final MapWidget mw : cw.getAllDescendantComponentsOf(MapWidget.class)) {
        l.add(mw.getMap());
      }
    }
    return l;
  }

  /**
   * Utility method to return a list of all map components in the module
   *
   * @return Iterator over all maps
   * @deprecated Use {@link #getMapList()} instead.
   */
  @Deprecated(since = "2020-08-05", forRemoval = true)
  public static Iterator<Map> getAllMaps() {
    ProblemDialog.showDeprecated("2020-08-05"); //NON-NLS
    return getMapList().iterator();
  }

  /**
   * Find a contained Global Property (variable) by name. Does NOT search at the Module level.
   * @param name Name of Global Property to find
   * @return Mutable property corresponding to the name given
   */
  @Override
  public MutableProperty getMutableProperty(String name) {
    return propsContainer.getMutableProperty(name);
  }

  /**
   * Adds a new Global Property to this map.
   * @param key Name of the new property
   * @param p The property object to add
   */
  @Override
  public void addMutableProperty(String key, MutableProperty p) {
    propsContainer.addMutableProperty(key, p);
    p.addMutablePropertyChangeListener(repaintOnPropertyChange);
  }

  /**
   * Removes a new Global Property from this map.
   * @param key Name of the property to be removed
   * @return the object just removed
   */
  @Override
  public MutableProperty removeMutableProperty(String key) {
    final MutableProperty p = propsContainer.removeMutableProperty(key);
    if (p != null) {
      p.removeMutablePropertyChangeListener(repaintOnPropertyChange);
    }
    return p;
  }

  /**
   * @return The container ID for map-level Global Properties on this object (just uses the map name)
   */
  @Override
  public String getMutablePropertiesContainerId() {
    return getMapName();
  }

  /**
   * Make a best guess for a unique identifier for the target. Use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getConfigureName} if non-null, otherwise use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getId}
   *
   * @return Unique Identifier
   */
  public String getIdentifier() {
    return UniqueIdManager.getIdentifier(this);
  }

  /** @return the Swing component representing the map */
  public JComponent getView() {
    if (theMap == null) {
      theMap = new View(this);

      scroll = new AdjustableSpeedScrollPane(
        theMap,
        JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      scroll.unregisterKeyboardAction(
        KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, 0));
      scroll.unregisterKeyboardAction(
        KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, 0));
      scroll.setAlignmentX(0.0f);
      scroll.setAlignmentY(0.0f);

      layeredPane.setLayout(new InsetLayout(layeredPane, scroll));
      layeredPane.add(scroll, JLayeredPane.DEFAULT_LAYER);
    }
    return theMap;
  }

  /** @return the JLayeredPane holding map insets */
  public JLayeredPane getLayeredPane() {
    return layeredPane;
  }

  /**
   * The Layout responsible for arranging insets which overlay the Map
   * InsetLayout currently is responsible for keeping the {@link GlobalMap}
   * in the upper-left corner of the {@link Map.View}.
   */
  public static class InsetLayout extends OverlayLayout {
    private static final long serialVersionUID = 1L;

    private final JScrollPane base;

    /**
     * @param target Component we are to lay out
     * @param base JScrollPane for it
     */
    public InsetLayout(Container target, JScrollPane base) {
      super(target);
      this.base = base;
    }

    /**
     * @param target Component to lay out
     */
    @Override
    public void layoutContainer(Container target) {
      super.layoutContainer(target);
      base.getLayout().layoutContainer(base);

      final Dimension viewSize = base.getViewport().getSize();
      final Insets insets = base.getInsets();
      viewSize.width += insets.left;
      viewSize.height += insets.top;

      // prevent non-base components from overlapping the base's scrollbars
      final int n = target.getComponentCount();
      for (int i = 0; i < n; ++i) {
        final Component c = target.getComponent(i);
        if (c != base && c.isVisible()) {
          final Rectangle b = c.getBounds();
          b.width = Math.min(b.width, viewSize.width);
          b.height = Math.min(b.height, viewSize.height);
          c.setBounds(b);
        }
      }
    }
  }

  /**
   * Implements default logic for merging pieces (into a {@link Stack} or {@link Deck}}
   * at a given location within a map Returns a {@link Command} that merges the input
   * {@link GamePiece} with an existing piece at the input position, provided the pieces
   * are stackable, visible, in the same layer, etc.
   */
  public static class Merger implements DeckVisitor {
    private final Point pt;
    private final Map map;
    private final GamePiece p;

    /**
     * Constructor for a Merger. This is passed the map, location, and piece we are going to be merging into something.
     * @param map map
     * @param pt point
     * @param p piece
     */
    public Merger(Map map, Point pt, GamePiece p) {
      this.map = map;
      this.pt = pt;
      this.p = p;
    }

    /**
     * Returns a command that merges our piece into the specified deck, provided that
     * the Deck shares the location of our merger point provided in the constructor.
     * @param d Deck to consider merging into
     * @return A command to merge our piece into the specified deck, or null if deck isn't in correct position
     */
    @Override
    public Object visitDeck(Deck d) {
      if (d.getPosition().equals(pt)) {
        return map.getStackMetrics().merge(d, p);
      }
      else {
        return null;
      }
    }

    /**
     * Returns a command to merge our piece into the specified stack, provided that the stack is in the precise
     * map location specified, the map allows stacking, our piece allows stacking, and our stack & piece are in the
     * same layer.
     * @param s Stack to consider merging with
     * @return Command to merge into the stack, or null if any of the necessary conditions weren't met
     */
    @Override
    public Object visitStack(Stack s) {
      if (s.getPosition().equals(pt) && map.getStackMetrics().isStackingEnabled() && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
          && s.topPiece() != null && map.getPieceCollection().canMerge(s, p)) {  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
        return map.getStackMetrics().merge(s, p);
      }
      else {
        return null;
      }
    }

    /**
     * @return a command to form a new stack with a piece found at the our location, provided all of the conditions to form a
     * stack are met. Returns null if the necessary conditions aren't met.
     * @param piece piece to consider forming a new stack with.
     */
    @Override
    public Object visitDefault(GamePiece piece) {
      if (piece.getPosition().equals(pt) && map.getStackMetrics().isStackingEnabled() && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
          && !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME)) && !Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK))
          && map.getPieceCollection().canMerge(piece, p)) {
        return map.getStackMetrics().merge(piece, p);
      }
      else {
        return null;
      }
    }
  }

  /**
   * The (JPanel-extending) component that represents the map itself
   */
  public static class View extends JPanel {
    private static final long serialVersionUID = 1L;

    protected Map map;

    /**
     * Create our view
     * @param m lets us know what Map we represent
     */
    public View(Map m) {
      setFocusTraversalKeysEnabled(false);
      map = m;
    }

    /**
     * Draw our graphics to the graphics object
     * @param g target graphics object
     */
    @Override
    public void paint(Graphics g) {
      // Don't draw the map until the game is updated.
      if (GameModule.getGameModule().getGameState().isUpdating()) {
        return;
      }

      final Graphics2D g2d = (Graphics2D) g;

      g2d.addRenderingHints(SwingUtils.FONT_HINTS);
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON);

      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

      // HDPI: We may get a transform where scale != 1. This means we
      // are running on an HDPI system. We want to draw at the effective
      // scale factor to prevent poor quality upscaling, so reset the
      // transform to scale of 1 and multiply the map zoom by the OS scaling.
      final AffineTransform orig_t = g2d.getTransform();
      g2d.setTransform(SwingUtils.descaleTransform(orig_t));

      final Rectangle r = map.componentToDrawing(getVisibleRect(), os_scale);

      g2d.setColor(map.bgColor);
      g2d.fillRect(r.x, r.y, r.width, r.height);
      map.paintRegion(g2d, r);

      g2d.setTransform(orig_t);
    }

    /**
     * Update our panel (by painting it)
     * @param g target graphics object
     */
    @Override
    public void update(Graphics g) {
      // To avoid flicker, don't clear the display first
      paint(g);
    }

    /**
     * @return our preferred size will be that of the map.
     */
    @Override
    public Dimension getPreferredSize() {
      return map.getPreferredSize();
    }

    /** returns the map we're assigned to */
    public Map getMap() {
      return map;
    }
  }


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(moveWithinFormat, moveToFormat, createFormat, changeFormat);
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    final List<String> l = new ArrayList<>();
    if (!GlobalOptions.NEVER.equals(markMovedOption)) {
      l.add(markUnmovedText);
      l.add(markUnmovedTooltip);
    }
    if (useLaunchButtonEdit) {
      l.add(getAttributeValueString(BUTTON_NAME));
      l.add(getAttributeValueString(TOOLTIP));
    }
    return l;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(HOTKEY)), moveKey);
  }

  /**
   * In case reports use HTML and  refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    HTMLImageFinder h;
    h = new HTMLImageFinder(moveWithinFormat);
    h.addImageNames(s);
    h = new HTMLImageFinder(moveToFormat);
    h.addImageNames(s);
    h = new HTMLImageFinder(createFormat);
    h.addImageNames(s);
    h = new HTMLImageFinder(changeFormat);
    h.addImageNames(s);

    String string;
    if (!GlobalOptions.NEVER.equals(markMovedOption)) {
      string = getAttributeValueString(MARK_UNMOVED_ICON);
      if (string != null) { // Launch buttons sometimes have null icon attributes - yay
        s.add(string);
      }
    }

    if (useLaunchButtonEdit) {
      string = getLaunchButton().getAttributeValueString(getLaunchButton().getIconAttribute());
      if (string != null) { // Launch buttons sometimes have null icon attributes - yay
        s.add(string);
      }
    }
  }
}

