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
package VASSAL.build.module;

import java.awt.AWTEventMulticaster;
import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.RootPaneContainer;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import org.w3c.dom.Element;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.DefaultPieceCollection;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.Drawable;
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
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.GlobalPropertiesContainer;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.widget.MapWidget;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.CompoundValidityChecker;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.MandatoryComponent;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Highlighter;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.ReportState;
import VASSAL.counters.Stack;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.ToolBarComponent;
import VASSAL.tools.UniqueIdManager;

/**
 * The Map is the main component for displaying and containing {@link GamePiece}s during play. Pieces are displayed on
 * a Map and moved by clicking and dragging. Keyboard events are forwarded to selected pieces. Multiple map windows are
 * supported in a single game, with dragging between windows allowed.
 * 
 * A Map may contain many different {@link Buildable} subcomponents. Components which are added directly to a Map are
 * contained in the <code>VASSAL.build.module.map</code> package
 */
public class Map extends AbstractConfigurable implements GameComponent, FocusListener, MouseListener, MouseMotionListener, DropTargetListener, Configurable,
		UniqueIdManager.Identifyable, ToolBarComponent, GlobalPropertiesContainer, PropertySource, PlayerRoster.SideChangeListener, Runnable {
	private String mapID = "";
	private String mapName = "";
	private static final String MAIN_WINDOW_HEIGHT = "mainWindowHeight";
	private static UniqueIdManager idMgr = new UniqueIdManager("Map");
	protected JPanel theMap;
	private Vector drawComponents = new Vector();
	protected JScrollPane scroll;
	protected ComponentSplitter.SplitPane mainWindowDock;
	protected BoardPicker picker;
	protected JToolBar toolBar = new JToolBar();
	protected Zoomer zoom;
	protected StackMetrics metrics;
	protected Dimension edgeBuffer = new Dimension(0, 0);
	protected LaunchButton launchButton;
	protected boolean useLaunchButton = false;
	protected String markMovedOption = GlobalOptions.ALWAYS;
	protected String markUnmovedIcon = "/images/unmoved.gif";
	protected MouseListener multicaster = null;
	protected Vector mouseListenerStack = new Vector();
	protected Vector boards = new Vector();
	private int[][] boardWidths; // Cache of board widths by row/column
	private int[][] boardHeights; // Cache of board heights by row/column
	protected PieceCollection pieces = new DefaultPieceCollection();
	protected java.util.Map globalProperties = new HashMap();
	protected Highlighter highlighter = new ColoredBorder();
  protected ArrayList highlighters = new ArrayList();
	private boolean clearFirst = false; // Whether to clear the display before
	// drawing the map
	private boolean hideCounters = false;// Option to hide counters to see
	// map
	protected float pieceOpacity = 1.0f;
	private boolean allowMultiple = false;
	private VisibilityCondition visibilityCondition;
	private DragGestureListener dragGestureListener;
	private String moveWithinFormat;
	private String moveToFormat;
	private String createFormat;
	private String changeFormat = "$" + MESSAGE + "$";
	protected KeyStroke moveKey;
	protected JPanel root;
	private PropertyChangeListener globalPropertyListener;

	public Map() {
		getView();
		theMap.addMouseListener(this);
		theMap.addMouseMotionListener(this);
		theMap.addFocusListener(this);
		toolBar.setFloatable(false);
	}
	public static final String NAME = "mapName";
	public static final String MARK_MOVED = "markMoved";
	public static final String MARK_UNMOVED_ICON = "markUnmovedIcon";
	public static final String EDGE_WIDTH = "edgeWidth";
	public static final String EDGE_HEIGHT = "edgeHeight";
	public static final String HIGHLIGHT_COLOR = "color";
	public static final String HIGHLIGHT_THICKNESS = "thickness";
	public static final String ALLOW_MULTIPLE = "allowMultiple";
	public static final String USE_LAUNCH_BUTTON = "launch";
	public static final String BUTTON_NAME = "buttonName";
	public static final String ICON = "icon";
	public static final String HOTKEY = "hotkey";
	public static final String SUPPRESS_AUTO = "suppressAuto";
	public static final String MOVE_WITHIN_FORMAT = "moveWithinFormat";
	public static final String MOVE_TO_FORMAT = "moveToFormat";
	public static final String CREATE_FORMAT = "createFormat";
	public static final String CHANGE_FORMAT = "changeFormat";
	public static final String MOVE_KEY = "moveKey";

	public void setAttribute(String key, Object value) {
		if (NAME.equals(key)) {
			setMapName((String) value);
		}
		else if (MARK_MOVED.equals(key)) {
			markMovedOption = (String) value;
		}
		else if (MARK_UNMOVED_ICON.equals(key)) {
			markUnmovedIcon = (String) value;
		}
		else if ("edge".equals(key)) { // Backward-compatible
			String s = (String) value;
			int i = s.indexOf(",");
			if (i > 0) {
				edgeBuffer = new Dimension(Integer.parseInt(s.substring(0, i)), Integer.parseInt(s.substring(i + 1)));
			}
		}
		else if (EDGE_WIDTH.equals(key)) {
			if (value instanceof String) {
				value = new Integer((String) value);
			}
			try {
				edgeBuffer = new Dimension(((Integer) value).intValue(), edgeBuffer.height);
			}
			catch (NumberFormatException ex) {
			}
		}
		else if (EDGE_HEIGHT.equals(key)) {
			if (value instanceof String) {
				value = new Integer((String) value);
			}
			try {
				edgeBuffer = new Dimension(edgeBuffer.width, ((Integer) value).intValue());
			}
			catch (NumberFormatException ex) {
			}
		}
		else if (ALLOW_MULTIPLE.equals(key)) {
			if (value instanceof String) {
				value = new Boolean((String) value);
			}
			allowMultiple = ((Boolean) value).booleanValue();
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
				value = new Integer((String) value);
			}
			if (highlighter instanceof ColoredBorder) {
				((ColoredBorder) highlighter).setThickness(((Integer) value).intValue());
			}
		}
		else if (USE_LAUNCH_BUTTON.equals(key)) {
			if (value instanceof String) {
				value = new Boolean((String) value);
			}
			useLaunchButton = ((Boolean) value).booleanValue();
			launchButton.setVisible(useLaunchButton);
		}
		else if (SUPPRESS_AUTO.equals(key)) {
			if (value instanceof String) {
				value = new Boolean((String) value);
			}
			if (Boolean.TRUE.equals(value)) {
				moveWithinFormat = "";
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
				value = HotKeyConfigurer.decode((String) value);
			}
			moveKey = (KeyStroke) value;
		}
		else {
			launchButton.setAttribute(key, value);
		}
	}

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
		else if (EDGE_WIDTH.equals(key)) {
			return "" + edgeBuffer.width;
		}
		else if (EDGE_HEIGHT.equals(key)) {
			return "" + edgeBuffer.height;
		}
		else if (ALLOW_MULTIPLE.equals(key)) {
			return "" + picker.isAllowMultiple();
		}
		else if (HIGHLIGHT_COLOR.equals(key)) {
			if (highlighter instanceof ColoredBorder) {
				return ColorConfigurer.colorToString(((ColoredBorder) highlighter).getColor());
			}
			else {
				return null;
			}
		}
		else if (HIGHLIGHT_THICKNESS.equals(key)) {
			if (highlighter instanceof ColoredBorder) {
				return "" + ((ColoredBorder) highlighter).getThickness();
			}
			else {
				return null;
			}
		}
		else if (USE_LAUNCH_BUTTON.equals(key)) {
			return String.valueOf(useLaunchButton);
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
			return HotKeyConfigurer.encode(moveKey);
		}
		else {
			return launchButton.getAttributeValueString(key);
		}
	}

	public void build(Element e) {
		ActionListener al = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (mainWindowDock == null && launchButton.isEnabled() && theMap.getTopLevelAncestor() != null) {
					theMap.getTopLevelAncestor().setVisible(!theMap.getTopLevelAncestor().isVisible());
				}
			}
		};
		launchButton = new LaunchButton("Map", BUTTON_NAME, HOTKEY, ICON, al);
		launchButton.setEnabled(false);
		launchButton.setVisible(false);
		if (e != null) {
			super.build(e);
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
			setMapName("Main Map");
		}
		if (!getComponents(GlobalProperties.class).hasMoreElements()) {
			addChild(new GlobalProperties());
		}
    if (!getComponents(GlobalProperties.class).hasMoreElements()) {
      addChild(new SelectionHighlighters());
    }
		setup(false);
	}

	private void addChild(Buildable b) {
		add(b);
		b.addTo(this);
	}

	/**
   * Every map must include a {@link BoardPicker} as one of its build components
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
   * Every map must include a {@link BoardPicker} as one of its build components
   * 
   * @return the BoardPicker for this map
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
   * A map may include a {@link Zoomer} as one of its build components
   */
	public void setZoomer(Zoomer z) {
		zoom = z;
	}

	/**
   * A map may include a {@link Zoomer} as one of its build components
   * 
   * @return the Zoomer for this map
   */
	public Zoomer getZoomer() {
		return zoom;
	}

	/**
   * Every map must include a {@link StackMetrics} as one of its build components, which governs the stacking behavior
   * of GamePieces on the map
   */
	public void setStackMetrics(StackMetrics sm) {
		metrics = sm;
	}

	/**
   * Every map must include a {@link StackMetrics} as one of its build components, which governs the stacking behavior
   * of GamePieces on the map
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
   * @return the current zoom factor for the map
   */
	public double getZoom() {
		return zoom == null ? 1.0 : zoom.getZoomFactor();
	}

	/**
   * @return the toolbar for this map's window
   */
	public JToolBar getToolBar() {
		return toolBar;
	}

	public PropertyChangeListener getPropertyListener() {
		if (globalPropertyListener == null) {
			globalPropertyListener = new PropertyChangeListener() {
				public void propertyChange(PropertyChangeEvent evt) {
					globalProperties.put(evt.getPropertyName(), evt.getNewValue());
					repaint();
				}
			};
		}
		return globalPropertyListener;
	}

	/**
   * Add a {@link Drawable} component to this map
   * 
   * @see #paint
   */
	public void addDrawComponent(Drawable theComponent) {
		drawComponents.addElement(theComponent);
	}

	/**
   * Remove a {@link Drawable} component from this map
   * 
   * @see #paint
   */
	public void removeDrawComponent(Drawable theComponent) {
		drawComponents.removeElement(theComponent);
	}

	/**
   * Expects to be added to a {@link GameModule}. Determines a unique id for this map. Registers itself as
   * {@link KeyStrokeSource}. Registers itself as a {@link GameComponent}. Registers itself as a drop target and drag
   * source
   * 
   * @see #getId
   * @see DragBuffer
   */
	public void addTo(Buildable b) {
		idMgr.add(this);
		validator = new CompoundValidityChecker(new MandatoryComponent(this, BoardPicker.class), new MandatoryComponent(this, StackMetrics.class)).append(idMgr);
		DragGestureListener dgl = new DragGestureListener() {
			public void dragGestureRecognized(DragGestureEvent dge) {
				if (mouseListenerStack.size() == 0 && dragGestureListener != null) {
					dragGestureListener.dragGestureRecognized(dge);
				}
			}
		};
		DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(theMap, DnDConstants.ACTION_MOVE, dgl);
		theMap.setDropTarget(PieceMover.DragHandler.makeDropTarget(theMap, DnDConstants.ACTION_MOVE, this));
		GameModule.getGameModule().getGameState().addGameComponent(this);
		GameModule.getGameModule().getToolBar().add(launchButton);
		if (shouldDockIntoMainWindow()) {
			IntConfigurer config = new IntConfigurer(MAIN_WINDOW_HEIGHT, null, new Integer(-1));
			GameModule.getGameModule().getGlobalPrefs().addOption(null, config);
			root = new JPanel(new BorderLayout());
			root.add(scroll, BorderLayout.CENTER);
			ComponentSplitter splitter = new ComponentSplitter();
			mainWindowDock = splitter.splitBottom(splitter.getSplitAncestor(GameModule.getGameModule().getControlPanel(), -1), root, true);
			GameModule.getGameModule().addKeyStrokeSource(new KeyStrokeSource(theMap, JComponent.WHEN_FOCUSED));
		}
		else {
			GameModule.getGameModule().addKeyStrokeSource(new KeyStrokeSource(theMap, JComponent.WHEN_IN_FOCUSED_WINDOW));
		}
		PlayerRoster.addSideChangeListener(this);
	}

	public void removeFrom(Buildable b) {
		GameModule.getGameModule().getGameState().removeGameComponent(this);
		Window w = SwingUtilities.getWindowAncestor(theMap);
		if (w != null) {
			w.dispose();
		}
		GameModule.getGameModule().getToolBar().remove(launchButton);
		idMgr.remove(this);
		if (picker != null) {
			GameModule.getGameModule().removeCommandEncoder(picker);
			GameModule.getGameModule().getGameState().addGameComponent(picker);
		}
		PlayerRoster.removeSideChangeListener(this);
	}

	public void sideChanged(String oldSide, String newSide) {
		repaint();
	}

	/**
   * Set the boards for this map. Each map may contain more than one {@link Board}
   */
	public synchronized void setBoards(Enumeration boardList) {
		boards.removeAllElements();
		System.gc();
		while (boardList.hasMoreElements()) {
			Board board = (Board) boardList.nextElement();
			board.setMap(this);
			boards.addElement(board);
		}
		setBoardBoundaries();
	}

	public Command getRestoreCommand() {
		return null;
	}

	/**
   * @return the {@link Board} on this map containing the argument point
   */
	public Board findBoard(Point p) {
		for (int i = 0; i < boards.size(); ++i) {
			Board b = (Board) boards.elementAt(i);
			if (b.bounds().contains(p))
				return b;
		}
		return null;
	}

	/**
   * 
   * @return the {@link Zone} on this map containing the argument point
   */
	public Zone findZone(Point p) {
		Board b = findBoard(p);
		if (b != null) {
			MapGrid grid = b.getGrid();
			if (grid != null && grid instanceof ZonedGrid) {
				return ((ZonedGrid) grid).findZone(p);
			}
		}
		return null;
	}

	/**
   * Return the board with the given name
   * 
   * @param name
   * @return null if no such board found
   */
	public Board getBoardByName(String name) {
		Board board = null;
		if (name != null) {
			for (Enumeration e = getAllBoards(); e.hasMoreElements();) {
				Board b = (Board) e.nextElement();
				if (name.equals(b.getName())) {
					board = b;
					break;
				}
			}
		}
		return board;
	}

	public Dimension getPreferredSize() {
		return new Dimension((int) (getZoom() * mapSize().width), (int) (getZoom() * mapSize().height));
	}

	/**
   * @return the size of the map in pixels at 100% zoom, including the edge buffer
   */
	public synchronized Dimension mapSize() {
		Rectangle r = new Rectangle(0, 0);
		for (int i = 0; i < boards.size(); ++i) {
			Board b = (Board) boards.elementAt(i);
			r = r.union(b.bounds());
		}
		r.width += edgeBuffer.width;
		r.height += edgeBuffer.height;
		return r.getSize();
	}

	/**
   * @return true if the given point may not be a local location. I.e., if this grid will attempt to snap it to the
   *         nearest grid location
   */
	public boolean isLocationRestricted(Point p) {
		Board b = findBoard(p);
		if (b != null) {
			Rectangle r = b.bounds();
			Point snap = new Point(p);
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
		Board b = findBoard(p);
		if (b == null)
			return snap;
		Rectangle r = b.bounds();
		snap.translate(-r.x, -r.y);
		snap = b.snapTo(snap);
		snap.translate(r.x, r.y);
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
   * The buffer of empty space around the boards in the Map window, in component coordinates at 100% zoom
   */
	public Dimension getEdgeBuffer() {
		return new Dimension(edgeBuffer);
	}

	/**
   * Translate a point from component coordinates (i.e. x,y position on the JPanel) to map coordinates, i.e. accounting
   * for zoom factor
   * 
   * @see #componentCoordinates
   */
	public Point mapCoordinates(Point p1) {
		Point p = new Point(p1.x, p1.y);
		p.x /= getZoom();
		p.y /= getZoom();
		return p;
	}

	public Rectangle mapRectangle(Rectangle r) {
		r = new Rectangle(r);
		r.x /= getZoom();
		r.y /= getZoom();
		r.width /= getZoom();
		r.height /= getZoom();
		return r;
	}

	/**
   * Translate a point from map coordinates to component coordinates
   * 
   * @see #mapCoordinates
   */
	public Point componentCoordinates(Point p1) {
		Point p = new Point(p1.x, p1.y);
		p.x *= getZoom();
		p.y *= getZoom();
		return p;
	}

	public Rectangle componentRectangle(Rectangle r) {
		r = new Rectangle(r);
		r.x *= getZoom();
		r.y *= getZoom();
		r.width *= getZoom();
		r.height *= getZoom();
		return r;
	}

	/**
   * @return a String name for the given location on the map
   * 
   * @see Board#locationName
   */
	public String locationName(Point p) {
		String loc = getDeckNameAt(p);
		if (loc == null) {
			Board b = findBoard(p);
			if (b != null) {
				loc = b.locationName(new Point(p.x - b.bounds().x, p.y - b.bounds().y));
			}
		}
		if (loc == null) {
			loc = "offboard";
		}
		return loc;
	}

	/**
   * @return a String name for the given location on the map. Include Map name if requested. Report deck name instead of
   *         location if point is inside the bounds of a deck. Do not include location if this map is not visible to all
   *         players.
   */
	public String getFullLocationName(Point p, boolean includeMap) {
		String loc = "";
		if (includeMap && getMapName() != null && getMapName().length() > 0) {
			loc = "[" + getMapName() + "]";
		}
		if (isVisibleToAll() && p != null) {
			String pos = getDeckNameContaining(p);
			if (pos == null) {
				if (locationName(p) != null) {
					loc = locationName(p) + loc;
				}
			}
			else {
				loc = pos;
			}
		}
		return loc;
	}

	/**
   * Is this map visible to all players
   */
	public boolean isVisibleToAll() {
		if (this instanceof PrivateMap) {
			if (!getAttributeValueString(PrivateMap.VISIBLE).equals("true")) {
				return false;
			}
		}
		return true;
	}

	/**
   * Return the name of the deck whose bounding box contains p
   */
	public String getDeckNameContaining(Point p) {
		String deck = null;
		if (p != null) {
			Enumeration e = getComponents(DrawPile.class);
			while (e.hasMoreElements()) {
				DrawPile d = (DrawPile) e.nextElement();
				Rectangle box = d.boundingBox();
				if (box != null && box.contains(p)) {
					deck = d.getConfigureName();
					break;
				}
			}
		}
		return deck;
	}

	/**
   * Return the name of the deck whose position is p
   * 
   * @param p
   * @return
   */
	public String getDeckNameAt(Point p) {
		String deck = null;
		if (p != null) {
			Enumeration e = getComponents(DrawPile.class);
			while (e.hasMoreElements()) {
				DrawPile d = (DrawPile) e.nextElement();
				if (d.getPosition().equals(p)) {
					deck = d.getConfigureName();
					break;
				}
			}
		}
		return deck;
	}

	public void focusGained(FocusEvent e) {
	}

	/**
   * When focus is lost, deselect all selected counters on this map.
   */
	public void focusLost(FocusEvent fe) {
		if (!fe.isTemporary()) {
			boolean dirty = false;
			java.util.List l = new ArrayList();
			for (Enumeration e = KeyBuffer.getBuffer().getPieces(); e.hasMoreElements();) {
				l.add(e.nextElement());
			}
			for (Iterator it = l.iterator(); it.hasNext();) {
				GamePiece p = (GamePiece) it.next();
				if (p.getMap() == this) {
					KeyBuffer.getBuffer().remove(p);
					dirty = true;
				}
			}
			if (dirty) {
				theMap.repaint();
			}
		}
	}

	/**
   * Because MouseEvents are received in component coordinates, it is inconvenient for MouseListeners on the map to have
   * to translate to map coordinates. MouseListeners added with this method will receive mouse events with points
   * already translated into map coordinates
   */
	public void addLocalMouseListener(MouseListener l) {
		multicaster = AWTEventMulticaster.add(multicaster, l);
	}

	public void removeLocalMouseListener(MouseListener l) {
		multicaster = AWTEventMulticaster.remove(multicaster, l);
	}

	/**
   * MouseListeners on a map may be pushed and popped onto a stack. Only the top listener on the stack receives mouse
   * events
   */
	public void pushMouseListener(MouseListener l) {
		mouseListenerStack.addElement(l);
	}

	/**
   * MouseListeners on a map may be pushed and popped onto a stack. Only the top listener on the stack receives mouse
   * events
   */
	public void popMouseListener() {
		mouseListenerStack.removeElement(mouseListenerStack.lastElement());
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	/**
   * Mouse events are first translated into map coordinates. Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   * 
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
   */
	public void mouseClicked(MouseEvent e) {
		if (mouseListenerStack.size() > 0) {
			Point p = mapCoordinates(e.getPoint());
			e.translatePoint(p.x - e.getX(), p.y - e.getY());
			((MouseListener) mouseListenerStack.lastElement()).mouseClicked(e);
		}
		else if (multicaster != null) {
			Point p = mapCoordinates(e.getPoint());
			e.translatePoint(p.x - e.getX(), p.y - e.getY());
			multicaster.mouseClicked(e);
		}
	}

	/**
   * Mouse events are first translated into map coordinates. Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   * 
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
   */
	public void mousePressed(MouseEvent e) {
		if (mouseListenerStack.size() > 0) {
			Point p = mapCoordinates(e.getPoint());
			e.translatePoint(p.x - e.getX(), p.y - e.getY());
			((MouseListener) mouseListenerStack.lastElement()).mousePressed(e);
		}
		else if (multicaster != null) {
			Point p = mapCoordinates(e.getPoint());
			e.translatePoint(p.x - e.getX(), p.y - e.getY());
			multicaster.mousePressed(e);
		}
	}

	/**
   * Mouse events are first translated into map coordinates. Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   * 
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
   */
	public void mouseReleased(MouseEvent e) {
		Point p = e.getPoint();
		p.translate(theMap.getLocation().x, theMap.getLocation().y);
		if (theMap.getBounds().contains(p)) {
			if (mouseListenerStack.size() > 0) {
				p = mapCoordinates(e.getPoint());
				e.translatePoint(p.x - e.getX(), p.y - e.getY());
				((MouseListener) mouseListenerStack.lastElement()).mouseReleased(e);
			}
			else if (multicaster != null) {
				p = mapCoordinates(e.getPoint());
				e.translatePoint(p.x - e.getX(), p.y - e.getY());
				multicaster.mouseReleased(e);
			}
			// Request Focus so that keyboard input will be recognized
			theMap.requestFocus();
		}
		// Clicking with mouse always repaints the map
		clearFirst = true;
		theMap.repaint();
	}

	/**
   * This listener will be notified when a drag event is initiated, assuming that no MouseListeners are on the stack.
   * 
   * @see #pushMouseListener
   * @param dragGestureListener
   */
	public void setDragGestureListener(DragGestureListener dragGestureListener) {
		this.dragGestureListener = dragGestureListener;
	}

	public DragGestureListener getDragGestureListener() {
		return dragGestureListener;
	}

	/*
   * Restart scroll delay timer when re-entering map
   */
	public void dragEnter(DropTargetDragEvent dtde) {
		restartDelay();
	}

	public void dragOver(DropTargetDragEvent dtde) {
		scrollAtEdge(dtde.getLocation(), 15);
	}

	public void dropActionChanged(DropTargetDragEvent dtde) {
	}

	/*
   * Cancel final scroll and repaint map
   */
	public void dragExit(DropTargetEvent dte) {
		scroll_dx = 0;
		scroll_dy = 0;
		repaint();
	}

	public void drop(DropTargetDropEvent dtde) {
		if (dtde.getDropTargetContext().getComponent() == theMap) {
			MouseEvent evt = new MouseEvent(theMap, MouseEvent.MOUSE_RELEASED, System.currentTimeMillis(), 0, dtde.getLocation().x, dtde.getLocation().y, 1, false);
			theMap.dispatchEvent(evt);
		}
	}

	/**
   * Mouse motion events are not forwarded to LocalMouseListeners or to listeners on the stack
   */
	public void mouseMoved(MouseEvent e) {
	}

	/**
   * Mouse motion events are not forwarded to LocalMouseListeners or to listeners on the stack
   * 
   * The map scrolls when dragging the mouse near the edge
   */
	public void mouseDragged(MouseEvent e) {
		if (!e.isMetaDown()) {
			scrollAtEdge(e.getPoint(), 15);
		}
		else {
			restartDelay();
		}
	}
	/*
   * Delay before starting scroll at edge
   */
	protected Thread scrollDelayThread;
	protected int scrollDelay = 200;
	protected long scrollExpirationTime;
	protected int scroll_dist;
	protected int scroll_dx;
	protected int scroll_dy;

	/**
   * Scoll map so that the argument point is at least a certain distance from the visible edte
   * 
   * @param evtPt
   */
	public void scrollAtEdge(Point evtPt, int dist) {
		Point p = new Point(evtPt.x - scroll.getViewport().getViewPosition().x, evtPt.y - scroll.getViewport().getViewPosition().y);
		int dx = 0, dy = 0;
		if (p.x < dist && p.x >= 0)
			dx = -1;
		if (p.x >= scroll.getViewport().getSize().width - dist && p.x < scroll.getViewport().getSize().width)
			dx = 1;
		if (p.y < dist && p.y >= 0)
			dy = -1;
		if (p.y >= scroll.getViewport().getSize().height - dist && p.y < scroll.getViewport().getSize().height)
			dy = 1;
		scroll_dx = dx;
		scroll_dy = dy;
		scroll_dist = dist;
		if (dx != 0 || dy != 0) {
			if (scrollDelayThread == null || !scrollDelayThread.isAlive()) {
				scrollDelayThread = new Thread(this);
				scrollDelayThread.start();
			}
		}
		else {
			restartDelay();
		}
	}

	protected void restartDelay() {
		scrollExpirationTime = System.currentTimeMillis() + scrollDelay;
	}

	public void run() {
		while (System.currentTimeMillis() < scrollExpirationTime) {
			try {
				Thread.sleep(Math.max(0, scrollExpirationTime - System.currentTimeMillis()));
			}
			catch (InterruptedException e) {
			}
		}
		if (scroll_dx != 0 || scroll_dy != 0) {
			scroll(2 * scroll_dist * scroll_dx, 2 * scroll_dist * scroll_dy);
		}
	}

	public void repaint(boolean cf) {
		clearFirst = cf;
		theMap.repaint();
	}

	/**
   * Painting the map is done in three steps: 1) draw each of the {@link Board}s on the map. 2) draw all of the
   * counters on the map. 3) draw all of the {@link Drawable} components on the map
   * 
   * @see #addDrawComponent
   * @see #setBoards
   * @see #addPiece
   */
	public void paint(Graphics g) {
		paint(g, 0, 0);
	}

	public void paintRegion(Graphics g, Rectangle visibleRect) {
		clearMapBorder(g); // To avoid ghost pieces around the edge
		drawBoardsInRegion(g, visibleRect);
		drawDrawable(g, false);
		drawPiecesInRegion(g, visibleRect);
		drawDrawable(g, true);
	}

	public void drawBoardsInRegion(Graphics g, Rectangle visibleRect) {
		for (int i = 0; i < boards.size(); ++i) {
			Board b = (Board) boards.elementAt(i);
			Point p = getLocation(b, getZoom());
			b.drawRegion(g, p, visibleRect, getZoom(), theMap);
		}
	}

	public void repaint() {
		theMap.repaint();
	}

	public void drawPiecesInRegion(Graphics g, Rectangle visibleRect) {
		if (!hideCounters) {
			Graphics2D g2d = (Graphics2D) g;
			Composite oldComposite = g2d.getComposite();
			g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));
			GamePiece[] stack = pieces.getPieces();
			for (int i = 0; i < stack.length; ++i) {
				Point pt = componentCoordinates(stack[i].getPosition());
				if (stack[i].getClass() == Stack.class) {
					getStackMetrics().draw((Stack) stack[i], pt, g, this, getZoom(), visibleRect);
				}
				else {
					stack[i].draw(g, pt.x, pt.y, theMap, getZoom());
					if (Boolean.TRUE.equals(stack[i].getProperty(Properties.SELECTED))) {
						highlighter.draw(stack[i], g, pt.x, pt.y, theMap, getZoom());
					}
				}
			}
			g2d.setComposite(oldComposite);
		}
	}

	public void drawPieces(Graphics g, int xOffset, int yOffset) {
		if (!hideCounters) {
			Graphics2D g2d = (Graphics2D) g;
			Composite oldComposite = g2d.getComposite();
			g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));
			GamePiece[] stack = pieces.getPieces();
			for (int i = 0; i < stack.length; ++i) {
				Point pt = componentCoordinates(stack[i].getPosition());
				stack[i].draw(g, pt.x + xOffset, pt.y + yOffset, theMap, getZoom());
				if (Boolean.TRUE.equals(stack[i].getProperty(Properties.SELECTED))) {
					highlighter.draw(stack[i], g, pt.x - xOffset, pt.y - yOffset, theMap, getZoom());
				}
			}
			g2d.setComposite(oldComposite);
		}
	}

	public void drawDrawable(Graphics g, boolean aboveCounters) {
		for (Enumeration e = drawComponents.elements(); e.hasMoreElements();) {
			Drawable drawable = (Drawable) e.nextElement();
			if (!(aboveCounters ^ drawable.drawAboveCounters())) {
				drawable.draw(g, this);
			}
		}
	}

	/**
   * Paint the map at the given offset, i.e. such that (xOffset, yOffset) is in the upper left corner
   */
	public void paint(Graphics g, int xOffset, int yOffset) {
		drawBoards(g, xOffset, yOffset, getZoom(), theMap);
		drawDrawable(g, false);
		drawPieces(g, xOffset, yOffset);
		drawDrawable(g, true);
	}

	public Highlighter getHighlighter() {
		return highlighter;
	}

	public void setHighlighter(Highlighter h) {
		highlighter = h;
	}
  
  public void addHighlighter(Highlighter h) {
    highlighters.add(h);
  }
  
  public void removeHighlighter(Highlighter h) {
    highlighters.remove(h);
  }
  
  public Iterator getHighlighters() {
    return highlighters.iterator();
  }

	/**
   * @return an Enumeration of all {@link Board}s on the map
   */
	public Enumeration getAllBoards() {
		return boards.elements();
	}

	public int getBoardCount() {
		return boards.size();
	}

	/**
   * Returns the boundingBox of a GamePiece accounting for the offset of a piece within its parent stack. Return null if
   * this piece is not on the map
   * 
   * @see GamePiece#boundingBox
   */
	public Rectangle boundingBoxOf(GamePiece p) {
		Rectangle r = null;
		if (p.getMap() == this) {
			r = p.boundingBox();
			Point pos = p.getPosition();
			r.translate(pos.x, pos.y);
			if (Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
				r = r.union(highlighter.boundingBox(p));
        for (Iterator i = getHighlighters(); i.hasNext();) {
          r = r.union(((Highlighter) i).boundingBox(p));
        }
			}
			if (p.getParent() != null) {
				Point pt = getStackMetrics().relativePosition(p.getParent(), p);
				r.translate(pt.x, pt.y);
			}
		}
		return r;
	}

	/**
   * Returns the selection bounding box of a GamePiece accounting for the offset of a piece within a stack
   * 
   * @see GamePiece#getShape
   */
	public Rectangle selectionBoundsOf(GamePiece p) {
		if (p.getMap() != this) {
			throw new RuntimeException("Piece is not on this map");
		}
		Rectangle r = p.getShape().getBounds();
		r.translate(p.getPosition().x, p.getPosition().y);
		if (p.getParent() != null) {
			Point pt = getStackMetrics().relativePosition(p.getParent(), p);
			r.translate(pt.x, pt.y);
		}
		return r;
	}

	/**
   * Returns the position of a GamePiece accounting for the offset within a parent stack, if any
   */
	public Point positionOf(GamePiece p) {
		if (p.getMap() != this) {
			throw new RuntimeException("Piece is not on this map");
		}
		Point point = p.getPosition();
		if (p.getParent() != null) {
			Point pt = getStackMetrics().relativePosition(p.getParent(), p);
			point.translate(pt.x, pt.y);
		}
		return point;
	}

	/**
   * @return an array of all GamePieces on the map. This is a read-only copy. Altering the array does not alter the
   *         pieces on the map.
   */
	public GamePiece[] getPieces() {
		return pieces.getPieces();
	}

	public GamePiece[] getAllPieces() {
		return pieces.getAllPieces();
	}

	public void setPieceCollection(PieceCollection pieces) {
		this.pieces = pieces;
	}

	public PieceCollection getPieceCollection() {
		return pieces;
	}

	protected void clearMapBorder(Graphics g) {
		if (clearFirst || boards.size() == 0) {
			g.clearRect(0, 0, theMap.getSize().width, theMap.getSize().height);
			clearFirst = false;
		}
		else {
			Dimension buffer = new Dimension((int) (getZoom() * edgeBuffer.width), (int) (getZoom() * edgeBuffer.height));
			g.clearRect(0, 0, buffer.width, theMap.getSize().height);
			g.clearRect(0, 0, theMap.getSize().width, buffer.height);
			g.clearRect(theMap.getSize().width - buffer.width, 0, buffer.width, theMap.getSize().height);
			g.clearRect(0, theMap.getSize().height - buffer.height, theMap.getSize().width, buffer.height);
		}
	}

	/**
   * Adjusts the bounds() rectangle to account for the Board's relative position to other boards. In other words, if
   * Board A is N pixels wide and Board B is to the right of Board A, then the origin of Board B will be adjusted N
   * pixels to the right.
   */
	protected void setBoardBoundaries() {
		int maxX = 0;
		int maxY = 0;
		for (int i = 0, n = boards.size(); i < n; ++i) {
			Point relPos = ((Board) boards.get(i)).relativePosition();
			maxX = Math.max(maxX, relPos.x);
			maxY = Math.max(maxY, relPos.y);
		}
		boardWidths = new int[maxX + 1][maxY + 1];
		boardHeights = new int[maxX + 1][maxY + 1];
		for (int i = 0, n = boards.size(); i < n; ++i) {
			Board b = (Board) boards.get(i);
			Point relPos = b.relativePosition();
			boardWidths[relPos.x][relPos.y] = b.bounds().width;
			boardHeights[relPos.x][relPos.y] = b.bounds().height;
		}
		Point offset = new Point(edgeBuffer.width, edgeBuffer.height);
		for (int i = 0, n = boards.size(); i < n; ++i) {
			Board b = (Board) boards.get(i);
			Point relPos = b.relativePosition();
			Point location = getLocation(relPos.x, relPos.y, 1.0);
			b.setLocation(location.x, location.y);
			b.translate(offset.x, offset.y);
		}
		theMap.revalidate();
	}

	protected Point getLocation(Board b, double zoom) {
		Point p;
		if (zoom == 1.0) {
			p = b.bounds().getLocation();
		}
		else {
			Point relPos = b.relativePosition();
			p = getLocation(relPos.x, relPos.y, zoom);
			p.translate((int) (zoom * edgeBuffer.width), (int) (zoom * edgeBuffer.height));
		}
		return p;
	}

	protected Point getLocation(int column, int row, double zoom) {
		Point p = new Point();
		for (int x = 0; x < column; ++x) {
			p.translate((int) Math.floor(zoom * boardWidths[x][row]), 0);
		}
		for (int y = 0; y < row; ++y) {
			p.translate(0, (int) Math.floor(zoom * boardHeights[column][y]));
		}
		return p;
	}

	/**
   * Draw the boards of the map at the given point and zoom factor onto the given Graphics object
   */
	public void drawBoards(Graphics g, int xoffset, int yoffset, double zoom, Component obs) {
		for (int i = 0; i < boards.size(); ++i) {
			Board b = (Board) boards.elementAt(i);
			Point p = getLocation(b, zoom);
			p.translate(xoffset, yoffset);
			b.draw(g, p.x, p.y, zoom, obs);
		}
	}

	/**
   * Repaint the given area, specified in map coordinates
   */
	public void repaint(Rectangle r) {
		r.setLocation(componentCoordinates(new Point(r.x, r.y)));
		r.setSize((int) (r.width * getZoom()), (int) (r.height * getZoom()));
		theMap.repaint(r.x, r.y, r.width, r.height);
	}

	/**
   * @param show
   *          if true, enable drawing of GamePiece. If false, don't draw GamePiece when painting the map
   */
	public void setPiecesVisible(boolean show) {
		hideCounters = !show;
	}

	public boolean isPiecesVisible() {
		return !hideCounters && pieceOpacity != 0;
	}

	public float getPieceOpacity() {
		return pieceOpacity;
	}

	public void setPieceOpacity(float pieceOpacity) {
		this.pieceOpacity = pieceOpacity;
	}

	public Object getProperty(Object key) {
		Object value = null;
		value = globalProperties.get(key);
		if (value == null) {
			value = GameModule.getGameModule().getProperty(key);
		}
		return value;
	}

	public KeyStroke getMoveKey() {
		return moveKey;
	}

	/**
   * @return the top-level window containing this map
   */
	protected Window createParentFrame() {
		if (GlobalOptions.getInstance().isUseSingleWindow()) {
			JDialog d = new JDialog(GameModule.getGameModule().getFrame());
			d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
			d.setTitle(getDefaultWindowTitle());
			return d;
		}
		else {
			JFrame d = new JFrame();
			d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
			d.setTitle(getDefaultWindowTitle());
			return d;
		}
	}

	public boolean shouldDockIntoMainWindow() {
		boolean shouldDock = false;
		if (GlobalOptions.getInstance().isUseSingleWindow() && !useLaunchButton) {
			shouldDock = true;
			for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
				Map m = (Map) e.nextElement();
				if (m == this) {
					break;
				}
				if (m.shouldDockIntoMainWindow()) {
					shouldDock = false;
					break;
				}
			}
		}
		return shouldDock;
	}

	/**
   * When a game is started, create a top-level window, if none exists. When a game is ended, remove all boards from the
   * map
   * 
   * @see GameComponent
   */
	public void setup(boolean show) {
		if (show) {
			if (shouldDockIntoMainWindow()) {
				mainWindowDock.showComponent();
				int height = ((Integer) GameModule.getGameModule().getGlobalPrefs().getValue(MAIN_WINDOW_HEIGHT)).intValue();
				if (height > 0) {
					Container top = mainWindowDock.getTopLevelAncestor();
					top.setSize(top.getSize().width, height);
				}
				if (toolBar.getParent() == null) {
					GameModule.getGameModule().getToolBar().addSeparator();
					GameModule.getGameModule().getToolBar().add(toolBar);
				}
				toolBar.setVisible(true);
			}
			else {
				if (SwingUtilities.getWindowAncestor(theMap) == null) {
					final Window topWindow = createParentFrame();
					topWindow.addWindowListener(new WindowAdapter() {
						public void windowClosing(WindowEvent e) {
							if (useLaunchButton) {
								topWindow.setVisible(false);
							}
							else {
								GameModule.getGameModule().getGameState().setup(false);
							}
						}
					});
					((RootPaneContainer) topWindow).getContentPane().add("North", getToolBar());
					((RootPaneContainer) topWindow).getContentPane().add("Center", scroll);
					topWindow.setSize(600, 400);
					PositionOption option = new PositionOption(PositionOption.key + getIdentifier(), topWindow);
					GameModule.getGameModule().getPrefs().addOption(option);
				}
				theMap.getTopLevelAncestor().setVisible(!useLaunchButton);
				theMap.revalidate();
			}
		}
		else {
			pieces.clear();
			boards.removeAllElements();
			System.gc();
			if (mainWindowDock != null) {
				if (mainWindowDock.getHideableComponent().isVisible()) {
					GameModule.getGameModule().getGlobalPrefs().getOption(MAIN_WINDOW_HEIGHT)
							.setValue(new Integer(mainWindowDock.getTopLevelAncestor().getSize().height));
				}
				mainWindowDock.hideComponent();
				toolBar.setVisible(false);
			}
			else if (theMap.getTopLevelAncestor() != null) {
				theMap.getTopLevelAncestor().setVisible(false);
			}
		}
		launchButton.setEnabled(show);
    launchButton.setVisible(useLaunchButton);
	}

	public void appendToTitle(String s) {
		if (mainWindowDock == null) {
			Component c = theMap.getTopLevelAncestor();
			if (s == null) {
				if (c instanceof JFrame) {
					((JFrame) c).setTitle(getDefaultWindowTitle());
				}
				if (c instanceof JDialog) {
					((JDialog) c).setTitle(getDefaultWindowTitle());
				}
			}
			else {
				if (c instanceof JFrame) {
					((JFrame) c).setTitle(((JFrame) c).getTitle() + s);
				}
				if (c instanceof JDialog) {
					((JDialog) c).setTitle(((JDialog) c).getTitle() + s);
				}
			}
		}
	}

	protected String getDefaultWindowTitle() {
		return getMapName().length() > 0 ? getMapName() : GameModule.getGameModule().getGameName() + " map";
	}

	/**
   * Use the provided {@link PieceFinder} instance to locate a visible piece at the given location
   */
	public GamePiece findPiece(Point pt, PieceFinder finder) {
		GamePiece[] stack = pieces.getPieces();
		for (int i = stack.length - 1; i >= 0; --i) {
			GamePiece p = finder.select(this, stack[i], pt);
			if (p != null) {
				return p;
			}
		}
		return null;
	}

	/**
   * Use the provided {@link PieceFinder} instance to locate any piece at the given location, regardless of whether it
   * is visible or not
   */
	public GamePiece findAnyPiece(Point pt, PieceFinder finder) {
		GamePiece[] stack = pieces.getAllPieces();
		for (int i = stack.length - 1; i >= 0; --i) {
			GamePiece p = finder.select(this, stack[i], pt);
			if (p != null) {
				return p;
			}
		}
		return null;
	}

	/**
   * Place a piece at the destination point. If necessary, remove the piece from its parent Stack or Map
   * 
   * @return a {@link Command} that reproduces this action
   */
	public Command placeAt(GamePiece piece, Point pt) {
		Command c = null;
		if (GameModule.getGameModule().getGameState().getPieceForId(piece.getId()) == null) {
			piece.setPosition(pt);
			addPiece(piece);
			GameModule.getGameModule().getGameState().addPiece(piece);
			c = new AddPiece(piece);
		}
		else {
			MoveTracker tracker = new MoveTracker(piece);
			piece.setPosition(pt);
			addPiece(piece);
			c = tracker.getMoveCommand();
		}
		return c;
	}

	/**
   * Apply the provided {@link PieceVisitorDispatcher} to all pieces on this map. Returns the first non-null
   * {@link Command} returned by <code>commandFactory</code>
   * 
   * @param commandFactory
   * 
   */
	public Command apply(PieceVisitorDispatcher commandFactory) {
		GamePiece[] stack = pieces.getPieces();
		Command c = null;
		for (int i = 0; i < stack.length && c == null; ++i) {
			c = (Command) commandFactory.accept(stack[i]);
		}
		return c;
	}

	/**
   * Move a piece to the destination point. If a piece is at the point (i.e. has a location exactly equal to it), merge
   * with the piece by forwarding to {@link StackMetrics#merge}. Otherwise, place by forwarding to placeAt()
   * 
   * @see StackMetrics#merge
   */
	public Command placeOrMerge(final GamePiece p, final Point pt) {
		Command c = apply(new DeckVisitorDispatcher(new Merger(this, pt, p)));
		if (c == null || c.isNull()) {
			c = placeAt(p, pt);
		}
		return c;
	}

	/**
   * Adds a GamePiece to this map. Removes the piece from its parent Stack and from its current map, if different from
   * this map
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
	public void reposition(GamePiece s, int pos) {
	}

	/**
   * Returns the index of a piece. When painting the map, pieces are drawn in order of index Return -1 if the piece is
   * not on this map
   */
	public int indexOf(GamePiece s) {
		return pieces.indexOf(s);
	}

	/**
   * Removes a piece from the map
   */
	public void removePiece(GamePiece p) {
		pieces.remove(p);
		theMap.repaint();
	}

	/**
   * Center the map at given map coordinates within its JScrollPane container
   */
	public void centerAt(Point p) {
		centerAt(p, 0, 0);
	}

	/**
   * Center the map at the given map coordinates, if the point is not already within (dx,dy) of the center
   */
	public void centerAt(Point p, int dx, int dy) {
		if (scroll != null) {
			p = componentCoordinates(p);
			p.translate(-scroll.getViewport().getSize().width / 2, -scroll.getViewport().getSize().height / 2);
			Rectangle r = new Rectangle(p, scroll.getViewport().getSize());
			r.width = dx > r.width ? 0 : r.width - dx;
			r.height = dy > r.height ? 0 : r.height - dy;
			theMap.scrollRectToVisible(r);
		}
	}

	/** Ensure that the given region (in map coordinates) is visible */
	public void ensureVisible(Rectangle r) {
		if (scroll != null) {
			Point p = componentCoordinates(r.getLocation());
			r = new Rectangle(p.x, p.y, (int) (getZoom() * r.width), (int) (getZoom() * r.height));
			theMap.scrollRectToVisible(r);
		}
	}

	/**
   * Scrolls the map in the containing JScrollPane
   * 
   * @param dx
   *          number of pixels to scroll horizontally
   * @param dy
   *          number of pixels to scroll vertically
   */
	public void scroll(int dx, int dy) {
		Rectangle r = new Rectangle(scroll.getViewport().getViewRect());
		r.translate(dx, dy);
		r = r.intersection(new Rectangle(new Point(0, 0), getPreferredSize()));
		theMap.scrollRectToVisible(r);
	}

	public static String getConfigureTypeName() {
		return "Map Window";
	}

	public String getMapName() {
		return mapName;
	}

	public void setMapName(String s) {
		mapName = s;
		setConfigureName(mapName);
		launchButton.setToolTipText(s != null ? "Show/hide " + s + " window" : "Show/hide Map window");
	}

	public HelpFile getHelpFile() {
		File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
		dir = new File(dir, "ReferenceManual");
		try {
			return new HelpFile(null, new File(dir, "Map.htm"));
		}
		catch (MalformedURLException ex) {
			return null;
		}
	}

	public String[] getAttributeDescriptions() {
		return new String[] { "Map Name", "Mark pieces that move (if they possess the proper trait)", "\"Mark unmoved\" button icon", "Horizontal Padding",
				"Vertical Padding", "Can contain multiple boards", "Border color for selected counters", "Border thickness for selected counters",
				"Include toolbar button to show/hide", "Toolbar button name", "Toolbar button icon", "Hotkey", "Auto-report format for movement within this map",
				"Auto-report format for movement to this map", "Auto-report format for units created in this map", "Auto-report format for units modified on this map",
				"Key Command to apply to all units ending movement on this map" };
	}

	public String[] getAttributeNames() {
		return new String[] { NAME, MARK_MOVED, MARK_UNMOVED_ICON, EDGE_WIDTH, EDGE_HEIGHT, ALLOW_MULTIPLE, HIGHLIGHT_COLOR, HIGHLIGHT_THICKNESS,
				USE_LAUNCH_BUTTON, BUTTON_NAME, ICON, HOTKEY, MOVE_WITHIN_FORMAT, MOVE_TO_FORMAT, CREATE_FORMAT, CHANGE_FORMAT, MOVE_KEY };
	}

	public Class[] getAttributeTypes() {
		return new Class[] { String.class, GlobalOptions.Prompt.class, UnmovedIconConfig.class, Integer.class, Integer.class, Boolean.class, Color.class,
				Integer.class, Boolean.class, String.class, IconConfig.class, KeyStroke.class, MoveWithinFormatConfig.class, MoveToFormatConfig.class,
				CreateFormatConfig.class, ChangeFormatConfig.class, KeyStroke.class };
	}
	public static final String LOCATION = "location";
	public static final String OLD_LOCATION = "previousLocation";
	public static final String OLD_MAP = "previousMap";
	public static final String MAP_NAME = "mapName";
	public static final String PIECE_NAME = "pieceName";
	public static final String MESSAGE = "message";
	public static class IconConfig implements ConfigurerFactory {
		public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
			return new IconConfigurer(key, name, "/images/map.gif");
		}
	}
	public static class UnmovedIconConfig implements ConfigurerFactory {
		public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
			return new IconConfigurer(key, name, "/images/unmoved.gif");
		}
	}
	public static class MoveWithinFormatConfig implements ConfigurerFactory {
		public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
			return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, LOCATION, MAP_NAME, OLD_LOCATION });
		}
	}
	public static class MoveToFormatConfig implements ConfigurerFactory {
		public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
			return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, LOCATION, OLD_MAP, MAP_NAME, OLD_LOCATION });
		}
	}
	public static class CreateFormatConfig implements ConfigurerFactory {
		public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
			return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, MAP_NAME, LOCATION });
		}
	}
	public static class ChangeFormatConfig implements ConfigurerFactory {
		public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
			return new PlayerIdFormattedStringConfigurer(key, name, new String[] { MESSAGE, ReportState.COMMAND_NAME, ReportState.OLD_UNIT_NAME,
					ReportState.NEW_UNIT_NAME, ReportState.MAP_NAME, ReportState.LOCATION_NAME });
		}
	}

	public String getCreateFormat() {
		if (createFormat != null) {
			return createFormat;
		}
		else {
			String val = "$" + PIECE_NAME + "$ created in $" + LOCATION + "$";
			if (boards.size() > 0) {
				Board b = (Board) boards.firstElement();
				if (b.getGrid() == null || b.getGrid().getGridNumbering() == null) {
					val = "";
				}
			}
			return val;
		}
	}

	public String getChangeFormat() {
		return changeFormat;
	}

	public String getMoveToFormat() {
		if (moveToFormat != null) {
			return moveToFormat;
		}
		else {
			String val = "$" + PIECE_NAME + "$" + " moves $" + OLD_LOCATION + "$ -> $" + LOCATION + "$ *";
			if (boards.size() > 0) {
				Board b = (Board) boards.firstElement();
				if (b.getGrid() == null || b.getGrid().getGridNumbering() != null) {
					val = "";
				}
			}
			return val;
		}
	}

	public String getMoveWithinFormat() {
		if (moveWithinFormat != null) {
			return moveWithinFormat;
		}
		else {
			String val = "$" + PIECE_NAME + "$" + " moves $" + OLD_LOCATION + "$ -> $" + LOCATION + "$ *";
			if (boards.size() > 0) {
				Board b = (Board) boards.firstElement();
				if (b.getGrid() == null) {
					val = "";
				}
			}
			return val;
		}
	}

	public Class[] getAllowableConfigureComponents() {
		Class[] c = { GlobalMap.class, LOS_Thread.class, ToolbarMenu.class, MultiActionButton.class, HidePiecesButton.class, Zoomer.class,
				CounterDetailViewer.class, HighlightLastMoved.class, LayeredPieceCollection.class, ImageSaver.class, TextSaver.class, DrawPile.class, SetupStack.class,
				MassKeyCommand.class, MapShader.class, PieceRecenterer.class };
		return c;
	}

	public VisibilityCondition getAttributeVisibility(String name) {
		if (visibilityCondition == null) {
			visibilityCondition = new VisibilityCondition() {
				public boolean shouldBeVisible() {
					return useLaunchButton;
				}
			};
		}
		if (HOTKEY.equals(name)) {
			return visibilityCondition;
		}
		else if (BUTTON_NAME.equals(name)) {
			return visibilityCondition;
		}
		else {
			return super.getAttributeVisibility(name);
		}
	}

	/**
   * Each Map must have a unique String id
   */
	public void setId(String id) {
		mapID = id;
	}

	public static Map getMapById(String id) {
		return (Map) idMgr.findInstance(id);
	}

	/**
   * Utility method to return a list of all map components in the module
   * 
   * @return
   */
	public static Iterator getAllMaps() {
		List l = new ArrayList();
		for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
			Map m = (Map) e.nextElement();
			l.add(m);
		}
		for (Enumeration charts = GameModule.getGameModule().getComponents(ChartWindow.class); charts.hasMoreElements();) {
			ChartWindow w = (ChartWindow) charts.nextElement();
			for (Enumeration e = w.getAllDescendantComponents(MapWidget.class); e.hasMoreElements();) {
				l.add(((MapWidget) e.nextElement()).getMap());
			}
		}
		return l.iterator();
	}

	/**
   * Each Map must have a unique String id
   * 
   * @return the id for this map
   */
	public String getId() {
		return mapID;
	}

	/**
   * Make a best gues for a unique identifier for the target. Use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getConfigureName} if non-null, otherwise use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getId}
   * 
   * @return
   */
	public String getIdentifier() {
		return UniqueIdManager.getIdentifier(this);
	}

	/** Return the AWT component representing the map */
	public JComponent getView() {
		if (theMap == null) {
			theMap = new View(this);
			scroll = new AdjustableSpeedScrollPane(
            theMap,
            JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			scroll.unregisterKeyboardAction(KeyStroke.getKeyStroke(
            KeyEvent.VK_PAGE_DOWN, 0));
			scroll.unregisterKeyboardAction(KeyStroke.getKeyStroke(
            KeyEvent.VK_PAGE_UP, 0));
		}
		return theMap;
	}
	/**
   * Implements default logic for merging pieces at a given location within a map Returns a {@link Command} that merges
   * the input {@link GamePiece} with an existing piece at the input position, provided the pieces are stackable,
   * visible, in the same layer, etc.
   */
	public static class Merger implements DeckVisitor {
		private Point pt;
		private Map map;
		private GamePiece p;

		public Merger(Map map, Point pt, GamePiece p) {
			this.map = map;
			this.pt = pt;
			this.p = p;
		}

		public Object visitDeck(Deck d) {
			if (d.getPosition().equals(pt)) {
				return map.getStackMetrics().merge(d, p);
			}
			else {
				return null;
			}
		}

		public Object visitStack(Stack s) {
			if (s.getPosition().equals(pt) && map.getStackMetrics().isStackingEnabled() && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
					&& s.topPiece() != null && map.getPieceCollection().canMerge(s, p)) {
				return map.getStackMetrics().merge(s, p);
			}
			else {
				return null;
			}
		}

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
   * The component that represents the map itself
   */
	public static class View extends JPanel {
		protected Map map;

		public View(Map m) {
      setFocusTraversalKeysEnabled(false);
			map = m;
		}

		public void paint(Graphics g) {
			Rectangle r = getVisibleRect();
			g.clearRect(r.x, r.y, r.width, r.height);
			map.paintRegion(g, r);
		}

		public void update(Graphics g) {
			// To avoid flicker, don't clear the display first *
			paint(g);
		}

		public Dimension getPreferredSize() {
			return map.getPreferredSize();
		}

		public Map getMap() {
			return map;
		}
	}
}
