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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
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

import net.miginfocom.swing.MigLayout;

import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.TimingTargetAdapter;
import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
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
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.ChangePropertyCommandEncoder;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.widget.MapWidget;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.CompoundValidityChecker;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
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
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ToolBarComponent;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.WrapLayout;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;

/**
 * The Map is the main component for displaying and containing {@link GamePiece}s during play. Pieces are displayed on
 * a Map and moved by clicking and dragging. Keyboard events are forwarded to selected pieces. Multiple map windows are
 * supported in a single game, with dragging between windows allowed.
 *
 * A Map may contain many different {@link Buildable} subcomponents. Components which are added directly to a Map are
 * contained in the <code>VASSAL.build.module.map</code> package
 */
public class Map extends AbstractConfigurable implements GameComponent, MouseListener, MouseMotionListener, DropTargetListener, Configurable,
    UniqueIdManager.Identifyable, ToolBarComponent, MutablePropertiesContainer, PropertySource, PlayerRoster.SideChangeListener {
  protected static boolean changeReportingEnabled = true;
  protected String mapID = ""; //$NON-NLS-1$
  protected String mapName = ""; //$NON-NLS-1$
  protected static final String MAIN_WINDOW_HEIGHT = "mainWindowHeight"; //$NON-NLS-1$
  protected static UniqueIdManager idMgr = new UniqueIdManager("Map"); //$NON-NLS-1$
  protected JPanel theMap;
  protected ArrayList<Drawable> drawComponents = new ArrayList<>();
  protected JLayeredPane layeredPane = new JLayeredPane();
  protected JScrollPane scroll;
  protected ComponentSplitter.SplitPane mainWindowDock;
  protected BoardPicker picker;
  protected JToolBar toolBar = new JToolBar();
  protected Zoomer zoom;
  protected StackMetrics metrics;
  protected Dimension edgeBuffer = new Dimension(0, 0);
  protected Color bgColor = Color.white;
  protected LaunchButton launchButton;
  protected boolean useLaunchButton = false;
  protected boolean useLaunchButtonEdit = false;
  protected String markMovedOption = GlobalOptions.ALWAYS;
  protected String markUnmovedIcon = "/images/unmoved.gif"; //$NON-NLS-1$
  protected String markUnmovedText = ""; //$NON-NLS-1$
  protected String markUnmovedTooltip = Resources.getString("Map.mark_unmoved"); //$NON-NLS-1$
  protected MouseListener multicaster = null;
  protected ArrayList<MouseListener> mouseListenerStack = new ArrayList<>();
  protected List<Board> boards = new CopyOnWriteArrayList<>();
  protected int[][] boardWidths; // Cache of board widths by row/column
  protected int[][] boardHeights; // Cache of board heights by row/column
  protected PieceCollection pieces = new DefaultPieceCollection();
  protected Highlighter highlighter = new ColoredBorder();
  protected ArrayList<Highlighter> highlighters = new ArrayList<>();
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
  protected PropertyChangeListener globalPropertyListener;
  protected String tooltip = ""; //$NON-NLS-1$
  protected MutablePropertiesContainer propsContainer = new MutablePropertiesContainer.Impl();
  protected PropertyChangeListener repaintOnPropertyChange = new PropertyChangeListener() {
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
      repaint();
    }
  };
  protected PieceMover pieceMover;
  protected KeyListener[] saveKeyListeners = null;

  public Map() {
    getView();
    theMap.addMouseListener(this);
    if (shouldDockIntoMainWindow()) {
      toolBar.setLayout(new MigLayout("ins 0,gapx 0,hidemode 3"));
    }
    else {
      toolBar.setLayout(new WrapLayout(WrapLayout.LEFT, 0, 0));
    }
    toolBar.setAlignmentX(0.0F);
    toolBar.setFloatable(false);
  }

  // Global Change Reporting control
  public static void setChangeReportingEnabled(boolean b) {
    changeReportingEnabled = b;
  }

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
  public static final String BACKGROUND_COLOR = "backgroundcolor";
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
      String s = (String) value;
      int i = s.indexOf(','); //$NON-NLS-1$
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
      launchButton.setVisible(useLaunchButton);
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
      launchButton.setAttribute(key, value);
    }
    else {
      launchButton.setAttribute(key, value);
    }
  }

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
        ? launchButton.getAttributeValueString(name) : tooltip;
    }
    else {
      return launchButton.getAttributeValueString(key);
    }
  }

  @Override
  public void build(Element e) {
    ActionListener al = new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        if (mainWindowDock == null && launchButton.isEnabled() && theMap.getTopLevelAncestor() != null) {
          theMap.getTopLevelAncestor().setVisible(!theMap.getTopLevelAncestor().isVisible());
        }
      }
    };
    launchButton = new LaunchButton(Resources.getString("Editor.Map.map"), TOOLTIP, BUTTON_NAME, HOTKEY, ICON, al);
    launchButton.setEnabled(false);
    launchButton.setVisible(false);
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
      setMapName("Main Map");
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
   * Every map must include a {@link StackMetrics} as one of its build
   * components, which governs the stacking behavior of GamePieces on the map
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
  @Override
  public JToolBar getToolBar() {
    return toolBar;
  }

  /**
   * Add a {@link Drawable} component to this map
   *
   * @see #paint
   */
  public void addDrawComponent(Drawable theComponent) {
    drawComponents.add(theComponent);
  }

  /**
   * Remove a {@link Drawable} component from this map
   *
   * @see #paint
   */
  public void removeDrawComponent(Drawable theComponent) {
    drawComponents.remove(theComponent);
  }

  /**
   * Expects to be added to a {@link GameModule}. Determines a unique id for
   * this Map. Registers itself as {@link KeyStrokeSource}. Registers itself
   * as a {@link GameComponent}. Registers itself as a drop target and drag
   * source.
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

    final DragGestureListener dgl = new DragGestureListener() {
      @Override
      public void dragGestureRecognized(DragGestureEvent dge) {
        if (dragGestureListener != null &&
            mouseListenerStack.isEmpty() &&
            SwingUtils.isDragTrigger(dge)) {
          dragGestureListener.dragGestureRecognized(dge);
        }
      }
    };

    DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(
      theMap, DnDConstants.ACTION_MOVE, dgl);
    theMap.setDropTarget(PieceMover.DragHandler.makeDropTarget(
      theMap, DnDConstants.ACTION_MOVE, this));
    g.getGameState().addGameComponent(this);
    g.getToolBar().add(launchButton);

    if (shouldDockIntoMainWindow()) {
      final IntConfigurer config =
        new IntConfigurer(MAIN_WINDOW_HEIGHT, null, -1);
      Prefs.getGlobalPrefs().addOption(null, config);

      mainWindowDock = ComponentSplitter.split(
        ComponentSplitter.splitAncestorOf(g.getControlPanel(), -1),
        layeredPane,
        ComponentSplitter.SplitPane.HIDE_BOTTOM,
        true
      );
      mainWindowDock.setResizeWeight(0.0);

      g.addKeyStrokeSource(
        new KeyStrokeSource(theMap, JComponent.WHEN_FOCUSED));
    }
    else {
      g.addKeyStrokeSource(
        new KeyStrokeSource(theMap, JComponent.WHEN_IN_FOCUSED_WINDOW));
    }
    // Fix for bug 1630993: toolbar buttons not appearing
    toolBar.addHierarchyListener(new HierarchyListener() {
      @Override
      public void hierarchyChanged(HierarchyEvent e) {
        Window w;
        if ((w = SwingUtilities.getWindowAncestor(toolBar)) != null) {
          w.validate();
        }
        if (toolBar.getSize().width > 0) {
          toolBar.removeHierarchyListener(this);
        }
      }
    });

    GameModule.getGameModule().addSideChangeListenerToPlayerRoster(this);
    g.getPrefs().addOption(
      Resources.getString("Prefs.general_tab"), //$NON-NLS-1$
      new IntConfigurer(
        PREFERRED_EDGE_DELAY,
        Resources.getString("Map.scroll_delay_preference"), //$NON-NLS-1$
        PREFERRED_EDGE_SCROLL_DELAY
      )
    );

    g.getPrefs().addOption(
      Resources.getString("Prefs.general_tab"), //$NON-NLS-1$
      new BooleanConfigurer(
        MOVING_STACKS_PICKUP_UNITS,
        Resources.getString("Map.moving_stacks_preference"), //$NON-NLS-1$
        Boolean.FALSE
      )
    );
  }

  public void setPieceMover(PieceMover mover) {
    pieceMover = mover;
  }

  @Override
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

  @Override
  public void sideChanged(String oldSide, String newSide) {
    repaint();
  }

  /**
   * Set the boards for this map. Each map may contain more than one
   * {@link Board}.
   */
  public synchronized void setBoards(Collection<Board> c) {
    boards.clear();
    for (Board b : c) {
      b.setMap(this);
      boards.add(b);
    }
    setBoardBoundaries();
  }

  /**
   * Set the boards for this map. Each map may contain more than one
   * {@link Board}.
   * @deprecated Use {@link #setBoards(Collection<Board>)} instead.
   */
  @Deprecated
  public synchronized void setBoards(Enumeration<Board> boardList) {
    setBoards(Collections.list(boardList));
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * @return the {@link Board} on this map containing the argument point
   */
  public Board findBoard(Point p) {
    for (Board b : boards) {
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
      if (grid instanceof ZonedGrid) {
        Rectangle r = b.bounds();
        p.translate(-r.x, -r.y);  // Translate to Board co-ords
        return ((ZonedGrid) grid).findZone(p);
      }
    }
    return null;
  }

  /**
   * Search on all boards for a Zone with the given name
   * @param Zone name
   * @return Located zone
   */
  public Zone findZone(String name) {
    for (Board b : boards) {
      for (ZonedGrid zg : b.getAllDescendantComponentsOf(ZonedGrid.class)) {
        Zone z = zg.findZone(name);
        if (z != null) {
          return z;
        }
      }
    }
    return null;
  }

  /**
   * Search on all boards for a Region with the given name
   * @param Region name
   * @return Located region
   */
  public Region findRegion(String name) {
    for (Board b : boards) {
      for (RegionGrid rg : b.getAllDescendantComponentsOf(RegionGrid.class)) {
        Region r = rg.findRegion(name);
        if (r != null) {
          return r;
        }
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
    if (name != null) {
      for (Board b : boards) {
        if (name.equals(b.getName())) {
          return b;
        }
      }
    }
    return null;
  }

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
    final Rectangle r = new Rectangle(0,0);
    for (Board b : boards) r.add(b.bounds());
    r.width += edgeBuffer.width;
    r.height += edgeBuffer.height;
    return r.getSize();
  }

  /**
   * @return true if the given point may not be a legal location. I.e., if this grid will attempt to snap it to the
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

    final Board b = findBoard(p);
    if (b == null) return snap;

    final Rectangle r = b.bounds();
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
   * The buffer of empty space around the boards in the Map window,
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
   */
  @Deprecated
  public Point mapCoordinates(Point p) {
    return componentToMap(p);
  }

  @Deprecated
  public Rectangle mapRectangle(Rectangle r) {
    return componentToMap(r);
  }

  /**
   * Translate a point from map coordinates to component coordinates
   *
   * @see #mapCoordinates
   */
  @Deprecated
  public Point componentCoordinates(Point p) {
    return mapToComponent(p);
  }

  @Deprecated
  public Rectangle componentRectangle(Rectangle r) {
    return mapToComponent(r);
  }

  protected int scale(int c, double zoom) {
    return (int)(c * zoom);
  }

  protected Point scale(Point p, double zoom) {
    return new Point((int)(p.x * zoom), (int)(p.y * zoom));
  }

  protected Rectangle scale(Rectangle r, double zoom) {
    return new Rectangle(
      (int)(r.x * zoom),
      (int)(r.y * zoom),
      (int)(r.width * zoom),
      (int)(r.height * zoom)
    );
  }

  public int mapToDrawing(int c, double os_scale) {
    return scale(c, getZoom() * os_scale);
  }

  public Point mapToDrawing(Point p, double os_scale) {
    return scale(p, getZoom() * os_scale);
  }

  public Rectangle mapToDrawing(Rectangle r, double os_scale) {
    return scale(r, getZoom() * os_scale);
  }

  public int mapToComponent(int c) {
    return scale(c, getZoom());
  }

  public Point mapToComponent(Point p) {
    return scale(p, getZoom());
  }

  public Rectangle mapToComponent(Rectangle r) {
    return scale(r, getZoom());
  }

  public int componentToDrawing(int c, double os_scale) {
    return scale(c, os_scale);
  }

  public Point componentToDrawing(Point p, double os_scale) {
    return scale(p, os_scale);
  }

  public Rectangle componentToDrawing(Rectangle r, double os_scale) {
    return scale(r, os_scale);
  }

  public int componentToMap(int c) {
    return scale(c, 1.0/getZoom());
  }

  public Point componentToMap(Point p) {
    return scale(p, 1.0/getZoom());
  }

  public Rectangle componentToMap(Rectangle r) {
    return scale(r, 1.0/getZoom());
  }

  public int drawingToMap(int c, double os_scale) {
    return scale(c, 1.0/(getZoom() * os_scale));
  }

  public Point drawingToMap(Point p, double os_scale) {
    return scale(p, 1.0/(getZoom() * os_scale));
  }

  public Rectangle drawingToMap(Rectangle r, double os_scale) {
    return scale(r, 1.0/(getZoom() * os_scale));
  }

  public int drawingToComponent(int c, double os_scale) {
    return scale(c, 1.0/os_scale);
  }

  public Point drawingToComponent(Point p, double os_scale) {
    return scale(p, 1.0/os_scale);
  }

  public Rectangle drawingToComponent(Rectangle r, double os_scale) {
    return scale(r, 1.0/os_scale);
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
      loc = Resources.getString("Map.offboard"); //$NON-NLS-1$
    }
    return loc;
  }

  public String localizedLocationName(Point p) {
    String loc = getLocalizedDeckNameAt(p);
    if (loc == null) {
      Board b = findBoard(p);
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
   * @return a String name for the given location on the map. Include Map name if requested. Report deck name instead of
   *         location if point is inside the bounds of a deck. Do not include location if this map is not visible to all
   *         players.
   */
//  public String getFullLocationName(Point p, boolean includeMap) {
//    String loc = ""; //$NON-NLS-1$
//    if (includeMap && getMapName() != null && getMapName().length() > 0) {
//      loc = "[" + getMapName() + "]"; //$NON-NLS-1$ //$NON-NLS-2$
//    }
//    if (isVisibleToAll() && p != null) {
//      String pos = getDeckNameContaining(p);
//      if (pos == null) {
//        if (locationName(p) != null) {
//          loc = locationName(p) + loc;
//        }
//      }
//      else {
//        loc = pos;
//      }
//    }
//    return loc;
//  }

  /**
   * Is this map visible to all players
   */
  public boolean isVisibleToAll() {
    if (this instanceof PrivateMap) {
      if (!getAttributeValueString(PrivateMap.VISIBLE).equals("true")) { //$NON-NLS-1$
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
      for (DrawPile d : getComponentsOf(DrawPile.class)) {
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
      for (DrawPile d : getComponentsOf(DrawPile.class)) {
        if (d.getPosition().equals(p)) {
          deck = d.getConfigureName();
          break;
        }
      }
    }
    return deck;
  }

  public String getLocalizedDeckNameAt(Point p) {
    String deck = null;
    if (p != null) {
      for (DrawPile d : getComponentsOf(DrawPile.class)) {
        if (d.getPosition().equals(p)) {
          deck = d.getLocalizedConfigureName();
          break;
        }
      }
    }
    return deck;
  }

  /**
   * Because MouseEvents are received in component coordinates, it is
   * inconvenient for MouseListeners on the map to have to translate to map
   * coordinates. MouseListeners added with this method will receive mouse
   * events with points already translated into map coordinates.
   * addLocalMouseListenerFirst inserts the new listener at the start of the
   * chain.
   */
  public void addLocalMouseListener(MouseListener l) {
    multicaster = AWTEventMulticaster.add(multicaster, l);
  }

  public void addLocalMouseListenerFirst(MouseListener l) {
    multicaster = AWTEventMulticaster.add(l, multicaster);
  }

  public void removeLocalMouseListener(MouseListener l) {
    multicaster = AWTEventMulticaster.remove(multicaster, l);
  }

  /**
   * MouseListeners on a map may be pushed and popped onto a stack.
   * Only the top listener on the stack receives mouse events.
   */
  public void pushMouseListener(MouseListener l) {
    mouseListenerStack.add(l);
  }

  /**
   * MouseListeners on a map may be pushed and popped onto a stack. Only the top listener on the stack receives mouse
   * events
   */
  public void popMouseListener() {
    mouseListenerStack.remove(mouseListenerStack.size()-1);
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }
  
  
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
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
   */
  @Override
  public void mouseClicked(MouseEvent e) {
    if (!mouseListenerStack.isEmpty()) {
      mouseListenerStack.get(mouseListenerStack.size()-1).mouseClicked(translateEvent(e));
    }
    else if (multicaster != null) {
      multicaster.mouseClicked(translateEvent(e));
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

  public static Map activeMap = null;
  public static void clearActiveMap() {
    if (activeMap != null) {
      activeMap.repaint();
      activeMap = null;
    }
  }

  @Override
  public void mousePressed(MouseEvent e) {
    // Deselect any counters on the last Map with focus
    if (!this.equals(activeMap)) {
      boolean dirty = false;

      final KeyBuffer kbuf = KeyBuffer.getBuffer();
      final ArrayList<GamePiece> l = new ArrayList<>(kbuf.asList());

      for (GamePiece p : l) {
        if (p.getMap() == activeMap) {
          kbuf.remove(p);
          dirty = true;
        }
      }

      if (dirty && activeMap != null) {
        activeMap.repaint();
      }
    }
    activeMap = this;

    if (!mouseListenerStack.isEmpty()) {
      mouseListenerStack.get(mouseListenerStack.size()-1).mousePressed(translateEvent(e));
    }
    else if (multicaster != null) {
      multicaster.mousePressed(translateEvent(e));
    }
  }

  /**
   * Mouse events are first translated into map coordinates.
   * Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners.
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener
   */
  @Override
  public void mouseReleased(MouseEvent e) {
    // don't write over Java's mouse event
    Point p = e.getPoint();
    p.translate(theMap.getX(), theMap.getY());
    if (theMap.getBounds().contains(p)) {
      if (!mouseListenerStack.isEmpty()) {
        mouseListenerStack.get(mouseListenerStack.size()-1).mouseReleased(translateEvent(e));
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

    for (KeyListener kl : saveKeyListeners) {
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
    for (KeyListener kl : saveKeyListeners) {
      theMap.removeKeyListener(kl);
    }
  }

  /**
   * This listener will be notified when a drag event is initiated, assuming
   * that no MouseListeners are on the stack.
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

  @Override
  public void dragEnter(DropTargetDragEvent dtde) {
  }

  @Override
  public void dragOver(DropTargetDragEvent dtde) {
    scrollAtEdge(dtde.getLocation(), SCROLL_ZONE);
  }

  @Override
  public void dropActionChanged(DropTargetDragEvent dtde) {
  }

  /*
   * Cancel final scroll and repaint map
   */
  @Override
  public void dragExit(DropTargetEvent dte) {
    if (scroller.isRunning()) scroller.stop();
    repaint();
  }

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
   */
  @Override
  public void mouseMoved(MouseEvent e) {
  }

  /**
   * Mouse motion events are not forwarded to LocalMouseListeners or to
   * listeners on the stack.
   *
   * The map scrolls when dragging the mouse near the edge.
   */
  @Override
  public void mouseDragged(MouseEvent e) {
    if (!SwingUtils.isRightMouseButton(e)) {
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
  public static final int SCROLL_ZONE = 30;

  /** The horizontal component of the autoscrolling vector, -1, 0, or 1. */
  protected int sx;
  /** The vertical component of the autoscrolling vector, -1, 0, or 1. */
  protected int sy;

  protected int dx, dy;

  /**
   * Begin autoscrolling the map if the given point is within the given
   * distance from a viewport edge.
   *
   * @param evtPt
   * @param dist
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

      @Override
      public void timingEvent(float fraction) {
        // Constant velocity along each axis, 0.5px/ms
        final long t1 = System.currentTimeMillis();
        final int dt = (int)((t1 - t0)/2);
        t0 = t1;

        scroll(sx*dt, sy*dt);

        // Check whether we have hit an edge
        final Rectangle vrect = scroll.getViewport().getViewRect();

        if ((sx == -1 && vrect.x == 0) ||
            (sx ==  1 && vrect.x + vrect.width >= theMap.getWidth())) sx = 0;

        if ((sy == -1 && vrect.y == 0) ||
            (sy ==  1 && vrect.y + vrect.height >= theMap.getHeight())) sy = 0;

        // Stop if the scroll vector is zero
        if (sx == 0 && sy == 0) scroller.stop();
      }

      @Override
      public void begin() {
        t0 = System.currentTimeMillis();
      }
    }
  );

  public void repaint(boolean cf) {
    clearFirst = cf;
    theMap.repaint();
  }

  public void paintRegion(Graphics g, Rectangle visibleRect) {
    paintRegion(g, visibleRect, theMap);
  }

  public void paintRegion(Graphics g, Rectangle visibleRect, Component c) {
    clearMapBorder(g); // To avoid ghost pieces around the edge
    drawBoardsInRegion(g, visibleRect, c);
    drawDrawable(g, false);
    drawPiecesInRegion(g, visibleRect, c);
    drawDrawable(g, true);
  }

  public void drawBoardsInRegion(Graphics g,
                                 Rectangle visibleRect,
                                 Component c) {
    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final double dzoom = getZoom() * os_scale;
    for (Board b : boards) {
      b.drawRegion(g, getLocation(b, dzoom), visibleRect, dzoom, c);
    }
  }

  public void drawBoardsInRegion(Graphics g, Rectangle visibleRect) {
    drawBoardsInRegion(g, visibleRect, theMap);
  }

  public void repaint() {
    theMap.repaint();
  }

  public void drawPiecesInRegion(Graphics g,
                                 Rectangle visibleRect,
                                 Component c) {
    if (hideCounters) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final double dzoom = getZoom() * os_scale;

    Composite oldComposite = g2d.getComposite();
    g2d.setComposite(
      AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));
    final GamePiece[] stack = pieces.getPieces();
    for (GamePiece gamePiece : stack) {
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

  public void drawPiecesInRegion(Graphics g, Rectangle visibleRect) {
    drawPiecesInRegion(g, visibleRect, theMap);
  }

  public void drawPieces(Graphics g, int xOffset, int yOffset) {
    if (hideCounters) {
      return;
    }

    Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    Composite oldComposite = g2d.getComposite();
    g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));
    GamePiece[] stack = pieces.getPieces();
    for (GamePiece gamePiece : stack) {
      Point pt = mapToDrawing(gamePiece.getPosition(), os_scale);
      gamePiece.draw(g, pt.x + xOffset, pt.y + yOffset, theMap, getZoom());
      if (Boolean.TRUE.equals(gamePiece.getProperty(Properties.SELECTED))) {
        highlighter.draw(gamePiece, g, pt.x - xOffset, pt.y - yOffset, theMap, getZoom());
      }
    }
    g2d.setComposite(oldComposite);
  }

  public void drawDrawable(Graphics g, boolean aboveCounters) {
    for (Drawable drawable : drawComponents) {
      if (!(aboveCounters ^ drawable.drawAboveCounters())) {
        drawable.draw(g, this);
      }
    }
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
  @Deprecated
  public Enumeration<Board> getAllBoards() {
    return Collections.enumeration(boards);
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
      final Point pos = p.getPosition();
      r.translate(pos.x, pos.y);

      if (Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
        r.add(highlighter.boundingBox(p));
        for (Iterator<Highlighter> i = getHighlighters(); i.hasNext();) {
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
   * Returns the selection bounding box of a GamePiece accounting for the offset of a piece within a stack
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
   * @return an array of all GamePieces on the map. This is a read-only copy.
   * Altering the array does not alter the pieces on the map.
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
    for (Board b : boards) {
      Point relPos = b.relativePosition();
      maxX = Math.max(maxX, relPos.x);
      maxY = Math.max(maxY, relPos.y);
    }
    boardWidths = new int[maxX + 1][maxY + 1];
    boardHeights = new int[maxX + 1][maxY + 1];
    for (Board b : boards) {
      Point relPos = b.relativePosition();
      boardWidths[relPos.x][relPos.y] = b.bounds().width;
      boardHeights[relPos.x][relPos.y] = b.bounds().height;
    }
    Point offset = new Point(edgeBuffer.width, edgeBuffer.height);
    for (Board b : boards) {
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
   * Draw the boards of the map at the given point and zoom factor onto
   * the given Graphics object
   */
  public void drawBoards(Graphics g, int xoffset, int yoffset, double zoom, Component obs) {
    for (Board b : boards) {
      Point p = getLocation(b, zoom);
      p.translate(xoffset, yoffset);
      b.draw(g, p.x, p.y, zoom, obs);
    }
  }

  /**
   * Repaint the given area, specified in map coordinates
   */
  public void repaint(Rectangle r) {
    r.setLocation(mapToComponent(new Point(r.x, r.y)));
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

  @Override
  public Object getProperty(Object key) {
    Object value = null;
    MutableProperty p = propsContainer.getMutableProperty(String.valueOf(key));
    if (p != null) {
      value = p.getPropertyValue();
    }
    else {
      value = GameModule.getGameModule().getProperty(key);
    }
    return value;
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    Object value = null;
    MutableProperty p = propsContainer.getMutableProperty(String.valueOf(key));
    if (p != null) {
      value = p.getPropertyValue();
    }
    if (value == null) {
      value = GameModule.getGameModule().getLocalizedProperty(key);
    }
    return value;
  }

  /**
   * Return the auto-move key. It may be named, so just return
   * the allocated KeyStroke.
   * @return auto move keystroke
   */
  public KeyStroke getMoveKey() {
    return moveKey == null ? null : moveKey.getKeyStroke();
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
      d.setJMenuBar(MenuManager.getInstance().getMenuBarFor(d));
      return d;
    }
  }

  public boolean shouldDockIntoMainWindow() {
    // set to show via a button, or no combined window at all, don't dock
    if (useLaunchButton || !GlobalOptions.getInstance().isUseSingleWindow()) {
      return false;
    }

    // otherwise dock if this map is the first not to show via a button
    for (Map m : GameModule.getGameModule().getComponentsOf(Map.class)) {
      if (m == this) {
        return true;
      }
      else if (m.shouldDockIntoMainWindow()) {
        return false;
      }
    }
    // should be impossible
    return true;
  }

  /**
   * When a game is started, create a top-level window, if none exists.
   * When a game is ended, remove all boards from the map.
   *
   * @see GameComponent
   */
  @Override
  public void setup(boolean show) {
    if (show) {
      final GameModule g = GameModule.getGameModule();

      if (shouldDockIntoMainWindow()) {
        mainWindowDock.showComponent();
        final int height = (Integer)
          Prefs.getGlobalPrefs().getValue(MAIN_WINDOW_HEIGHT);
        if (height > 0) {
          final Container top = mainWindowDock.getTopLevelAncestor();
          top.setSize(top.getWidth(), height);
        }
        if (toolBar.getParent() == null) {
          g.getToolBar().addSeparator();
          g.getToolBar().add(toolBar);
        }
        toolBar.setVisible(true);
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
    }
    else {
      pieces.clear();
      boards.clear();
      if (mainWindowDock != null) {
        if (mainWindowDock.getHideableComponent().isShowing()) {
          Prefs.getGlobalPrefs().getOption(MAIN_WINDOW_HEIGHT)
               .setValue(mainWindowDock.getTopLevelAncestor().getHeight());
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
    return getLocalizedMapName().length() > 0 ? getLocalizedMapName() : Resources.getString("Map.window_title", GameModule.getGameModule().getLocalizedGameName()); //$NON-NLS-1$
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
  @Deprecated public void reposition(GamePiece s, int pos) {
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
   * Center the map at the given map coordinates, if the point is not
   * already within (dx,dy) of the center.
   */
  public void centerAt(Point p, int dx, int dy) {
    if (scroll != null) {
      p = mapToComponent(p);

      final Rectangle r = theMap.getVisibleRect();
      r.x = p.x - r.width/2;
      r.y = p.y - r.height/2;

      final Dimension d = getPreferredSize();
      if (r.x + r.width > d.width) r.x = d.width - r.width;
      if (r.y + r.height > d.height) r.y = d.height - r.height;

      r.width = dx > r.width ? 0 : r.width - dx;
      r.height = dy > r.height ? 0 : r.height - dy;

      theMap.scrollRectToVisible(r);
    }
  }

  /** Ensure that the given region (in map coordinates) is visible */
  public void ensureVisible(Rectangle r) {
    if (scroll != null) {
      final Point p = mapToComponent(r.getLocation());
      r = new Rectangle(p.x, p.y,
            (int) (getZoom() * r.width), (int) (getZoom() * r.height));
      theMap.scrollRectToVisible(r);
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

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Map.component_type"); //$NON-NLS-1$
  }

  public String getMapName() {
    return getConfigureName();
  }

  public String getLocalizedMapName() {
    return getLocalizedConfigureName();
  }

  public void setMapName(String s) {
    mapName = s;
    setConfigureName(mapName);
    if (tooltip == null || tooltip.length() == 0) {
      launchButton.setToolTipText(s != null ? Resources.getString("Map.show_hide", s) : Resources.getString("Map.show_hide", Resources.getString("Map.map"))); //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm"); //$NON-NLS-1$
  }

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
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/map.gif"); //$NON-NLS-1$
    }
  }
  public static class UnmovedIconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/unmoved.gif"); //$NON-NLS-1$
    }
  }
  public static class MoveWithinFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, LOCATION, MAP_NAME, OLD_LOCATION });
    }
  }
  public static class MoveToFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, LOCATION, OLD_MAP, MAP_NAME, OLD_LOCATION });
    }
  }
  public static class CreateFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { PIECE_NAME, MAP_NAME, LOCATION });
    }
  }
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

  public String getCreateFormat() {
    if (createFormat != null) {
      return createFormat;
    }
    else {
      String val = "$" + PIECE_NAME + "$ created in $" + LOCATION + "$"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      if (!boards.isEmpty()) {
        Board b = boards.get(0);
        if (b.getGrid() == null || b.getGrid().getGridNumbering() == null) {
          val = ""; //$NON-NLS-1$
        }
      }
      return val;
    }
  }

  public String getChangeFormat() {
    return isChangeReportingEnabled() ? changeFormat : "";
  }

  public String getMoveToFormat() {
    if (moveToFormat != null) {
      return moveToFormat;
    }
    else {
      String val = "$" + PIECE_NAME + "$" + " moves $" + OLD_LOCATION + "$ -> $" + LOCATION + "$ *"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      if (!boards.isEmpty()) {
        Board b = boards.get(0);
        if (b.getGrid() == null || b.getGrid().getGridNumbering() != null) {
          val = ""; //$NON-NLS-1$
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
      String val = "$" + PIECE_NAME + "$" + " moves $" + OLD_LOCATION + "$ -> $" + LOCATION + "$ *"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      if (!boards.isEmpty()) {
        Board b = boards.get(0);
        if (b.getGrid() == null) {
          val = ""; //$NON-NLS-1$
        }
      }
      return val;
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    Class<?>[] c = { GlobalMap.class, LOS_Thread.class, ToolbarMenu.class, MultiActionButton.class, HidePiecesButton.class, Zoomer.class,
      CounterDetailViewer.class, HighlightLastMoved.class, LayeredPieceCollection.class, ImageSaver.class, TextSaver.class, DrawPile.class, SetupStack.class,
      MassKeyCommand.class, MapShader.class, PieceRecenterer.class };
    return c;
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (visibilityCondition == null) {
      visibilityCondition = new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return useLaunchButton;
        }
      };
    }
    if (List.of(HOTKEY, BUTTON_NAME, TOOLTIP, ICON).contains(name)) {
      return visibilityCondition;
    }
    else if (List.of(MARK_UNMOVED_TEXT, MARK_UNMOVED_ICON, MARK_UNMOVED_TOOLTIP).contains(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return !GlobalOptions.NEVER.equals(markMovedOption);
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  /**
   * Each Map must have a unique String id
   */
  @Override
  public void setId(String id) {
    mapID = id;
  }

  public static Map getMapById(String id) {
    return (Map) idMgr.findInstance(id);
  }

  /**
   * Utility method to return a {@link List} of all map components in the
   * module.
   *
   * @return the list of <code>Map</code>s
   */
  public static List<Map> getMapList() {
    final GameModule g = GameModule.getGameModule();

    final List<Map> l = g.getComponentsOf(Map.class);
    for (ChartWindow cw : g.getComponentsOf(ChartWindow.class)) {
      for (MapWidget mw : cw.getAllDescendantComponentsOf(MapWidget.class)) {
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
  @Deprecated
  public static Iterator<Map> getAllMaps() {
    return getMapList().iterator();
  }

  /**
   * Find a contained Global Variable by name
   */
  @Override
  public MutableProperty getMutableProperty(String name) {
    return propsContainer.getMutableProperty(name);
  }

  @Override
  public void addMutableProperty(String key, MutableProperty p) {
    propsContainer.addMutableProperty(key, p);
    p.addMutablePropertyChangeListener(repaintOnPropertyChange);
  }

  @Override
  public MutableProperty removeMutableProperty(String key) {
    MutableProperty p = propsContainer.removeMutableProperty(key);
    if (p != null) {
      p.removeMutablePropertyChangeListener(repaintOnPropertyChange);
    }
    return p;
  }

  @Override
  public String getMutablePropertiesContainerId() {
    return getMapName();
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
   * Make a best gues for a unique identifier for the target. Use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getConfigureName} if non-null, otherwise use
   * {@link VASSAL.tools.UniqueIdManager.Identifyable#getId}
   *
   * @return
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

    public InsetLayout(Container target, JScrollPane base) {
      super(target);
      this.base = base;
    }

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
        Component c = target.getComponent(i);
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
   * Implements default logic for merging pieces at a given location within
   * a map Returns a {@link Command} that merges the input {@link GamePiece}
   * with an existing piece at the input position, provided the pieces are
   * stackable, visible, in the same layer, etc.
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

    @Override
    public Object visitDeck(Deck d) {
      if (d.getPosition().equals(pt)) {
        return map.getStackMetrics().merge(d, p);
      }
      else {
        return null;
      }
    }

    @Override
    public Object visitStack(Stack s) {
      if (s.getPosition().equals(pt) && map.getStackMetrics().isStackingEnabled() && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
          && s.topPiece() != null && map.getPieceCollection().canMerge(s, p)) {
        return map.getStackMetrics().merge(s, p);
      }
      else {
        return null;
      }
    }

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
   * The component that represents the map itself
   */
  public static class View extends JPanel {
    private static final long serialVersionUID = 1L;

    protected Map map;

    public View(Map m) {
      setFocusTraversalKeysEnabled(false);
      map = m;
    }

    @Override
    public void paint(Graphics g) {
      // Don't draw the map until the game is updated.
      if (GameModule.getGameModule().getGameState().isUpdating()) {
        return;
      }

      final Graphics2D g2d = (Graphics2D) g;

      g2d.addRenderingHints(SwingUtils.FONT_HINTS);

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

    @Override
    public void update(Graphics g) {
      // To avoid flicker, don't clear the display first
      paint(g);
    }

    @Override
    public Dimension getPreferredSize() {
      return map.getPreferredSize();
    }

    public Map getMap() {
      return map;
    }
  }
}
