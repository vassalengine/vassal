/*
 *
 * Copyright (c) 2005-2006 by Rodney Kinney, Brent Easton,
 * Torsten Spindler, and Scot McConnachie
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MenuDisplayer;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.GamePieceFormattedStringConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.counters.PropertyExporter;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.swing.SwingUtils;

import org.apache.commons.lang3.ArrayUtils;

public class Inventory extends AbstractToolbarItem
                       implements GameComponent,
                                  PlayerRoster.SideChangeListener {

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  public static final String REFRESH_HOTKEY = "refreshHotkey"; //NON-NLS
  protected NamedKeyStrokeListener refreshListener;

  protected CounterInventory results;
  protected JTree tree;

  public static final String VERSION = "2.1"; //$NON-NLS-1$

  // public static final String DEST = "destination";

  /*
   * For use in formatted text output.
   */
  protected String mapSeparator = "\n"; //$NON-NLS-1$
  protected String groupSeparator = "   "; //$NON-NLS-1$

  /*
   * Options Destination - Chat, Dialog, File.
   */

  public static final String FILTER = "include"; //$NON-NLS-1$
  protected PropertyExpression piecePropertiesFilter = new PropertyExpression(); //$NON-NLS-1$

  public static final String GROUP_BY = "groupBy"; //$NON-NLS-1$
  protected String[] groupBy = {""}; //$NON-NLS-1$

  public static final String NON_LEAF_FORMAT = "nonLeafFormat"; //$NON-NLS-1$
  protected String nonLeafFormat = "$PropertyValue$"; //$NON-NLS-1$

  public static final String CENTERONPIECE = "centerOnPiece"; //$NON-NLS-1$
  protected boolean centerOnPiece = true;

  public static final String FORWARD_KEYSTROKE = "forwardKeystroke"; //$NON-NLS-1$
  protected boolean forwardKeystroke = true;

  public static final String SHOW_MENU = "showMenu"; //$NON-NLS-1$
  protected boolean showMenu = true;

  public static final String SIDES = "sides"; //$NON-NLS-1$
  protected String[] sides = null;

  public static final String KEYSTROKE = "keystroke"; //$NON-NLS-1$
  protected KeyStroke keyStroke = null;

  public static final String CUTBELOWROOT = "cutRoot"; //$NON-NLS-1$
  protected int cutBelowRoot = 0;

  public static final String CUTABOVELEAVES = "cutLeaves"; //$NON-NLS-1$
  protected int cutAboveLeaves = 0;

  public static final String LEAF_FORMAT = "leafFormat"; //$NON-NLS-1$
  protected String pieceFormat = "$PieceName$"; //$NON-NLS-1$

  public static final String PIECE_ZOOM = "pieceZoom"; //$NON-NLS-1$
  protected double pieceZoom = .25;

  public static final String DRAW_PIECES = "drawPieces"; //$NON-NLS-1$
  protected boolean drawPieces = true;

  public static final String FOLDERS_ONLY = "foldersOnly"; //$NON-NLS-1$
  protected boolean foldersOnly = false;

  public static final String SORT_PIECES = "sortPieces"; //$NON-NLS-1$
  protected boolean sortPieces = true;

  public static final String SORT_FORMAT = "sortFormat"; //$NON-NLS-1$
  protected String sortFormat = "$PieceName$"; //$NON-NLS-1$

  public static final String ALPHA = "alpha"; //$NON-NLS-1$
  public static final String LENGTHALPHA = "length"; //$NON-NLS-1$
  public static final String NUMERIC = "numeric"; //$NON-NLS-1$
  public static final String[] SORT_OPTIONS = { ALPHA, LENGTHALPHA, NUMERIC };

  public static final String LAUNCH_FUNCTION  = "launchFunction"; //NON-NLS
  public static final String FUNCTION_REFRESH = "functionRefresh"; //NON-NLS
  public static final String FUNCTION_HIDE    = "functionHide"; //NON-NLS
  public static final String[] FUNCTION_OPTIONS = { FUNCTION_REFRESH, FUNCTION_HIDE };
  public static final String[] FUNCTION_KEYS    = { "Editor.Inventory.function_refresh", //NON-NLS
                                                    "Editor.Inventory.function_hide"}; //NON-NLS
  protected String launchFunction = FUNCTION_HIDE; //BR// This default is "more like how most toolbar buttons work"

  protected String sortStrategy = ALPHA;

  public static final String SORTING = "sorting"; //$NON-NLS-1$

  public static final String BUTTON_FUNCTION = "buttonFunction"; //NON-NLS

  protected JDialog frame;

  // These five identical to AbstractToolbarItem, and are only here for "clirr purposes"
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String NAME = "name"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$


  public Inventory() {
    setLaunchButton(makeLaunchButton(
      Resources.getString("Inventory.show_inventory"),
      Resources.getString("Inventory.inventory"),
      "/images/inventory.gif", //NON-NLS
      e -> launch()
    ));
    launch = getLaunchButton(); // for compatibility

    refreshListener = new NamedKeyStrokeListener(e -> refresh());
    GameModule.getGameModule().addKeyStrokeListener(refreshListener);

    getLaunchButton().setEnabled(false);
    getLaunchButton().setVisible(false);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Inventory.component_type"); //$NON-NLS-1$
  }

  @Override
  public void addTo(Buildable b) {
    super.addTo(b);
    // Support for players changing sides
    GameModule.getGameModule().addSideChangeListenerToPlayerRoster(this);
    getLaunchButton().setAlignmentY(0.0F);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    frame = new JDialog(GameModule.getGameModule().getPlayerWindow());
    frame.setTitle(getConfigureName());
    final String key = "Inventory." + getConfigureName(); //$NON-NLS-1$
    GameModule.getGameModule().getPrefs().addOption(new PositionOption(key, frame));
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
    frame.add(initTree());
    frame.add(initButtons());
    frame.setSize(250, 350);
  }

  /**
   * Construct an explorer like interface for the selected counters
   */
  protected Component initTree() {
    // Initialize the tree to be displayed from the results tree
    tree = new JTree();
    tree.setRootVisible(false);
    tree.setShowsRootHandles(true);
    tree.setRowHeight(0);
    tree.setCellRenderer(initTreeCellRenderer());
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    // If wanted center on a selected counter
    tree.addTreeSelectionListener(e -> {
      if (centerOnPiece) {
        final GamePiece piece = getSelectedCounter();
        if (piece != null && piece.getMap() != null)
          piece.getMap().centerAt(piece.getPosition());
      }
    });
    tree.addMouseListener(new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent e) {
        maybePopup(e);
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        maybePopup(e);
      }

      private void maybePopup(MouseEvent e) {
        if (showMenu && e.isPopupTrigger()) {
          final TreePath path = tree.getPathForLocation(e.getX(), e.getY());
          if (path != null) {
            if (path.getLastPathComponent() instanceof CounterNode) {
              final CounterNode node = (CounterNode) path.getLastPathComponent();
              final GamePiece piece = node.getCounter().getPiece();
              if (piece != null) {
                final JPopupMenu menu = MenuDisplayer.createPopup(piece);
                //$NON-NLS-1$
                menu.addPropertyChangeListener("visible", evt -> { //NON-NLS
                  if (Boolean.FALSE.equals(evt.getNewValue())) {
                    SwingUtilities.invokeLater(() -> refresh());
                  }
                });
                menu.show(tree, e.getX(), e.getY());
              }
            }
          }
        }
      }
    });
    tree.addKeyListener(new HotKeySender());

    final JScrollPane scrollPane = new ScrollPane(tree,
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    refresh();
    return scrollPane;
  }

  protected TreeCellRenderer initTreeCellRenderer() {
    return new DefaultTreeCellRenderer() {

      private static final long serialVersionUID = -250332615261355856L;

      @Override
      public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf && !foldersOnly, row, hasFocus);
        if (value instanceof CounterNode) {
          final GamePiece piece = ((CounterNode) value).getCounter().getPiece();
          if (piece != null) {
            final Rectangle r = piece.getShape().getBounds();
            r.x = (int) Math.round(r.x * pieceZoom);
            r.y = (int) Math.round(r.y * pieceZoom);
            r.width = (int) Math.round(r.width * pieceZoom);
            r.height = (int) Math.round(r.height * pieceZoom);
            setIcon(drawPieces ? new Icon() {

              @Override
              public int getIconHeight() {
                return r.height;
              }

              @Override
              public int getIconWidth() {
                return r.width;
              }

              @Override
              public void paintIcon(Component c, Graphics g, int x, int y) {
                final Graphics2D g2d = (Graphics2D) g;
                final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
                final AffineTransform orig_t = g2d.getTransform();
                g2d.setTransform(SwingUtils.descaleTransform(orig_t));

                piece.draw(
                  g,
                  (int)(-r.x * os_scale),
                  (int)(-r.y * os_scale),
                  c,
                  pieceZoom * os_scale
                );

                g2d.setTransform(orig_t);
              }

            } : null);
          }
        }
        return this;
      }
    };
  }

  protected Component initButtons() {
    final Box buttonBox = Box.createHorizontalBox();
    // Written by Scot McConnachie.
    final JButton writeButton = new JButton(Resources.getString(Resources.SAVE));
    writeButton.addActionListener(e -> inventoryToText());
    buttonBox.add(writeButton);
    final JButton refreshButton = new JButton(Resources.getString(Resources.REFRESH));
    refreshButton.addActionListener(e -> refresh());
    buttonBox.add(refreshButton);
    final JButton closeButton = new JButton(Resources.getString(Resources.CLOSE));
    closeButton.addActionListener(e -> frame.setVisible(false));
    buttonBox.add(closeButton);
    return buttonBox;
  }

  /**
   * @author Scot McConnachie.
   * Writes the inventory text data to a user selected file.
   * This allows a module designer to use Inventory to create customized text
   * reports from the game.
   * @author spindler
   * Changed FileChooser to use the new Vassal.tool.FileChooser
   * Changed Separator before getResultString call
   * TODO add check for existing file
   * TODO rework text display of Inventory
   */
  protected void inventoryToText() {
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showSaveDialog() == FileChooser.CANCEL_OPTION) return;

    final StringBuilder output = new StringBuilder();
    final File file = fc.getSelectedFile();

    // TODO replace this hack
    mapSeparator = System.getProperty("line.separator"); //$NON-NLS-1$
    // groupSeparator = mapSeparator + "  ";
    // groupSeparator = " ";
    output.append(results.getResultString());
    // .substring(1).replaceAll(
  //      mapSeparator, System.getProperty("line.separator"));

    // Writing out a text file for the user to do whatever with. Use the native encoding.
    try (Writer bw = Files.newBufferedWriter(file.toPath(), Charset.defaultCharset());
         PrintWriter p = new PrintWriter(bw)) {
      p.print(output);

      final Command c = new Chatter.DisplayText(
        GameModule.getGameModule().getChatter(),
        Resources.getString("Inventory.wrote", file)  //$NON-NLS-1$
      );
      c.execute();
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, file);
    }
  }

  public GamePiece getSelectedCounter() {
    GamePiece piece = null;
    final CounterNode node = (CounterNode) tree.getLastSelectedPathComponent();
    if (node != null && node.isLeaf()) {
      piece = node.getCounter().getPiece();
    }
    return piece;
  }

  @Override
  public void removeFrom(Buildable b) {
    super.removeFrom(b);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public void remove(Buildable b) {
  }

  protected void launch() {
    if (FUNCTION_HIDE.equals(launchFunction) && frame.isVisible()) {
      frame.setVisible(false);
    }
    else {
      refresh();
      frame.setVisible(true);
    }
  }

  private void buildTreeModel() {
    // Initialize all pieces with CurrentBoard correctly.
    for (final VASSAL.build.module.Map m : VASSAL.build.module.Map.getMapList()) {
      m.getPieces();
    }

    final ArrayList<String> path = new ArrayList<>();
    Collections.addAll(path, groupBy);
    results = new CounterInventory(
      new Counter(this.getConfigureName()), path, sortPieces);

    final PieceIterator pi = new PieceIterator(
      GameModule.getGameModule().getGameState().getAllPieces().iterator(),
      piecePropertiesFilter
    );

    while (pi.hasMoreElements()) {
      final ArrayList<String> groups = new ArrayList<>();
      final GamePiece p = pi.nextPiece();

      if (p instanceof Decorator || p instanceof BasicPiece) {
        for (final String s : groupBy) {
          if (s.length() > 0) {
            final String prop = (String) p.getLocalizedProperty(s);
            if (prop != null)
              groups.add(prop);
          }
        }

        int count = 1;
        if (nonLeafFormat.length() > 0)
          count = getTotalValue(p);

        final Counter c = new Counter(p, groups, count, pieceFormat, sortFormat);
        // Store
        results.insert(c);
      }
    }
  }

  protected int getTotalValue(GamePiece p) {
    final String s = (String) p.getProperty(nonLeafFormat);
    int count;
    try {
      count = Integer.parseInt(s);
    }
    catch (NumberFormatException e) {
      // Count each piece as 1 if the property isn't a number
      count = 1;
    }

    return count;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Inventory.html"); //$NON-NLS-1$
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(super.getAttributeDescriptions(),
      // "Display",
      Resources.getString("Editor.Inventory.show_pieces"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.sort_group_properties"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.label_folders"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.show_folders"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.label_pieces"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.sort"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.label_sort"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.sort_method"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.center_piece"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.forward_keystroke"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.rightclick_piece"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.draw_piece"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.zoom"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.available"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.function"), //$NON-NLS-1$
      Resources.getString("Editor.Inventory.refresh_hotkey")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(super.getAttributeTypes(),
      // DestConfig.class,
      PropertyExpression.class,
      String[].class,
      String.class,
      Boolean.class,
      PieceFormatConfig.class,
      Boolean.class,
      PieceFormatConfig.class,
      SortConfig.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Double.class,
      String[].class,
      FunctionConfig.class,
      NamedKeyStroke.class
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(super.getAttributeNames(),
   // DEST,
      FILTER,
      GROUP_BY,
      NON_LEAF_FORMAT,
      FOLDERS_ONLY,
      LEAF_FORMAT,
      SORT_PIECES,
      SORT_FORMAT,
      SORTING,
      CENTERONPIECE,
      FORWARD_KEYSTROKE,
      SHOW_MENU,
      DRAW_PIECES,
      PIECE_ZOOM,
      SIDES,
      LAUNCH_FUNCTION,
      REFRESH_HOTKEY
    );
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/inventory.gif"); //$NON-NLS-1$
    }
  }

  public static class PieceFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new GamePieceFormattedStringConfigurer(key, name);
    }
  }

  public static class SortConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new StringEnumConfigurer(key, name, SORT_OPTIONS);
    }
  }

  public static class FunctionConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new TranslatingStringEnumConfigurer(key, name, FUNCTION_OPTIONS, FUNCTION_KEYS);
    }
  }

  @Override
  public void setAttribute(String key, Object o) {
    if (FILTER.equals(key)) {
      piecePropertiesFilter.setExpression((String) o);
    }
    else if (GROUP_BY.equals(key)) {
      if (o instanceof String) {
        o = StringArrayConfigurer.stringToArray((String) o);
      }
      groupBy = (String[]) o;
    }
    else if (NON_LEAF_FORMAT.equals(key)) {
      nonLeafFormat = (String) o;
    }
    else if (LEAF_FORMAT.equals(key)) {
      pieceFormat = (String) o;
    }
    else if (CENTERONPIECE.equals(key)) {
      centerOnPiece = getBooleanValue(o);
    }
    else if (SHOW_MENU.equals(key)) {
      showMenu = getBooleanValue(o);
    }
    else if (DRAW_PIECES.equals(key)) {
      drawPieces = getBooleanValue(o);
    }
    else if (FOLDERS_ONLY.equals(key)) {
      foldersOnly = getBooleanValue(o);
      cutAboveLeaves = foldersOnly ? 1 : 0;
    }
    else if (PIECE_ZOOM.equals(key)) {
      if (o instanceof String) {
        o = Double.valueOf((String) o);
      }
      pieceZoom = (Double) o;
    }
    else if (FORWARD_KEYSTROKE.equals(key)) {
      forwardKeystroke = getBooleanValue(o);
    }
    else if (SIDES.equals(key)) {
      if (o instanceof String) {
        o = StringArrayConfigurer.stringToArray((String) o);
      }
      sides = (String[]) o;
    }
    else if (KEYSTROKE.equals(key)) {
      if (o instanceof String) {
        o = HotKeyConfigurer.decode((String) o);
      }
      keyStroke = (KeyStroke) o;
    }
    else if (CUTBELOWROOT.equals(key)) {
      if (o instanceof String)
        cutBelowRoot = Integer.parseInt((String) o);
      else {
        cutBelowRoot = (Integer) o;
      }
    }
    else if (CUTABOVELEAVES.equals(key)) {
      if (o instanceof String)
        cutAboveLeaves = Integer.parseInt((String) o);
      else {
        cutAboveLeaves = (Integer) o;
      }
    }
    else if (SORT_PIECES.equals(key)) {
      sortPieces = getBooleanValue(o);
    }
    else if (SORT_FORMAT.equals(key)) {
      sortFormat = (String) o;
    }
    else if (SORTING.equals(key)) {
      sortStrategy = (String) o;
    }
    else if (LAUNCH_FUNCTION.equals(key)) {
      launchFunction = (String) o;
    }
    else if (REFRESH_HOTKEY.equals(key)) {
      if (o instanceof String) {
        o = NamedHotKeyConfigurer.decode((String) o);
      }
      refreshListener.setKeyStroke((NamedKeyStroke) o);
    }
    else {
      super.setAttribute(key, o);
    }
  }

  private final VisibilityCondition piecesVisible = () -> !foldersOnly;

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (PIECE_ZOOM.equals(name)) {
      return () -> drawPieces && !foldersOnly;
    }
    else if (List.of(LEAF_FORMAT, CENTERONPIECE, FORWARD_KEYSTROKE, SHOW_MENU, DRAW_PIECES).contains(name)) {
      return piecesVisible;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  /**
   * @param o object
   */
  protected boolean getBooleanValue(Object o) {
    if (o instanceof String) {
      o = Boolean.valueOf((String) o);
    }
    return (Boolean) o;
  }

  @Override
  public String getAttributeValueString(String key) {
    if (FILTER.equals(key)) {
      return piecePropertiesFilter.getExpression();
    }
    else if (GROUP_BY.equals(key)) {
      return StringArrayConfigurer.arrayToString(groupBy);
    }
    else if (NON_LEAF_FORMAT.equals(key)) {
      return nonLeafFormat;
    }
    else if (LEAF_FORMAT.equals(key)) {
      return pieceFormat;
    }
    else if (CENTERONPIECE.equals(key)) {
      return String.valueOf(centerOnPiece);
    }
    else if (FORWARD_KEYSTROKE.equals(key)) {
      return String.valueOf(forwardKeystroke);
    }
    else if (SHOW_MENU.equals(key)) {
      return String.valueOf(showMenu);
    }
    else if (DRAW_PIECES.equals(key)) {
      return String.valueOf(drawPieces);
    }
    else if (FOLDERS_ONLY.equals(key)) {
      return String.valueOf(foldersOnly);
    }
    else if (PIECE_ZOOM.equals(key)) {
      return String.valueOf(pieceZoom);
    }
    else if (SIDES.equals(key)) {
      return StringArrayConfigurer.arrayToString(sides);
    }
    else if (KEYSTROKE.equals(key)) {
      return HotKeyConfigurer.encode(keyStroke);
    }
    else if (CUTBELOWROOT.equals(key)) {
      return Integer.toString(cutBelowRoot);
    }
    else if (CUTABOVELEAVES.equals(key)) {
      return Integer.toString(cutAboveLeaves);
    }
    else if (SORT_PIECES.equals(key)) {
      return Boolean.toString(sortPieces);
    }
    else if (SORT_FORMAT.equals(key)) {
      return sortFormat;
    }
    else if (SORTING.equals(key)) {
      return sortStrategy;
    }
    else if (LAUNCH_FUNCTION.equals(key)) {
      return launchFunction;
    }
    else if (REFRESH_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(refreshListener.getNamedKeyStroke());
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  @Override
  public void setup(boolean gameStarting) {
    getLaunchButton().setEnabled(gameStarting && enabledForPlayersSide());
    if (gameStarting) {
      setupLaunch();
    }
    else {
      if (!GameModule.getGameModule().isLoadOverSemaphore()) {
        frame.setVisible(false);
      }
    }
  }

  protected void setupLaunch() {
    final LaunchButton myButton = getLaunchButton();
    myButton.setEnabled(enabledForPlayersSide());
    // Only change button visibility if it has not already been hidden by a ToolBarMenu
    if (myButton.getClientProperty(ToolbarMenu.HIDDEN_BY_TOOLBAR) == null) {
      myButton.setVisible(myButton.isEnabled());
    }
  }

  /**
   * Update inventory according to change of side.
   */
  @Override
  public void sideChanged(String oldSide, String newSide) {
    setupLaunch();
  }

  protected boolean enabledForPlayersSide() {
    if (sides == null || sides.length == 0)
      return true;
    for (final String side : sides) {
      if (side.equalsIgnoreCase(PlayerRoster.getMySide()))
        return true;
    }
    return false;
  }

  /**
   * @return Command which only has some text in. The actual processing is done
   *         within the pieces.
   */
  protected Command sendHotKeyToPieces(final KeyStroke keyStroke) {
    final Command c = new NullCommand();
    final TreePath[] tp = tree.getSelectionPaths();
    // set to not get duplicates
    final HashSet<GamePiece> pieces = new HashSet<>();
    for (final TreePath treePath : tp) {
      final CounterNode node = (CounterNode) treePath.getLastPathComponent();
      if (node.isLeaf()) {
        pieces.add(node.getCounter().getPiece());
      }
      else {
        for (final Iterator<CounterNode> j = node.iterator(); j.hasNext();) {
          final CounterNode childNode = j.next();
          if (childNode.isLeaf())
            pieces.add(childNode.getCounter().getPiece());
        }
      }
    }

    for (final GamePiece piece : pieces) {
      GameModule.getGameModule().sendAndLog(piece.keyEvent(keyStroke));
    }
    return c;
  }

  protected Command myUndoCommand() {
    return null;
  }

  private void refresh() {
    // Make an attempt to keep the same nodes expanded
    final HashSet<String> expanded = new HashSet<>();
    final int n = tree.getRowCount();
    for (int i = 0; i < n; ++i) {
      if (tree.isExpanded(i)) {
        expanded.add(tree.getPathForRow(i).getLastPathComponent().toString());
      }
    }
    buildTreeModel();
    tree.setModel(results);

    for (int i = 0; i < tree.getRowCount(); ++i) {
      if (expanded.contains(
            tree.getPathForRow(i).getLastPathComponent().toString())) {
        tree.expandRow(i);
      }
    }
  }

  public class HotKeySender implements KeyListener {
    BoundsTracker tracker;

    public void keyCommand(KeyStroke stroke) {
      if (forwardKeystroke) {
        final CounterNode node = (CounterNode) tree.getLastSelectedPathComponent();
        if (node != null) {
          final Command comm = getCommand(node, stroke);
          if (comm != null && !comm.isNull()) {
            tracker.repaint();
            GameModule.getGameModule().sendAndLog(comm);
            tracker = null;
            refresh();
          }
        }
      }
    }

    protected Command getCommand(CounterNode node, KeyStroke stroke) {
      final GamePiece p = node.getCounter() == null ? null : node.getCounter().getPiece();
      Command comm;
      if (p != null) {
        // Save state first
        p.setProperty(Properties.SNAPSHOT, ((PropertyExporter) p).getProperties());
        if (tracker == null) {
          tracker = new BoundsTracker();
          tracker.addPiece(p);
        }
        comm = p.keyEvent(stroke);
      }
      else {
        comm = new NullCommand();
        final int n = node.getChildCount();
        for (int i = 0; i < n; ++i) {
          comm = comm.append(getCommand((CounterNode) node.getChild(i), stroke));
        }
      }
      return comm;
    }

    @Override
    public void keyPressed(KeyEvent e) {
      keyCommand(SwingUtils.getKeyStrokeForEvent(e));
    }

    @Override
    public void keyReleased(KeyEvent e) {
      keyCommand(SwingUtils.getKeyStrokeForEvent(e));
    }

    @Override
    public void keyTyped(KeyEvent e) {
      keyCommand(SwingUtils.getKeyStrokeForEvent(e));
    }
  }

//  public static class Dest extends StringEnum {
//    public String[] getValidValues(AutoConfigurable target) {
//      return new String[] {DEST_CHAT, DEST_DIALOG, DEST_TREE};
//    }
//  }
//  public static class SortStrategy extends StringEnum {
//    public String[] getValidValues(AutoConfigurable target) {
//      return new String[] {ALPHA, LENGTHALPHA, NUMERIC};
//    }
//  }

  /**
   * Holds static information of and a reference to a gamepiece. Pay attention
   * to the equals method. It checks if two pieces can be found under the same
   * path!
   *
   * @author Brent Easton and Torsten Spindler
   *
   */
  public class Counter implements PropertySource {
    // The gamepiece is stored here to allow dynamic changes of name, location
    // and so forth
    protected GamePiece piece;
    protected GamePiece source;
    protected List<String> groups;
    protected int value;
    // Only used when no piece is defined
    protected String localName;
    protected FormattedString format;
    protected FormattedString sortingFormat;
    protected CounterNode node;

    public Counter(String name) {
      this(name, null);
    }

    public Counter(String name, GamePiece p) {
      this(null, null, 0, nonLeafFormat, sortFormat);
      this.localName = name;
      this.source = p;
    }

    public Counter(GamePiece piece, List<String> groups, int value,
                   String format, String sortFormat) {
      this.piece = piece;
      this.value = value;
      this.groups = groups;
      this.format = new FormattedString(format);
      this.sortingFormat = new FormattedString(sortFormat);
    }

    // piece can be null, so provide a alternate name
    public String getName() {
      if (piece != null)
        return piece.getName();
      return localName;
    }

    @Override
    public int hashCode() {
      return getName().hashCode();
    }

    @Override
    public String toString() {
      return format.getLocalizedText(this);
    }
    public String toSortKey() {
      return sortingFormat.getLocalizedText(this);
    }

    public String[] getPath() {
      return groups.toArray(new String[0]);
    }

    public int getValue() {
      return value;
    }

    public void setValue(int value) {
      this.value = value;
    }

    public GamePiece getPiece() {
      return piece;
    }

    public void setPiece(GamePiece piece) {
      this.piece = piece;
    }

    @Override
    public boolean equals(Object o) {
      if (!(o instanceof Counter))
        return false;
      final Counter c = (Counter) o;
      return getPath().equals(c.getPath());
    }

    @Override
    public Object getProperty(Object key) {
      Object value = null;
      final String s = (String) key;
      if (s.startsWith("sum_")) { //$NON-NLS-1$
        if (piece != null) {
          value = piece.getProperty(s.substring(4));
        }
        else {
          int sum = 0;
          final int n = results.getChildCount(node);
          for (int i = 0; i < n; ++i) {
            try {
              final CounterNode childNode = (CounterNode) results.getChild(node, i);
              sum += Integer.parseInt((String) (childNode.getCounter()).getProperty(key));
            }
            catch (NumberFormatException e) {
              // Count each piece as 1 if property isn't a number
              sum++;
            }
          }
          value = String.valueOf(sum);
        }
      }
      else if ("PropertyValue".equals(s)) { //$NON-NLS-1$
        return localName;
      }
      else if (piece != null) {
        value = piece.getProperty(key);
      }
      else if (source != null) {
        value = source.getProperty(key);
      }
      return value;
    }

    @Override
    public Object getLocalizedProperty(Object key) {
      return getProperty(key);
    }

    public void setNode(CounterNode node) {
      this.node = node;
    }
  }

  /**
   * Filter to select pieces required
   *
   * @author Brent Easton
   */
  protected static class Selector implements PieceFilter {

    protected PieceFilter filter;

    public Selector(String include) {
      if (include != null && include.length() > 0) {
        filter = PropertiesPieceFilter.parse(include);
      }
    }

    @Override
    public boolean accept(GamePiece piece) {
      // Honor visibility

      if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME)))
        return false;

      // Ignore Stacks, pieces are reported individually from GameState
      if (piece instanceof Stack)
        return false;

      // Don't report pieces with no map
      if (piece.getMap() == null)
        return false;

      // Check for marker
      if (filter != null) {
        return filter.accept(piece);
      }

      // Default Accept piece
      return true;
    }

  }

  /**
   * CounterNode for the result tree.
   *
   * @author spindler
   *
   */
  public class CounterNode implements Comparable<CounterNode> {
    protected final String entry;
    protected final Counter counter;
    protected List<CounterNode> children;
    protected int level;

    // protected int depth;

    public CounterNode(final String entry,
                       final Counter counter,
                       final int level) {
      this(entry, counter);
      this.level = level;
    }

    protected CounterNode(String entry, Counter counter) {
      this.level = 0;
      // this.depth = 0;
      this.entry = entry;
      this.counter = counter;
      counter.setNode(this);
      children = new ArrayList<>();
    }

    @Override
    public String toString() {
      if (counter != null)
        return counter.toString();
      return getEntry();
    }

    /**
     * Places a separator between elements.
     * The separator consists of an indent and a linebreak.
     * @return
     */
    protected String separator() {
      final StringBuilder sep = new StringBuilder();

      if (getLevel() > 0)
        sep.append(mapSeparator);
      for (int i = 0; i < getLevel(); i++)
        sep.append(groupSeparator);
      return sep.toString();
    }

    public String toResultString() {
      final StringBuilder name = new StringBuilder();

      name.append(separator());

      if (counter != null)
        name.append(counter);
      else
        name.append(getEntry());

      for (final CounterNode child : children) {
        name.append(child.toResultString());
      }
      return name.toString();
    }

    public String getEntry() {
      return entry;
    }

    public Counter getCounter() {
      return counter;
    }

    public void addChild(final CounterNode counterNode, boolean sort) {
      children.add(counterNode);
      if (sort)
        sortChildren();
    }

    public void addChild(final int i,
                         final CounterNode counterNode, boolean sort) {
      children.add(i, counterNode);
      if (sort)
        sortChildren();
    }

    protected void sortChildren() {
      if (sortStrategy.equals(ALPHA))
        Collections.sort(children);
      else if (sortStrategy.equals(LENGTHALPHA))
        children.sort(new LengthAlpha());
      else if (sortStrategy.equals(NUMERIC))
        children.sort(new Numerical());
      else
        Collections.sort(children);
    }

    public void removeChild(CounterNode child) {
      children.remove(child);
    }

    public int getChildCount() {
      return children.size();
    }

    public boolean isLeaf() {
      return children.isEmpty();
    }

    public Object getChild(final int index) {
      return children.get(index);
    }

    public int getIndexOfChild(final Object child) {
      return children.indexOf(child);
    }

    public int getLevel() {
      return level;
    }

    public void setLevel(final int level) {
      this.level = level;
    }

    public int updateValues() {
      int value = 0;
      if (counter != null)
        value = counter.getValue();

      // inform children about update
      for (final CounterNode child : children) {
        value += child.updateValues();
      }

      // save new value in counter
      counter.setValue(value);
      return counter.getValue();
    }

    public Iterator<CounterNode> iterator() {
      return children.iterator();
    }

    public void cutLevel(int cut) {
      if (cut == 0) {
        children.clear();
        return;
      }

      for (final CounterNode child : children) {
        child.cutLevel(cut - 1);
      }
    }

    public void cutLeaves() {
      final ArrayList<CounterNode> toBeRemoved = new ArrayList<>();
      for (final CounterNode child : children) {
        if (child.isLeaf())
          toBeRemoved.add(child);
        else
          child.cutLeaves();
      }
      children.removeAll(toBeRemoved);
    }

    /**
     * Compare this CounterNode to another one based on the respective SortKeys.
     */
    @Override
    public int compareTo(CounterNode node) {
      return this.toSortKey().compareTo(node.toSortKey());
    }

    /**
     * Sort this CounterNode by the counters key, if no counter use the label.
     * If no children, use the name of the counterNode, probably could be
     * $PropertyValue$ as well?
     *
     * @return key as String
     */
    protected String toSortKey() {
      String sortKey = getEntry();
      if (counter != null)
        sortKey = counter.toSortKey();
      if (!children.isEmpty())
        sortKey = toString();
      return sortKey;
    }

    /**
     * Base class for comparing two CounterNodes. Contains methods for
     * sanity checking of arguments and comparing non-sane arguments.
     *
     * @author spindler
     */
    protected class CompareCounterNodes {
      /**
       * Sanity check for arguments.
       * @return true if arguments looks processable, false else
       */
      protected boolean argsOK(Object arg0, Object arg1) {
        return
          arg0 instanceof CounterNode &&
          arg1 instanceof CounterNode;
      }

      protected int compareStrangeArgs(Object arg0, Object arg1) {
        if (arg0 == null)
          return 1;

        if (arg0.equals(arg1))
          return 0;

        if (arg1 == null)
          return -1;
        if (arg0 instanceof CounterNode && !(arg1 instanceof CounterNode))
          return -1;
        if (arg1 instanceof CounterNode && !(arg0 instanceof CounterNode))
          return 1;

        throw new IllegalArgumentException(
          "These CounterNodes are not strange!"); //$NON-NLS-1$
      }
    }

    /**
     * Compare two CounterNodes based on the alphanumerical order of their
     * SortKeys.
     *
     * @author spindler
     */
    protected class Alpha extends CompareCounterNodes
                          implements Comparator<CounterNode> {

      @Override
      public int compare(CounterNode left, CounterNode right) {
        if (!argsOK(left, right))
          return compareStrangeArgs(left, right);

        return left.compareTo(right);
      }
    }

    /**
     * Compare two CounterNodes based on the first integer value found in
     * their SortKeys. If a CounterNodes SortKey does not contain an integer
     * at all it is assigned the lowest available integer.
     *
     * @author spindler
     */
    protected class Numerical extends CompareCounterNodes
                              implements Comparator<CounterNode> {
      protected final String regex =  "\\d+"; //$NON-NLS-1$ //NOPMD
      protected final Pattern p = Pattern.compile(regex);

      /**
       * Get first integer in key, if any. Otherwise return lowest possible
       * integer.
       *
       * @param key is a string that may or may not contain an integer value
       * @return the value of the integer found, min(Integer) otherwise
       *
       */
      protected int getInt(String key) {
        int found = Integer.MIN_VALUE;
        final Matcher match = p.matcher(key);

        if (!match.find()) {
          // return minimum value
          return found;
        }
        final int start = match.start();
        found = Integer.parseInt(key.substring(start, match.end()));

        // Check for sign
        if ((start > 0) && (key.charAt(start - 1) == '-')) {
          // negative integer found
          // FIXME: Is this a safe operation? What happens when
          // MAX_VALUE * -1 < MIN_VALUE?
          found *= -1;
        }
        return found;
      }

      /**
       * Compare two CounterNodes based on the first integer found in their
       * SortKeys.
       */
      @Override
      public int compare(CounterNode left, CounterNode right) {
        if (!argsOK(left, right))
          return compareStrangeArgs(left, right);

        final int l = getInt(left.toSortKey());
        final int r = getInt(right.toSortKey());

        if (l < r)
          return -1;
        if (l > r)
          return 1;

        return 0;
      }
    }

    /**
     * Compare two CounterNodes based on the length of their SortKeys and
     * alphanumerical sorting.
     *
     * @author spindler
     */
    protected class LengthAlpha extends CompareCounterNodes
                                implements Comparator<CounterNode> {
      @Override
      public int compare(CounterNode left, CounterNode right) {
        if (!argsOK(left, right))
          return compareStrangeArgs(left, right);

        final int leftLength = left.toSortKey().length();
        final int rightLength = right.toSortKey().length();
        if (leftLength < rightLength)
          return -1;
        if (leftLength > rightLength)
          return 1;
        // Native comparison
        return (left.compareTo(right));
      }
    }
  }

  public class CounterInventory implements TreeModel {
    // Needed for TreeModel
    protected List<TreeModelListener> treeModelListeners =
      new ArrayList<>();
    // This contains shortcuts to the nodes of the tree
    protected Map<String, CounterNode> inventory;
    // The start of the tree
    protected CounterNode root;
    // Text view of the tree
    protected String resultString;
    // The path determines where a counter is found in the tree
    protected List<String> path;
    // Small speed up, only update values in tree when something has changed
    protected boolean changed;
    // Sort the tree
    protected boolean sort;

    public CounterInventory(Counter c, List<String> path, boolean sort) {
      this.root = new CounterNode(c.getName(), c);
      this.path = path;
      this.inventory = new HashMap<>();
      this.sort = sort;
      changed = true;
    }

    /**
     * insert counter into the tree. It is not sorted in any way.
     *
     * @param counter
     */
    public void insert(Counter counter) {
      final String[] path = counter.getPath();
      final StringBuilder hash = new StringBuilder();

      CounterNode insertNode = root;
      CounterNode newNode;
      for (int j = 0; path != null && j < path.length; j++) {
        hash.append(path[j]);
        if (inventory.get(hash.toString()) == null) {
          newNode = new CounterNode(
            path[j], new Counter(path[j], counter.getPiece()), insertNode.getLevel() + 1);
          inventory.put(hash.toString(), newNode);
          insertNode.addChild(newNode, sort);
        }
        insertNode = inventory.get(hash.toString());
      }
      newNode = new CounterNode(
        counter.toString(), counter, insertNode.getLevel() + 1);
      insertNode.addChild(newNode, sort);
      changed = true;
    }

    private void updateEntries() {
      root.updateValues();
    }

    /**
     * Deliver information of the tree as text.
     *
     * @return String
     */
    public String getResultString() {
      if (changed)
        updateTree();
      changed = false;
      return root.toResultString();
    }

    /**
     * Compatibility for DisplayResults class.
     *
     * @return String[]
     */
    public String[] getResultStringArray() {
      return new String[] {getResultString()};
    }

    @Override
    public Object getRoot() {
      if (changed)
        updateTree();
      return root;
    }

    private void updateTree() {
      updateEntries();
      if (cutBelowRoot > 0)
        root.cutLevel(cutBelowRoot);
      for (int i = cutAboveLeaves; i > 0; i--)
        root.cutLeaves();
      changed = false;
    }

    @Override
    public int getChildCount(Object parent) {
      final CounterNode counter = (CounterNode) parent;
      return counter.getChildCount();
    }

    @Override
    public boolean isLeaf(Object node) {
      final CounterNode counter = (CounterNode) node;
      return counter.isLeaf();
    }

    @Override
    public void addTreeModelListener(TreeModelListener l) {
      treeModelListeners.add(l);
    }

    @Override
    public void removeTreeModelListener(TreeModelListener l) {
      treeModelListeners.remove(l);
    }

    public void fireNodesRemoved(Object[] path, int[] childIndices,
                                 Object[] children) {
      final TreeModelEvent e = new TreeModelEvent(this, path, childIndices, children);
      for (final TreeModelListener l : treeModelListeners) {
        l.treeNodesRemoved(e);
      }
    }

    @Override
    public Object getChild(Object parent, int index) {
      final CounterNode counter = (CounterNode) parent;
      return counter.getChild(index);
    }

    @Override
    public int getIndexOfChild(Object parent, Object child) {
      final CounterNode counter = (CounterNode) parent;
      return counter.getIndexOfChild(child);
    }

    @Override
    public void valueForPathChanged(TreePath path, Object newValue) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return List.of(piecePropertiesFilter.getExpression());
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    final List<String> l = new ArrayList<>();
    l.add(nonLeafFormat);
    if (piecesVisible.shouldBeVisible()) {
      l.add(pieceFormat);
    }
    l.add(sortFormat);
    return l;
  }
}
