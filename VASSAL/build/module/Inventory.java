/*
 * $Id$
 *
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton, Torsten Spindler
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
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import VASSAL.build.AbstractConfigurable;
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
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.counters.Stack;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;

public class Inventory extends AbstractConfigurable implements GameComponent {
  protected LaunchButton launch;
  protected CounterInventory results;
  protected JTree tree;

  public static final String VERSION = "2.0";
  public static final String HOTKEY = "hotkey";
  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String TOOLTIP = "tooltip";
  public static final String DEST = "destination";

  /*
   * For use in formatted text output.
   */
  final protected String mapSeparator = "\n";
  final protected String groupSeparator = "   ";

  /*
   * Options Destination - Chat, Dialog, File.
   */
  public static final String DEST_CHAT = "Chat Window";
  public static final String DEST_DIALOG = "Dialog Window";
  public static final String DEST_TREE = "Tree Window";
  protected String destination = DEST_TREE;

  public static final String FILTER = "include";
  protected String piecePropertiesFilter = "";

  public static final String GROUP_BY = "groupBy";
  protected String[] groupBy = {""};

  public static final String NON_LEAF_FORMAT = "nonLeafFormat";
  protected String nonLeafFormat = "$PropertyValue$";

  public static final String CENTERONPIECE = "centerOnPiece";
  protected boolean centerOnPiece = true;

  public static final String FORWARD_KEYSTROKE = "forwardKeystroke";
  protected boolean forwardKeystroke = true;

  public static final String SHOW_MENU = "showMenu";
  protected boolean showMenu = true;

  public static final String SIDES = "sides";
  protected String[] sides = null;

  public static final String KEYSTROKE = "keystroke";
  protected KeyStroke keyStroke = null;

  public static final String CUTBELOWROOT = "cutRoot";
  protected int cutBelowRoot = 0;

  public static final String CUTABOVELEAVES = "cutLeaves";
  protected int cutAboveLeaves = 0;

  public static final String LEAF_FORMAT = "leafFormat";
  protected String pieceFormat = "$PieceName$";

  public static final String PIECE_ZOOM = "pieceZoom";
  protected double pieceZoom = .25;

  public static final String DRAW_PIECES = "drawPieces";
  protected boolean drawPieces = true;

  public static final String FOLDERS_ONLY = "foldersOnly";
  protected boolean foldersOnly = false;

  protected JDialog frame;

  public Inventory() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Inventory");
    setAttribute(BUTTON_TEXT, "Inventory");
    setAttribute(TOOLTIP, "Show inventory of all pieces");
    setAttribute(ICON, "/images/inventory.gif");
    launch.setEnabled(false);
    launch.setVisible(false);
  }

  public static String getConfigureTypeName() {
    return "Game Piece Inventory Window";
  }

  public void addTo(Buildable b) {
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getToolBar().add(getComponent());
    GameModule.getGameModule().getGameState().addGameComponent(this);
    frame = new JDialog(GameModule.getGameModule().getFrame());
    frame.setTitle(getConfigureName());
    String key = "Inventory." + getConfigureName();
    GameModule.getGameModule().getPrefs().addOption(new PositionOption(key, frame));
    frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
    frame.getContentPane().add(initTree());
    frame.getContentPane().add(initButtons());
    frame.setSize(150, 350);
  }

  /**
   * Construct an explorer like interface for the selected counters
   */
  protected Component initTree() {
    // Initialize the tree to be displayed from the results tree
    tree = new JTree();
    tree.setRootVisible(false);
    tree.setShowsRootHandles(true);
    tree.setCellRenderer(initTreeCellRenderer());
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    // If wanted center on a selected counter
    tree.addTreeSelectionListener(new TreeSelectionListener() {
      public void valueChanged(TreeSelectionEvent e) {
        if (centerOnPiece) {
          GamePiece piece = getSelectedCounter();
          if (piece != null)
            piece.getMap().centerAt(piece.getPosition());
        }
      }
    });
    tree.addMouseListener(new MouseAdapter() {
      public void mouseReleased(MouseEvent e) {
        if (showMenu && e.isMetaDown()) {
          final TreePath path = tree.getPathForLocation(e.getX(), e.getY());
          if (path != null) {
            if (path.getLastPathComponent() instanceof CounterNode) {
              final CounterNode node = (CounterNode) path.getLastPathComponent();
              final GamePiece p = node.getCounter().getPiece();
              JPopupMenu menu = MenuDisplayer.createPopup(p);
              menu.addPropertyChangeListener("visible", new PropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent evt) {
                  if (Boolean.FALSE.equals(evt.getNewValue())) {
                    SwingUtilities.invokeLater(new Runnable() {
                      public void run() {
                        refresh();
                      }
                    });
                  }
                }
              });
              menu.show(tree, e.getX(), e.getY());
            }
          }
        }
      }
    });
    tree.addKeyListener(new HotKeySender());

    JScrollPane scrollPane = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    return scrollPane;
  }

  protected TreeCellRenderer initTreeCellRenderer() {
    return new DefaultTreeCellRenderer() {

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
            setIcon(new Icon() {

              public int getIconHeight() {
                return r.height;
              }

              public int getIconWidth() {
                return r.width;
              }

              public void paintIcon(Component c, Graphics g, int x, int y) {
                piece.draw(g, -r.x, -r.y, c, pieceZoom);
              }

            });
          }
        }
        return this;
      }

    };
  }

  protected Component initButtons() {
    Box buttonBox = Box.createHorizontalBox();
    JButton refreshButton = new JButton("Refresh");
    refreshButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        refresh();
      }
    });
    buttonBox.add(refreshButton);
    JButton closeButton = new JButton("Close");
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        frame.setVisible(false);
      }
    });
    buttonBox.add(closeButton);
    return buttonBox;
  }

  public GamePiece getSelectedCounter() {
    GamePiece piece = null;
    CounterNode node = (CounterNode) tree.getLastSelectedPathComponent();
    if (node != null && node.isLeaf()) {
      piece = node.getCounter().getPiece();
    }
    return piece;
  }

  protected Component getComponent() {
    return launch;
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public void add(Buildable b) {
  }

  public void remove(Buildable b) {
  }

  protected void launch() {
    refresh();
    frame.setVisible(true);
  }

  private void buildTreeModel() {
    ArrayList path = new ArrayList();
    for (int i = 0; i < groupBy.length; i++)
      path.add(groupBy[i]);
    results = new CounterInventory(new Counter(this.getConfigureName()), path);

    PieceIterator pi = new PieceIterator(GameModule.getGameModule().getGameState().getPieces(), new Selector(piecePropertiesFilter));

    while (pi.hasMoreElements()) {
      ArrayList groups = new ArrayList();
      GamePiece p = pi.nextPiece();

      for (int i = 0; i < groupBy.length; i++) {
        if (groupBy[i].length() > 0) {
          String prop = (String) p.getProperty(groupBy[i]);
          if (prop != null)
            groups.add(p.getProperty(groupBy[i]));
        }
      }

      int count = 1;
      if (nonLeafFormat.length() > 0)
        count = getTotalValue(p);

      Counter c;
      c = new Counter(p, groups, count, pieceFormat);
      // Store
      results.insert(c);
    }
  }

  protected int getTotalValue(GamePiece p) {
    String s = (String) p.getProperty(nonLeafFormat);
    int count = 1;
    try {
      count = Integer.parseInt(s);
    }
    catch (Exception e) {
      count = 1;
    }

    return count;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Inventory.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }
  
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Name", "Button text", "Button icon", "Hotkey", "Tooltip", "Show only pieces matching these properties", "Sort and Group By Properties",
        "Label for folders", "Show only folders", "Label for pieces", "Center on selected piece", "Forward key strokes to selected piece",
        "Show right-click menu of piece", "Draw piece images", "Zoom factor", "Available to these sides"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, IconConfig.class, KeyStroke.class, String.class, String.class, String[].class, String.class, Boolean.class,
        PieceFormatConfig.class, Boolean.class, Boolean.class, Boolean.class, Boolean.class, Double.class, String[].class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, BUTTON_TEXT, ICON, HOTKEY, TOOLTIP, FILTER, GROUP_BY, NON_LEAF_FORMAT, FOLDERS_ONLY, LEAF_FORMAT, CENTERONPIECE,
        FORWARD_KEYSTROKE, SHOW_MENU, DRAW_PIECES, PIECE_ZOOM, SIDES};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/inventory.gif");
    }
  }
  public static class PieceFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new GamePieceFormattedStringConfigurer(key, name);
    }
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (FILTER.equals(key)) {
      piecePropertiesFilter = (String) o;
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
      pieceZoom = ((Double) o).doubleValue();
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
        cutBelowRoot = ((Integer) o).intValue();
      }
    }
    else if (CUTABOVELEAVES.equals(key)) {
      if (o instanceof String)
        cutAboveLeaves = Integer.parseInt((String) o);
      else {
        cutAboveLeaves = ((Integer) o).intValue();
      }
    }

    else {
      launch.setAttribute(key, o);
    }
  }

  private VisibilityCondition piecesVisible = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !foldersOnly;
    }
  };

  public VisibilityCondition getAttributeVisibility(String name) {
    if (PIECE_ZOOM.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return drawPieces && !foldersOnly;
        }
      };
    }
    else if (LEAF_FORMAT.equals(name) || CENTERONPIECE.equals(name) || FORWARD_KEYSTROKE.equals(name) || SHOW_MENU.equals(name) || DRAW_PIECES.equals(name)) {
      return piecesVisible;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  /**
   * @param o
   */
  protected boolean getBooleanValue(Object o) {
    if (o instanceof String) {
      o = new Boolean((String) o);
    }
    return ((Boolean) o).booleanValue();
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (FILTER.equals(key)) {
      return piecePropertiesFilter;
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
      return cutBelowRoot + "";
    }
    else if (CUTABOVELEAVES.equals(key)) {
      return cutAboveLeaves + "";
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting && enabledForPlayersSide());
    launch.setVisible(launch.isEnabled());
  }

  protected boolean enabledForPlayersSide() {
    if (sides == null || sides.length == 0)
      return true;
    for (int i = 0; i < sides.length; i++) {
      if (sides[i].equalsIgnoreCase(PlayerRoster.getMySide()))
        return true;
    }
    return false;
  }

  protected void executeCommand() {
    if (destination.equals(DEST_CHAT)) {
      Command c = new NullCommand();
      String res[] = results.getResultStringArray();

      for (int i = 0; i < res.length; i++) {
        c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), res[i]));
      }
      c.execute();
      GameModule.getGameModule().sendAndLog(c);
    }
    else if (destination.equals(DEST_DIALOG)) {
      String res[] = results.getResultStringArray();
      String text = "";
      for (int i = 0; i < res.length; i++) {
        text += res[i] + "\n";
      }
      JTextArea textArea = new JTextArea(text);
      textArea.setEditable(false);

      JScrollPane scrollPane = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

      JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), scrollPane, getConfigureName(), JOptionPane.PLAIN_MESSAGE);
    }
    else if (destination.equals(DEST_TREE)) {
      initTree();
    }
  }

  /**
   * @return Command which only has some text in. The actual processing is done
   *         within the pieces.
   */
  protected Command sendHotKeyToPieces(final KeyStroke keyStroke) {
    Command c = new NullCommand();
    final TreePath[] tp = tree.getSelectionPaths();
    // set to not get duplicates
    HashSet pieces = new HashSet();
    for (int i = 0; i < tp.length; i++) {
      CounterNode node = (CounterNode) tp[i].getLastPathComponent();
      if (node.isLeaf()) {
        pieces.add(node.getCounter().getPiece());
      }
      else {
        for (Iterator j = node.iterator(); j.hasNext();) {
          CounterNode childNode = (CounterNode) j.next();
          if (childNode.isLeaf())
            pieces.add(childNode.getCounter().getPiece());
        }
      }
    }
    for (Iterator i = pieces.iterator(); i.hasNext();) {
      GamePiece piece = (GamePiece) i.next();
      GameModule.getGameModule().sendAndLog(piece.keyEvent(keyStroke));
    }
    return c;
  }

  protected Command myUndoCommand() {
    return null;
  }

  private void refresh() {
    // Make an attempt to keep the same nodes expanded
    Set expanded = new HashSet();
    for (int i = 0, n = tree.getRowCount(); i < n; ++i) {
      if (tree.isExpanded(i)) {
        expanded.add(tree.getPathForRow(i).getLastPathComponent().toString());
      }
    }
    buildTreeModel();
    tree.setModel(results);
    for (int i = 0; i < tree.getRowCount(); ++i) {
      if (expanded.contains(tree.getPathForRow(i).getLastPathComponent().toString())) {
        tree.expandRow(i);
      }
    }
  }

  public class HotKeySender implements KeyListener {
    BoundsTracker tracker;

    public void keyCommand(KeyStroke stroke) {
      if (forwardKeystroke) {
        CounterNode node = (CounterNode) tree.getLastSelectedPathComponent();
        if (node != null) {
          Command comm = getCommand(node, stroke);
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
      GamePiece p = node.getCounter() == null ? null : node.getCounter().getPiece();
      Command comm = null;
      if (p != null) {
        // Save state first
        p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
        if (tracker == null) {
          tracker = new BoundsTracker();
          tracker.addPiece(p);
        }
        comm = p.keyEvent(stroke);
      }
      else {
        comm = new NullCommand();
        for (int i = 0, n = node.getChildCount(); i < n; ++i) {
          comm = comm.append(getCommand((CounterNode) node.getChild(i), stroke));
        }
      }
      return comm;
    }

    public void keyPressed(KeyEvent e) {
      keyCommand(KeyStroke.getKeyStrokeForEvent(e));
    }

    public void keyReleased(KeyEvent e) {
      keyCommand(KeyStroke.getKeyStrokeForEvent(e));
    }

    public void keyTyped(KeyEvent e) {
      keyCommand(KeyStroke.getKeyStrokeForEvent(e));
    }

  }
  public static class Dest extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {DEST_CHAT, DEST_DIALOG, DEST_TREE};
    }
  }

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
    protected ArrayList groups;
    protected int value;
    // Only used when no piece is defined
    protected String localName;
    protected FormattedString format;
    protected CounterNode node;

    public Counter(String name) {
      this(null, null, 0, nonLeafFormat);
      this.localName = name;
    }

    public Counter(GamePiece piece, ArrayList groups, int value, String format) {
      this.piece = piece;
      this.value = value;
      this.groups = groups;
      this.format = new FormattedString(format);
    }

    // piece can be null, so provide a alternate name
    public String getName() {
      if (piece != null)
        return piece.getName();
      return localName;
    }

    public int hashCode() {
      return getName().hashCode();
    }

    public String toString() {
      return format.getText(this);
    }

    public String[] getPath() {
      String[] retString = new String[groups.size()];
      for (int i = 0; i < groups.size(); i++)
        retString[i] = (String) groups.get(i);
      return retString;
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

    public boolean equals(Object o) {
      if (!(o instanceof Counter))
        return false;
      Counter c = (Counter) o;
      return getPath().equals(c.getPath());
    }

    public Object getProperty(Object key) {
      Object value = null;
      String s = (String) key;
      if (s.startsWith("sum_")) {
        if (piece != null) {
          value = piece.getProperty(s.substring(4));
        }
        else {
          int sum = 0;
          int n = results.getChildCount(node);
          for (int i = 0; i < n; ++i) {
            try {
              CounterNode childNode = (CounterNode) results.getChild(node, i);
              sum += Integer.parseInt((String) (childNode.getCounter()).getProperty(key));
            }
            catch (NumberFormatException e) {
              sum++;
            }
          }
          value = String.valueOf(sum);
        }
      }
      else if ("PropertyValue".equals(s)) {
        return localName;
      }
      else if (piece != null) {
        value = piece.getProperty(key);
      }
      return value;
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
  protected class Selector implements PieceFilter {

    protected PieceFilter filter;

    public Selector(String include) {
      if (include != null && include.length() > 0) {
        filter = PropertiesPieceFilter.parse(include);
      }
    }

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
  public class CounterNode {
    protected final String entry;
    protected final Counter counter;
    protected List children;
    protected int level;

    // protected int depth;

    public CounterNode(final String entry, final Counter counter, final int level) {
      this(entry, counter);
      this.level = level;
    }

    protected CounterNode(String entry, Counter counter) {
      this.level = 0;
      // this.depth = 0;
      this.entry = entry;
      this.counter = counter;
      counter.setNode(this);
      children = new ArrayList();
    }

    public String toString() {
      if (counter != null)
        return counter.toString();
      return getEntry();
    }

    protected String separator() {
      StringBuffer sep = new StringBuffer();
      if (children.isEmpty()) {
        sep.append(groupSeparator);
      }
      else {
        sep.append(mapSeparator);
        for (int i = 0; i < getLevel(); i++)
          sep.append(groupSeparator);
      }
      return sep.toString();
    }

    public String toResultString() {
      StringBuffer name = new StringBuffer();

      name.append(separator());

      if (counter != null)
        name.append(counter.toString());
      else
        name.append(getEntry());

      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
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

    public void addChild(final CounterNode counterNode) {
      children.add(counterNode);
    }

    public void addChild(final int i, final CounterNode counterNode) {
      children.add(i, counterNode);
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
      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
        value += child.updateValues();
      }

      // save new value in counter
      counter.setValue(value);
      return counter.getValue();
    }

    public Iterator iterator() {
      return children.iterator();
    }

    public void cutLevel(int cut) {
      if (cut == 0) {
        children.clear();
        return;
      }
      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
        child.cutLevel(cut - 1);
      }
    }

    public void cutLeaves() {
      List toBeRemoved = new ArrayList();
      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
        if (child.isLeaf()) {
          toBeRemoved.add(child);
        }
        else
          child.cutLeaves();
      }
      for (Iterator i = toBeRemoved.iterator(); i.hasNext();) {
        CounterNode removeMe = (CounterNode) i.next();
        children.remove(removeMe);
      }
    }

  }

  public class CounterInventory implements TreeModel {
    // Needed for TreeModel
    private Vector treeModelListeners = new Vector();
    // This contains shortcuts to the nodes of the tree
    protected Map inventory;
    // The start of the tree
    protected CounterNode root;
    // Text view of the tree
    protected String resultString;
    // The path determines where a counter is found in the tree
    protected ArrayList path;
    // Small speed up, only update values in tree when something has changed
    protected boolean changed;

    public CounterInventory(Counter c, ArrayList path) {
      this.root = new CounterNode(c.getName(), c);
      this.path = path;
      this.inventory = new HashMap();
      changed = true;
    }

    /**
     * insert counter into the tree. It is not sorted in any way.
     * 
     * @param counter
     */
    public void insert(Counter counter) {
      String[] path = counter.getPath();
      StringBuffer hash = new StringBuffer();

      CounterNode insertNode = root;
      CounterNode newNode = null;
      for (int j = 0; path != null && j < path.length; j++) {
        hash.append(path[j]);
        if (inventory.get(hash.toString()) == null) {
          newNode = new CounterNode(path[j], new Counter(path[j]), insertNode.getLevel() + 1);
          inventory.put(hash.toString(), newNode);
          insertNode.addChild(newNode);
        }
        insertNode = (CounterNode) inventory.get(hash.toString());
      }
      newNode = new CounterNode(counter.toString(), counter, insertNode.getLevel() + 1);
      insertNode.addChild(newNode);
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

    public int getChildCount(Object parent) {
      CounterNode counter = (CounterNode) parent;
      return counter.getChildCount();
    }

    public boolean isLeaf(Object node) {
      CounterNode counter = (CounterNode) node;
      return counter.isLeaf();
    }

    public void addTreeModelListener(TreeModelListener l) {
      treeModelListeners.add(l);
    }

    public void removeTreeModelListener(TreeModelListener l) {
      treeModelListeners.remove(l);
    }

    public void fireNodesRemoved(Object[] path, int[] childIndices, Object[] children) {
      TreeModelEvent e = new TreeModelEvent(this, path, childIndices, children);
      for (Enumeration en = treeModelListeners.elements(); en.hasMoreElements();) {
        ((TreeModelListener) en.nextElement()).treeNodesRemoved(e);
      }

    }

    public Object getChild(Object parent, int index) {
      CounterNode counter = (CounterNode) parent;
      return counter.getChild(index);
    }

    public int getIndexOfChild(Object parent, Object child) {
      CounterNode counter = (CounterNode) parent;
      return counter.getIndexOfChild(child);
    }

    public void valueForPathChanged(TreePath path, Object newValue) {
      throw new RuntimeException("No idea what to do!");

    }
  }
}
