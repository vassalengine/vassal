/*
 *
 * Copyright (c) 2004-2009 by Rodney Kinney, Joel Uckelman
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

import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.datatransfer.StringSelection;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DragSourceMotionListener;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.NewGameIndicator;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;

/**
 * This is the "At-Start Stack" component, which initializes a Map or Board with a specified stack.
 * Because it uses a regular stack, this component is better suited for limited-force-pool collections
 * of counters than a {@link DrawPile}
 *
 */
public class SetupStack extends AbstractConfigurable implements GameComponent, UniqueIdManager.Identifyable {
  private static UniqueIdManager idMgr = new UniqueIdManager("SetupStack");
  public static final String COMMAND_PREFIX = "SETUP_STACK\t";
  protected Point pos = new Point();
  public static final String OWNING_BOARD = "owningBoard";
  public final static String X_POSITION = "x";
  public final static String Y_POSITION = "y";
  protected Map map;
  protected String owningBoardName;
  protected String id;
  public static final String NAME = "name";
  protected static NewGameIndicator indicator;

  protected StackConfigurer stackConfigurer;
  protected JButton configureButton;
  protected String location;
  protected boolean useGridLocation;
  public static final String LOCATION = "location";
  public static final String USE_GRID_LOCATION = "useGridLocation";

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (USE_GRID_LOCATION.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          Board b = getConfigureBoard();
          if (b == null)
            return false;
          else
            return b.getGrid() != null;
        }
      };
    }
    else if (LOCATION.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return isUseGridLocation();
        }
      };
    }
    else if (X_POSITION.equals(name) || Y_POSITION.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return !isUseGridLocation();
        }
      };
    }
    else
      return super.getAttributeVisibility(name);
  }

  // must have a useable board with a grid
  protected boolean isUseGridLocation() {
    if (!useGridLocation) {
      return false;
    }
    Board b = getConfigureBoard();
    return b != null && b.getGrid() != null;
  }

  // only update the position if we're using the location name
  protected void updatePosition() {
    if (isUseGridLocation() && location != null && !location.equals("")) {
      try {
        pos = getConfigureBoard().getGrid().getLocation(location);
      }
      catch (BadCoords e) {
        ErrorDialog.dataError(new BadDataReport(this, "Error.setup_stack_position_error", location,e));
      }
    }
  }

  @Override
  public void validate(Buildable target, ValidationReport report) {
    if (isUseGridLocation()) {
      if (location == null) {
        report.addWarning(getConfigureName() + Resources.getString("SetupStack.null_location"));
      }
      else {
        try {
          getConfigureBoard().getGrid().getLocation(location);
        }
        catch (BadCoords e) {
          String msg = "Bad location name "+location+" in "+getConfigureName();
          if (e.getMessage() != null) {
            msg += ":  "+e.getMessage();
          }
          report.addWarning(msg);
        }
      }
    }

    super.validate(target, report);
  }

  protected void updateLocation() {
    Board b = getConfigureBoard();
    if (b != null) {
      MapGrid g = b.getGrid();
      if (g != null)
        location = g.locationName(pos);
    }
  }

  @Override
  public void setup(boolean gameStarting) {
    if (gameStarting && indicator.isNewGame() && isOwningBoardActive()) {
      Stack s = initializeContents();
      updatePosition();
      Point p = new Point(pos);
      if (owningBoardName != null) {
        Rectangle r = map.getBoardByName(owningBoardName).bounds();
        p.translate(r.x, r.y);
      }
      if (placeNonStackingSeparately()) {
        for (int i=0;i<s.getPieceCount();++i) {
          GamePiece piece = s.getPieceAt(i);
          if (Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK))) {
            s.remove(piece);
            piece.setParent(null);
            map.placeAt(piece,p);
            i--;
          }
        }
      }
      map.placeAt(s, p);
    }
  }

  protected boolean placeNonStackingSeparately() {
    return true;
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString("Editor.StartStack.board"), //$NON-NLS-1$
      Resources.getString("Editor.StartStack.grid"), //$NON-NLS-1$
      Resources.getString("Editor.StartStack.location"), //$NON-NLS-1$
      Resources.getString("Editor.StartStack.position_x"), //$NON-NLS-1$
      Resources.getString("Editor.StartStack.position_y"), //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      OwningBoardPrompt.class,
      Boolean.class,
      String.class,
      Integer.class,
      Integer.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      OWNING_BOARD,
      USE_GRID_LOCATION,
      LOCATION,
      X_POSITION,
      Y_POSITION
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (OWNING_BOARD.equals(key)) {
      return owningBoardName;
    }
    else if (USE_GRID_LOCATION.equals(key)) {
      return Boolean.toString(useGridLocation);
    }
    else if (LOCATION.equals(key)) {
      return location;
    }
    else if (X_POSITION.equals(key)) {
      return String.valueOf(pos.x);
    }
    else if (Y_POSITION.equals(key)) {
      return String.valueOf(pos.y);
    }
    else {
      return null;
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (OWNING_BOARD.equals(key)) {
      if (OwningBoardPrompt.ANY.equals(value)) {
        if (map != null) {
          List<String> selectedBoardNames = map.getBoardPicker().getSelectedBoardNames();
          owningBoardName = selectedBoardNames.isEmpty() ? null : selectedBoardNames.get(0);
        }
        else {
          owningBoardName = null;
        }
      }
      else {
        owningBoardName = (String) value;
      }
      updateConfigureButton();
    }
    else if (USE_GRID_LOCATION.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      useGridLocation = (Boolean) value;
    }
    else if (LOCATION.equals(key)) {
      location = (String) value;
    }
    else if (X_POSITION.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      pos.x = (Integer) value;
    }
    else if (Y_POSITION.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      pos.y = (Integer) value;
    }
  }

  @Override
  public void add(Buildable child) {
    super.add(child);
    updateConfigureButton();
  }

  @Override
  public void addTo(Buildable parent) {
    if (indicator == null) {
      indicator = new NewGameIndicator(COMMAND_PREFIX);
    }
    map = (Map) parent;
    idMgr.add(this);

    GameModule.getGameModule().getGameState().addGameComponent(this);
    setAttributeTranslatable(NAME, false);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{PieceSlot.class};
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SetupStack.htm");
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.StartStack.component_type"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
    idMgr.remove(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  protected boolean isOwningBoardActive() {
    boolean active = false;
    if (owningBoardName == null) {
      active = true;
    }
    else if (map.getBoardByName(owningBoardName) != null) {
      active = true;
    }
    return active;
  }

  protected Stack initializeContents() {
    Stack s = createStack();
    Configurable[] c = getConfigureComponents();
    for (Configurable configurable : c) {
      if (configurable instanceof PieceSlot) {
        PieceSlot slot = (PieceSlot) configurable;
        GamePiece p = slot.getPiece();
        p = PieceCloner.getInstance().clonePiece(p);
        GameModule.getGameModule().getGameState().addPiece(p);
        s.add(p);
      }
    }
    GameModule.getGameModule().getGameState().addPiece(s);
    return s;
  }

  protected Stack createStack() {
    return new Stack();
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public String getId() {
    return id;
  }

//  public static class GridPrompt extends StringEnum {
//    public static final String NONE = "<none>";
//    public static final String ZONE = "(Zone)";
//    public static final String BOARD = "(Board)";
//
//    public GridPrompt() {
//    }
//
//  @Override
//  public String[] getValidValues(AutoConfigurable target) {
//    ArrayList<String> values = new ArrayList<String>();
//    values.add(NONE);
//    if (target instanceof SetupStack) {
//      SetupStack stack = (SetupStack) target;
//      BoardPicker bp = stack.map.getBoardPicker();
//      if (stack.owningBoardName != null) {
//        Board b = bp.getBoard(stack.owningBoardName);
//        MapGrid grid = b.getGrid();
//        if (grid != null) {
//          GridNumbering gn = grid.getGridNumbering();
//          if (gn != null)
//            values.add(BOARD + " " + b.getName());
//          if (grid instanceof ZonedGrid) {
//            ZonedGrid zg = (ZonedGrid) grid;
//            for (Iterator i = zg.getZones(); i.hasNext(); ) {
//              Zone z = (Zone) i.next();
//              if (!z.isUseParentGrid() && z.getGrid() != null && z.getGrid().getGridNumbering() != null)
//                values.add(ZONE + " " + z.getName());
//            }
//          }
//        }
//      }
//    }
//    return values.toArray(new String[values.size()]);
//  }
//  }
//
  public static class OwningBoardPrompt extends StringEnum {
    public static final String ANY = "<any>";

    public OwningBoardPrompt() {
    }

    @Override
    public String[] getValidValues(AutoConfigurable target) {
      String[] values;
      if (target instanceof SetupStack) {
        ArrayList<String> l = new ArrayList<>();
        l.add(ANY);
        Map m = ((SetupStack) target).map;
        if (m != null) {
          l.addAll(Arrays.asList(m.getBoardPicker().getAllowableBoardNames()));
        }
        else {
          for (Map m2 : Map.getMapList()) {
            l.addAll(
              Arrays.asList(m2.getBoardPicker().getAllowableBoardNames()));
          }
        }
        values = l.toArray(new String[0]);
      }
      else {
        values = new String[]{ANY};
      }
      return values;
    }
  }

  /*
   *  GUI Stack Placement Configurer
   */
  protected Configurer xConfig, yConfig, locationConfig;

  @Override
  public Configurer getConfigurer() {
    config = null; // Don't cache the Configurer so that the list of available boards won't go stale
    Configurer c = super.getConfigurer();
    xConfig = ((AutoConfigurer) c).getConfigurer(X_POSITION);
    yConfig = ((AutoConfigurer) c).getConfigurer(Y_POSITION);
    locationConfig = ((AutoConfigurer) c).getConfigurer(LOCATION);
    updateConfigureButton();
    ((Container) c.getControls()).add(configureButton);

    return c;
  }

  protected void updateConfigureButton() {
    if (configureButton == null) {
      configureButton = new JButton("Reposition Stack");
      configureButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          configureStack();
        }
      });
    }
    configureButton.setEnabled(getConfigureBoard() != null && buildComponents.size() > 0);
  }

  protected void configureStack() {
    stackConfigurer = new StackConfigurer(this);
    stackConfigurer.init();
    stackConfigurer.setVisible(true);
  }

  protected PieceSlot getTopPiece() {
    Iterator<PieceSlot> i =
      getAllDescendantComponentsOf(PieceSlot.class).iterator();
    return i.hasNext() ? i.next() : null;
  }

  /*
   * Return a board to configure the stack on.
   */
  protected Board getConfigureBoard() {

    Board board = null;

    if (map != null && !OwningBoardPrompt.ANY.equals(owningBoardName)) {
      board = map.getBoardPicker().getBoard(owningBoardName);
    }

    if (board == null && map != null) {
      String[] allBoards = map.getBoardPicker().getAllowableBoardNames();
      if (allBoards.length > 0) {
        board = map.getBoardPicker().getBoard(allBoards[0]);
      }
    }

    return board;
  }

  protected static final Dimension DEFAULT_SIZE = new Dimension(800, 600);
  protected static final int DELTA = 1;
  protected static final int FAST = 10;
  protected static final int FASTER = 5;
  protected static final int DEFAULT_DUMMY_SIZE = 50;

  public class StackConfigurer extends JFrame implements ActionListener, KeyListener, MouseListener {

    private static final long serialVersionUID = 1L;

    protected Board board;
    protected View view;
    protected JScrollPane scroll;
    protected SetupStack myStack;
    protected PieceSlot mySlot;
    protected GamePiece myPiece;
    protected Point savePosition;
    protected Dimension dummySize;
    protected BufferedImage dummyImage;

    public StackConfigurer(SetupStack stack) {
      super("Adjust At-Start Stack");
      setJMenuBar(MenuManager.getInstance().getMenuBarFor(this));

      myStack = stack;
      mySlot = stack.getTopPiece();
      if (mySlot != null) {
        myPiece = mySlot.getPiece();
      }

      myStack.updatePosition();
      savePosition = new Point(myStack.pos);

      if (stack instanceof DrawPile) {
        dummySize = new Dimension(((DrawPile) stack).getSize());
      }
      else {
        dummySize = new Dimension(DEFAULT_DUMMY_SIZE, DEFAULT_DUMMY_SIZE);
      }

      addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          cancel();
        }
      });
    }

    // Main Entry Point
    protected void init() {

      board = getConfigureBoard();

      view = new View(board, myStack);

      view.addKeyListener(this);
      view.addMouseListener(this);
      view.setFocusable(true);


      scroll =
          new AdjustableSpeedScrollPane(
              view,
              JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
              JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

      scroll.setPreferredSize(DEFAULT_SIZE);

      add(scroll, BorderLayout.CENTER);

      Box textPanel = Box.createVerticalBox();
      textPanel.add(new JLabel("Arrow Keys - Move Stack"));
      textPanel.add(new JLabel("Ctrl/Shift Keys - Move Stack Faster  "));

      Box displayPanel = Box.createHorizontalBox();

      Box buttonPanel = Box.createHorizontalBox();
      JButton snapButton = new JButton("Snap to grid");
      snapButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          snap();
          view.grabFocus();
        }
      });
      buttonPanel.add(snapButton);

      JButton okButton = new JButton("Ok");
      okButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          StackConfigurer.this.setVisible(false);
          // Update the Component configurer to reflect the change
          xConfig.setValue(String.valueOf(myStack.pos.x));
          yConfig.setValue(String.valueOf(myStack.pos.y));
          if (locationConfig != null) { // DrawPile's do not have a location
            updateLocation();
            locationConfig.setValue(location);
          }
        }
      });
      JPanel okPanel = new JPanel();
      okPanel.add(okButton);

      JButton canButton = new JButton("Cancel");
      canButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          cancel();
          StackConfigurer.this.setVisible(false);
        }
      });
      okPanel.add(canButton);

      Box controlPanel = Box.createHorizontalBox();
      controlPanel.add(textPanel);
      controlPanel.add(displayPanel);
      controlPanel.add(buttonPanel);

      Box mainPanel = Box.createVerticalBox();
      mainPanel.add(controlPanel);
      mainPanel.add(okPanel);

      add(mainPanel, BorderLayout.SOUTH);

      scroll.revalidate();
      updateDisplay();
      pack();
      repaint();
    }

    protected void cancel() {
      myStack.pos.x = savePosition.x;
      myStack.pos.y = savePosition.y;
    }

    public void updateDisplay() {
      if (!view.getVisibleRect().contains(myStack.pos)) {
        view.center(new Point(myStack.pos.x, myStack.pos.y));
      }
    }

    protected void snap() {
      MapGrid grid = board.getGrid();
      if (grid != null) {
        Point snapTo = grid.snapTo(pos);
        pos.x = snapTo.x;
        pos.y = snapTo.y;
        updateDisplay();
        repaint();
      }
    }

    public JScrollPane getScroll() {
      return scroll;
    }

    /*
     * If the piece to be displayed does not have an Image, then we
     * need to supply a dummy one.
     */
    public BufferedImage getDummyImage() {
      if (dummyImage == null) {
        dummyImage = ImageUtils.createCompatibleTranslucentImage(
          dummySize.width*2, dummySize.height*2);
        final Graphics2D g = dummyImage.createGraphics();
        g.setColor(Color.white);
        g.fillRect(0, 0, dummySize.width, dummySize.height);
        g.setColor(Color.black);
        g.drawRect(0, 0, dummySize.width, dummySize.height);
        g.dispose();
      }
      return dummyImage;
    }

    public void drawDummyImage(Graphics g, int x, int y) {
      drawDummyImage(g, x-dummySize.width/2, y-dummySize.height/2, null, 1.0);
    }

    public void drawDummyImage(Graphics g, int x, int y, Component obs, double zoom) {
      final Graphics2D g2d = (Graphics2D) g;
      final AffineTransform orig_t = g2d.getTransform();
      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final AffineTransform scaled_t = new AffineTransform(orig_t);
      scaled_t.scale(os_scale, os_scale);
      g2d.setTransform(scaled_t);

      x /= os_scale;
      y /= os_scale;

      g.drawImage(getDummyImage(), x, y, obs);

      g2d.setTransform(orig_t);
    }

    public void drawImage(Graphics g, int x, int y, Component obs, double zoom) {
      Rectangle r = myPiece == null ? null : myPiece.boundingBox();
      if (r == null || r.width == 0 || r.height == 0) {
        drawDummyImage(g, x, y);
      }
      else {
        myPiece.draw(g, x, y, obs, zoom);
      }
    }

    public Rectangle getPieceBoundingBox() {
      Rectangle r = myPiece == null ? new Rectangle() : myPiece.getShape().getBounds();
      if (r == null || r.width == 0 || r.height == 0) {
        r.x = 0 - dummySize.width/2;
        r.y = 0 - dummySize.height/2;
        r.width = dummySize.width;
        r.height = dummySize.height;
      }
      return r;
    }

    @Override
    public void actionPerformed(ActionEvent e) {

    }

    @Override
    public void keyPressed(KeyEvent e) {

      switch (e.getKeyCode()) {
      case KeyEvent.VK_UP:
        adjustY(-1, e);
        break;
      case KeyEvent.VK_DOWN:
        adjustY(1, e);
        break;
      case KeyEvent.VK_LEFT:
        adjustX(-1, e);
        break;
      case KeyEvent.VK_RIGHT:
        adjustX(1, e);
        break;
      default :
        if (myPiece != null) {
          myPiece.keyEvent(KeyStroke.getKeyStrokeForEvent(e));
        }
        break;
      }
      updateDisplay();
      repaint();
      e.consume();
    }

    protected void adjustX(int direction, KeyEvent e) {
      int delta = direction * DELTA;
      if (e.isShiftDown()) {
        delta *= FAST;
      }
      if (e.isControlDown()) {
        delta *= FASTER;
      }
      int newX = myStack.pos.x + delta;
      if (newX < 0) newX = 0;
      if (newX >= board.getSize().getWidth()) newX = (int) board.getSize().getWidth() - 1;
      myStack.pos.x = newX;

    }

    protected void adjustY(int direction, KeyEvent e) {
      int delta = direction * DELTA;
      if (e.isShiftDown()) {
        delta *= FAST;
      }
      if (e.isControlDown()) {
        delta *= FASTER;
      }
      int newY = myStack.pos.y + delta;
      if (newY < 0) newY = 0;
      if (newY >= board.getSize().getHeight()) newY = (int) board.getSize().getHeight() - 1;
      myStack.pos.y = newY;
    }

    @Override
    public void keyReleased(KeyEvent e) {
    }

    @Override
    public void keyTyped(KeyEvent e) {
    }

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    protected void maybePopup(MouseEvent e) {
      if (!e.isPopupTrigger() || myPiece == null) {
        return;
      }

      Rectangle r = getPieceBoundingBox();
      r.translate(pos.x, pos.y);
      if (r.contains(e.getPoint())) {
        JPopupMenu popup = MenuDisplayer.createPopup(myPiece);
        popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
          @Override
          public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
            view.repaint();
          }
          @Override
          public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
            view.repaint();
          }
          @Override
          public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
          }
        });
        if (view.isShowing()) {
          popup.show(view, e.getX(), e.getY());
        }
      }
    }

    @Override
    public void mousePressed(MouseEvent e) {
      maybePopup(e);
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      maybePopup(e);
    }
  }

  @Override
  public ComponentI18nData getI18nData() {
    ComponentI18nData myI18nData = super.getI18nData();
    myI18nData.setAttributeTranslatable(LOCATION, false);
    return myI18nData;
  }

// FIXME: check for duplication with PieceMover

  public static class View extends JPanel implements DropTargetListener, DragGestureListener, DragSourceListener, DragSourceMotionListener {

    private static final long serialVersionUID = 1L;
    protected static final int CURSOR_ALPHA = 127;
    protected static final int EXTRA_BORDER = 4;
    protected Board myBoard;
    protected MapGrid myGrid;
    protected SetupStack myStack;
    protected GamePiece myPiece;
    protected PieceSlot slot;
    protected DragSource ds = DragSource.getDefaultDragSource();
    protected boolean isDragging = false;
    protected JLabel dragCursor;
    protected JLayeredPane drawWin;
    protected Point drawOffset = new Point();
    protected Rectangle boundingBox;
    protected int currentPieceOffsetX;
    protected int currentPieceOffsetY;
    protected int originalPieceOffsetX;
    protected int originalPieceOffsetY;
    protected Point lastDragLocation = new Point();

    public View(Board b, SetupStack s) {
      myBoard = b;
      myGrid = b.getGrid();
      myStack = s;
      slot = myStack.getTopPiece();
      if (slot != null) {
        myPiece = slot.getPiece();
      }
      new DropTarget(this, DnDConstants.ACTION_MOVE, this);
      ds.createDefaultDragGestureRecognizer(this,
        DnDConstants.ACTION_MOVE, this);
      setFocusTraversalKeysEnabled(false);
    }

    @Override
    public void paint(Graphics g) {
      final Graphics2D g2d = (Graphics2D) g;
      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

      final AffineTransform orig_t = g2d.getTransform();
      g2d.setTransform(SwingUtils.descaleTransform(orig_t));

      myBoard.draw(g, 0, 0, os_scale, this);
      if (myGrid != null) {
        final Rectangle bounds = new Rectangle(new Point(), myBoard.bounds().getSize());
        bounds.width *= os_scale;
        bounds.height *= os_scale;
        myGrid.draw(g, bounds, bounds, os_scale, false);
      }
      int x = (int)(myStack.pos.x * os_scale);
      int y = (int)(myStack.pos.y * os_scale);
      myStack.stackConfigurer.drawImage(g, x, y, this, os_scale);

      g2d.setTransform(orig_t);
    }

    @Override
    public void update(Graphics g) {
      // To avoid flicker, don't clear the display first *
      paint(g);
    }

    @Override
    public Dimension getPreferredSize() {
      return new Dimension(
          myBoard.bounds().width,
          myBoard.bounds().height);
    }

    public void center(Point p) {
      Rectangle r = this.getVisibleRect();
      if (r.width == 0) {
        r.width = DEFAULT_SIZE.width;
        r.height = DEFAULT_SIZE.height;
      }
      int x = p.x-r.width/2;
      int y = p.y-r.height/2;
      if (x < 0) x = 0;
      if (y < 0) y = 0;
      scrollRectToVisible(new Rectangle(x, y, r.width, r.height));
    }

    @Override
    public void dragEnter(DropTargetDragEvent arg0) {
    }

    @Override
    public void dragOver(DropTargetDragEvent e) {
      scrollAtEdge(e.getLocation(), 15);
    }

    public void scrollAtEdge(Point evtPt, int dist) {
      JScrollPane scroll = myStack.stackConfigurer.getScroll();

      Point p = new Point(evtPt.x - scroll.getViewport().getViewPosition().x,
          evtPt.y - scroll.getViewport().getViewPosition().y);
      int dx = 0, dy = 0;
      if (p.x < dist && p.x >= 0)
        dx = -1;
      if (p.x >= scroll.getViewport().getSize().width - dist
          && p.x < scroll.getViewport().getSize().width)
        dx = 1;
      if (p.y < dist && p.y >= 0)
        dy = -1;
      if (p.y >= scroll.getViewport().getSize().height - dist
          && p.y < scroll.getViewport().getSize().height)
        dy = 1;

      if (dx != 0 || dy != 0) {
        Rectangle r = new Rectangle(scroll.getViewport().getViewRect());
        r.translate(2 * dist * dx, 2 * dist * dy);
        r = r.intersection(new Rectangle(new Point(0, 0), getPreferredSize()));
        scrollRectToVisible(r);
      }
    }

    @Override
    public void dropActionChanged(DropTargetDragEvent arg0) {
    }

    @Override
    public void drop(DropTargetDropEvent event) {
      removeDragCursor();
      Point pos = event.getLocation();
      pos.translate(currentPieceOffsetX, currentPieceOffsetY);
      myStack.pos.x = pos.x;
      myStack.pos.y = pos.y;
      myStack.stackConfigurer.updateDisplay();
      repaint();
    }

    @Override
    public void dragExit(DropTargetEvent arg0) {
    }

    @Override
    public void dragEnter(DragSourceDragEvent arg0) {
    }

    @Override
    public void dragOver(DragSourceDragEvent arg0) {
    }

    @Override
    public void dropActionChanged(DragSourceDragEvent arg0) {
    }

    @Override
    public void dragDropEnd(DragSourceDropEvent arg0) {
      removeDragCursor();
    }

    @Override
    public void dragExit(DragSourceEvent arg0) {
    }

    @Override
    public void dragGestureRecognized(DragGestureEvent dge) {
      if (!SwingUtils.isDragTrigger(dge)) {
        return;
      }

      Point mousePosition = dge.getDragOrigin();
      Point piecePosition = new Point(myStack.pos);

      // Check drag starts inside piece
      Rectangle r = myStack.stackConfigurer.getPieceBoundingBox();
      r.translate(piecePosition.x, piecePosition.y);
      if (!r.contains(mousePosition)) {
        return;
      }

      originalPieceOffsetX = piecePosition.x - mousePosition.x;
      originalPieceOffsetY = piecePosition.y - mousePosition.y;

      drawWin = null;

      makeDragCursor();
      setDragCursor();

      SwingUtilities.convertPointToScreen(mousePosition, drawWin);
      moveDragCursor(mousePosition.x, mousePosition.y);

      // begin dragging
      try {
        dge.startDrag(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR),
                      new StringSelection(""), this); // DEBUG
        dge.getDragSource().addDragSourceMotionListener(this);
      }
      catch (InvalidDnDOperationException e) {
        ErrorDialog.bug(e);
      }
    }

    protected void setDragCursor() {
      JRootPane rootWin = SwingUtilities.getRootPane(this);
      if (rootWin != null) {
        // remove cursor from old window
        if (dragCursor.getParent() != null) {
          dragCursor.getParent().remove(dragCursor);
        }
        drawWin = rootWin.getLayeredPane();

        calcDrawOffset();
        dragCursor.setVisible(true);
        drawWin.add(dragCursor, JLayeredPane.DRAG_LAYER);
      }
    }

    /** Moves the drag cursor on the current draw window */
    protected void moveDragCursor(int dragX, int dragY) {
      if (drawWin != null) {
        dragCursor.setLocation(dragX - drawOffset.x, dragY - drawOffset.y);
      }
    }

    private void removeDragCursor() {
      if (drawWin != null) {
        if (dragCursor != null) {
          dragCursor.setVisible(false);
          drawWin.remove(dragCursor);
        }
        drawWin = null;
      }
    }

    /** calculates the offset between cursor dragCursor positions */
    private void calcDrawOffset() {
      if (drawWin != null) {
        // drawOffset is the offset between the mouse location during a drag
        // and the upper-left corner of the cursor
        // accounts for difference betwen event point (screen coords)
        // and Layered Pane position, boundingBox and off-center drag
        drawOffset.x = -boundingBox.x - currentPieceOffsetX + EXTRA_BORDER;
        drawOffset.y = -boundingBox.y - currentPieceOffsetY + EXTRA_BORDER;
        SwingUtilities.convertPointToScreen(drawOffset, drawWin);
      }
    }

    private BufferedImage featherDragImage(BufferedImage src,
                                         int w, int h, int b) {
// FIXME: duplicated from PieceMover!
      final BufferedImage dst =
        ImageUtils.createCompatibleTranslucentImage(w, h);

      final Graphics2D g = dst.createGraphics();
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

      // paint the rectangle occupied by the piece at specified alpha
      g.setColor(new Color(0xff, 0xff, 0xff, CURSOR_ALPHA));
      g.fillRect(0, 0, w, h);

      // feather outwards
      for (int f = 0; f < b; ++f) {
        final int alpha = CURSOR_ALPHA * (f + 1) / b;
        g.setColor(new Color(0xff, 0xff, 0xff, alpha));
        g.drawRect(f, f, w-2*f, h-2*f);
      }

      // paint in the source image
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_IN));
      g.drawImage(src, 0, 0, null);
      g.dispose();

      return dst;
    }

    private void makeDragCursor() {
      //double zoom = 1.0;
      // create the cursor if necessary
      if (dragCursor == null) {
        dragCursor = new JLabel();
        dragCursor.setVisible(false);
      }

      //dragCursorZoom = zoom;
      currentPieceOffsetX = originalPieceOffsetX;
      currentPieceOffsetY = originalPieceOffsetY;

      // Record sizing info and resize our cursor
      boundingBox =  myStack.stackConfigurer.getPieceBoundingBox();
      calcDrawOffset();

      final int w = boundingBox.width + EXTRA_BORDER * 2;
      final int h = boundingBox.height + EXTRA_BORDER * 2;

      BufferedImage cursorImage =
        ImageUtils.createCompatibleTranslucentImage(w, h);
      final Graphics2D g = cursorImage.createGraphics();

      myStack.stackConfigurer.drawImage(
        g,
        EXTRA_BORDER - boundingBox.x,
        EXTRA_BORDER - boundingBox.y, dragCursor, 1.0
      );

      g.dispose();

      dragCursor.setSize(w, h);

      cursorImage = featherDragImage(cursorImage, w, h, EXTRA_BORDER);

      // store the bitmap in the cursor
      dragCursor.setIcon(new ImageIcon(cursorImage));
    }

    @Override
    public void dragMouseMoved(DragSourceDragEvent event) {
      if (!event.getLocation().equals(lastDragLocation)) {
        lastDragLocation = event.getLocation();
        moveDragCursor(event.getX(), event.getY());
        if (dragCursor != null && !dragCursor.isVisible()) {
          dragCursor.setVisible(true);
        }
      }
    }
  }
}
