/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Brent Easton, Rodney Kinney
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
package VASSAL.build.module.map.boardPicker.board;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Stroke;
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
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.Configurer;
import VASSAL.configure.EditPropertiesAction;
import VASSAL.configure.PropertiesWindow;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;

public class RegionGrid extends AbstractConfigurable implements MapGrid, ConfigureTree.Mutable {
  private static final long serialVersionUID = 1L;

  // AreaList is the table of Map areas
  // pointList is a cross-reference of points to Area names

  protected Map<Point,Region> regionList = new HashMap<Point,Region>();
  protected GridContainer container;
  protected boolean visible = false;
  protected static boolean inConfig = false;
  protected int fontSize = 9; // Size square to display when configuring
  protected boolean snapTo = true;
  protected Config regionConfigurer;

  protected GridNumbering gridNumbering;
  RegionGrid me = this;

  public void addRegion(Region a) {
    regionList.put(a.getOrigin(), a);
    if (inConfig) {
        regionConfigurer.view.repaint();
    }
  }

  public void removeRegion(Region a) {
    regionList.remove(a.getOrigin());
  }

  public void removeAllRegions() {
    regionList.clear();
    buildComponents.clear();
  }
  
  public GridNumbering getGridNumbering() {
    return gridNumbering;
  }

  public void setGridNumbering(GridNumbering gridNumbering) {
    this.gridNumbering = gridNumbering;
  }

  public int getFontSize() {
    return fontSize;
  }

  public static final String SNAPTO = "snapto";
  public static final String VISIBLE = "visible";
  public static final String FONT_SIZE = "fontsize";

  public String[] getAttributeNames() {
    return new String[]{SNAPTO, VISIBLE, FONT_SIZE};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Snap to defined point?",
      "Draw region names?",
      "Font Size:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Boolean.class, Boolean.class, Integer.class};
  }

  public Configurer getConfigurer() {
    boolean buttonExists = config != null;
    Configurer c = super.getConfigurer();
    if (!buttonExists) {
      JButton b = new JButton("Define Regions");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          configureRegions();
        }
      });
      ((Container) c.getControls()).add(b);
    }
    return c;
  }

  public void addTo(Buildable b) {
    container = (GridContainer) b;
    container.setGrid(this);
  }

  public void removeFrom(Buildable b) {
    container.removeGrid(this);
    container = null;
  }

  public static String getConfigureTypeName() {
    return "Irregular Grid";
  }

  public String getConfigureName() {
    return null;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("IrregularGrid.htm");
  }

  public String getAttributeValueString(String key) {
    if (VISIBLE.equals(key)) {
      return "" + visible;
    }
    else if (FONT_SIZE.equals(key)) {
      return "" + fontSize;
    }
	else if (SNAPTO.equals(key)) {
	  return "" + snapTo;
	}
    return null;
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (FONT_SIZE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return visible;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public void setAttribute(String key, Object val) {
    if (val == null)
      return;
    if (VISIBLE.equals(key)) {

      if (val instanceof Boolean) {
        visible = ((Boolean) val).booleanValue();
      }
      else if (val instanceof String) {
        visible = "true".equals(val);
      }
    }
    else if (FONT_SIZE.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      fontSize = ((Integer) val).intValue();
    }
	else if (SNAPTO.equals(key)) {
		if (val instanceof Boolean) {
			snapTo = ((Boolean) val).booleanValue();
		} else if (val instanceof String) {
			snapTo = "true".equals(val);
		}
	}
  }

  public void configureRegions() {
  	inConfig = true;
    for (Region r : regionList.values()) {
      r.setSelected(false);
    }
  	regionConfigurer = new Config(this);
  	regionConfigurer.setVisible(true);
  }

  // Force Regions to be drawn when configuring
  public boolean isVisible() {
    return (visible || inConfig);
  }

  public void setVisible(boolean b) {
    visible = b;
  }

  public Board getBoard() {
    return container.getBoard();
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{Region.class};
  }

  public Point getLocation(String name) throws MapGrid.BadCoords {
    throw new MapGrid.BadCoords("No naming scheme specified");
  }

  public int range(Point p1, Point p2) {
    return (int)Math.round(p1.distance(p2));
  }

  //
  // Locate nearest point
  //
  public Point snapTo(Point p) {

    //
    // Need at least one point to snap to and snapping needs to be pn.
    //
    if (!snapTo || regionList.isEmpty()) {
      return p;
    }

    return doSnap(p);
  }

  public boolean isLocationRestricted(Point p) {
    return snapTo;
  }

  //
  // Internal routine to find closest point for region name reporting
  //
  protected Point doSnap(Point p) {
	  double distSq, minDistSq = 999999999;
  	Point snapPoint = p;

    // Iterate over each grid point and determine the closest.
    for (Point checkPoint : regionList.keySet()) {
      distSq =
          (p.x - checkPoint.x) * (p.x - checkPoint.x)
          + (p.y - checkPoint.y) * (p.y - checkPoint.y);
      if (distSq < minDistSq) {
        minDistSq = distSq;
        snapPoint = checkPoint;
      }
    }

    return new Point(snapPoint);
  }

  public String locationName(Point p) {

  	Point checkPoint;

  	if (regionList.isEmpty()) {
  		return null;
  	}

  	//
  	// If snap-to is turned off, then p has not been snapped to a grid point yet
  	//
  	if (snapTo) {
  		checkPoint = p;
  	}
    else {
    	checkPoint = doSnap(p);
    }

    Region region = regionList.get(checkPoint);
    return region != null ? region.getName() : null;

  }
  
  public String localizedLocationName(Point p) {

    Point checkPoint;

    if (regionList.isEmpty()) {
      return null;
    }

    //
    // If snap-to is turned off, then p has not been snapped to a grid point yet
    //
    if (snapTo) {
      checkPoint = p;
    }
    else {
      checkPoint = doSnap(p);
    }

    Region region = regionList.get(checkPoint);
    return region != null ? region.getLocalizedName() : null;

  }

  /**
   * Return Region selected by Point
   */
  public Region getRegion(Point p) {
    for (Region checkRegion : regionList.values()) {
      if (checkRegion.contains(p))
        return checkRegion;
    }
    return null;
  }
  
  /**
   * Return Region by Name
   */
  public Region findRegion(String name) {
    for (Region checkRegion : regionList.values()) {
      if (checkRegion.getConfigureName().equals(name)) {
        return checkRegion;
      }
    }
    return null;
  }

  //
  // Get each region to draw labels and dots
  //
  public void draw(
      Graphics g,
      Rectangle bounds,
      Rectangle visibleRect,
      double scale,
      boolean reversed) {
      if (visible) {
        forceDraw(g, bounds, visibleRect, scale, reversed);
      }
  }
  public void forceDraw(
      Graphics g,
      Rectangle bounds,
      Rectangle visibleRect,
      double scale,
      boolean reversed) {

    for (Region r : regionList.values()) {
      r.draw(g, bounds, visibleRect, scale, reversed);
    }

    return;
  }

  public void unSelectAll() {
    for (Region r : regionList.values()) {
      unSelect(r);
    }
  }

  public void unSelect(Region r) {
    r.setSelected(false);
  }

  public static class Config extends JFrame implements MouseListener, MouseMotionListener, ActionListener, KeyListener {
    private static final long serialVersionUID = 1L;

    protected RegionGrid grid;
    protected Board board;

    protected JPanel view;
    protected JScrollPane scroll;
    protected JPopupMenu myPopup;

    protected List<Region> selectedRegions = new ArrayList<Region>();
    protected Region lastClickedRegion = null;
    protected Point lastClick;
    protected Rectangle selectionRect = null;
    protected Point anchor;
    
    protected List<Region> saveRegions;
    
    protected boolean dirty = false;

    public Config(RegionGrid grid) {
      super("Regions for " + grid.container.getBoard().getName());
      board = grid.container.getBoard();
      this.grid = grid;
      initComponents();
      save();
    }

    // Main Entry Point
    protected void initComponents() {
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          doCancel();
        }
      });

      view = new Config.View(board, grid, this);

      view.addMouseListener(this);
      view.addMouseMotionListener(this);
      view.addKeyListener(this);
      view.setFocusable(true);

      scroll =
          new AdjustableSpeedScrollPane(
              view,
              JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
              JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

      scroll.setPreferredSize(new Dimension(800, 600));

      add(scroll, BorderLayout.CENTER);

      Box bottomPanel = Box.createVerticalBox();
      JPanel buttonPanel = new JPanel();
      
      JButton okButton = new JButton(Resources.getString(Resources.OK)); //$NON-NLS-1$
      okButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          close();
        }
      });
      buttonPanel.add(okButton);
      
      JButton canButton = new JButton(Resources.getString(Resources.CANCEL)); //$NON-NLS-1$
      canButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          doCancel();
        }
      });
      buttonPanel.add(canButton);
      
      JLabel mess = new JLabel("Drag and Drop selected regions to move, or use ctrl-arrows/ctrl-shift-arrows for precise movement.");
      mess.setAlignmentY(CENTER_ALIGNMENT);
      bottomPanel.add(mess);
      bottomPanel.add(buttonPanel);
      
      add(bottomPanel, BorderLayout.SOUTH);

      board.fixImage();
      scroll.revalidate();
      pack();
      repaint();
    }
    
    protected void setDirty(boolean b) {
      dirty = b;
    }

    protected void doCancel() {
      if (dirty) {
        if (JOptionPane.YES_OPTION == 
          JOptionPane.showConfirmDialog(this, 
              "Changes made. Are you sure you want to Cancel those changes?",
              "", JOptionPane.YES_NO_OPTION)) {
            restore();
            close();
         }
      }
      else {
        close();
      }
    }
    
    protected void close() {
      inConfig = false;
      Config.this.setVisible(false);
    }
    
    public void init() {
      for (Region r : selectedRegions) {
        r.setSelected(false);
      }
    }
    
    /*
     * Clone a list of the existing regions in case we have to restore
     * after changes
     */
    public void save() {
      saveRegions = new ArrayList<Region>(grid.regionList.size());
      for (Region r : grid.regionList.values()) {
        saveRegions.add(new Region(r));
      }      
    }
    
    /*
     * Restore the original list of regions. Remove all existing regions,
     * then add the originals back in
     */
    public void restore() {
      grid.removeAllRegions();
      for (Region r : saveRegions) {
        r.addTo(grid);
        grid.add(r);
      }
    }
    
    /*
     * Scrolls the map in the containing JScrollPane
     * @param dx number of pixels to scroll horizontally
     * @param dy number of pixels to scroll vertically
     */
    protected void doScroll(int dx, int dy) {
      Rectangle r = new Rectangle(scroll.getViewport().getViewRect());
      r.translate(dx, dy);
      r =
          r.intersection(
              new Rectangle(new Point(0, 0), view.getPreferredSize()));
      view.scrollRectToVisible(r);
    }

    /**
     * Scoll map so that the argument point is at least a certain distance from the visible edge
     * @param evtPt
     */
    protected void scrollAtEdge(Point evtPt, int dist) {
      Point p =
          new Point(
              evtPt.x - scroll.getViewport().getViewPosition().x,
              evtPt.y - scroll.getViewport().getViewPosition().y);
      int dx = 0, dy = 0;
      Dimension viewSize = scroll.getViewport().getSize();
      if (p.x < dist && p.x >= 0)
        dx = -1;
      if (p.x >= viewSize.width - dist
          && p.x < viewSize.width)
        dx = 1;
      if (p.y < dist && p.y >= 0)
        dy = -1;
      if (p.y >= viewSize.height - dist
          && p.y < viewSize.height)
        dy = 1;

      if (dx != 0 || dy != 0) {
        doScroll(2 * dist * dx, 2 * dist * dy);
      }
    }

    /* ------------------------------------------------------------------
     * The scrollpane client
     */
    public static class View extends JPanel implements DropTargetListener, DragGestureListener, DragSourceListener, DragSourceMotionListener {
      private static final long serialVersionUID = 1L;

      protected Board myBoard;
      protected RegionGrid grid;
      protected Config config;
      
      protected DragSource ds = DragSource.getDefaultDragSource();
      protected boolean isDragging = false;
      protected JLabel dragCursor; 
      protected JLayeredPane drawWin;
      protected Point dragStart;
      protected Point lastDragLocation = new Point();
      protected Point drawOffset = new Point();
      protected Rectangle boundingBox;
      protected int currentPieceOffsetX;
      protected int currentPieceOffsetY;
      protected int originalPieceOffsetX;
      protected int originalPieceOffsetY;

      public View(Board b, RegionGrid grid, Config config) {
        myBoard = b;
        this.grid = grid;
        this.config = config;
        new DropTarget(this, DnDConstants.ACTION_MOVE, this);
        ds.createDefaultDragGestureRecognizer(this,
          DnDConstants.ACTION_MOVE, this);
        setFocusTraversalKeysEnabled(false);
      }

      public void paint(Graphics g) {
        Rectangle b = getVisibleRect();
        g.clearRect(b.x, b.y, b.width, b.height);
        myBoard.draw(g, 0, 0, 1.0, this);
        Rectangle bounds = new Rectangle(new Point(),myBoard.bounds().getSize());
        grid.forceDraw(g,bounds,bounds,1.0,false);
        Rectangle selection = config.getSelectionRect();
        if (selection != null) {
          Graphics2D g2d = (Graphics2D) g;
          Stroke str = g2d.getStroke();
          g2d.setStroke(new BasicStroke(2.0f));
          g2d.setColor(Color.RED);
          g2d.drawRect(selection.x, selection.y, selection.width, selection.height);
          g2d.setStroke(str);
        }
      }

      public void update(Graphics g) {
        // To avoid flicker, don't clear the display first *
        paint(g);
      }

      public Dimension getPreferredSize() {
        return new Dimension(
            myBoard.bounds().width,
            myBoard.bounds().height);
      }

      public void dragEnter(DropTargetDragEvent arg0) {
      }

      public void dragExit(DropTargetEvent arg0) {
      }

      public void dragOver(DropTargetDragEvent arg0) {
      }

      public void drop(DropTargetDropEvent event) {
        removeDragCursor();
        Point dragEnd = event.getLocation();
        int x = dragEnd.x - dragStart.x;
        int y = dragEnd.y - dragStart.y;
        
        for (Region r : config.selectedRegions) {
          r.move(x, y, this);
          config.setDirty(true);
        }
        repaint();
      }

      public void dropActionChanged(DropTargetDragEvent arg0) {
      }

      public void dragGestureRecognized(DragGestureEvent dge) {
        
        Point mousePosition = dge.getDragOrigin();
        dragStart = new Point(mousePosition);
        Region r = grid.getRegion(mousePosition);
        if (r == null) {
          return;
        }
        
        Point piecePosition = new Point(r.getOrigin());
        
        originalPieceOffsetX = piecePosition.x - mousePosition.x;
        originalPieceOffsetY = piecePosition.y - mousePosition.y;
              
        drawWin = null;
        
        makeDragCursor();
        setDragCursor();
        
        SwingUtilities.convertPointToScreen(drawOffset, drawWin);   
        SwingUtilities.convertPointToScreen(mousePosition, drawWin);
        moveDragCursor(mousePosition.x, mousePosition.y);
        
        // begin dragging
        try {
          dge.startDrag(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR), new StringSelection(""), this); // DEBUG
          dge.getDragSource().addDragSourceMotionListener(this);
        }
        catch (InvalidDnDOperationException e) {
          e.printStackTrace();
        }
      }

      public void dragDropEnd(DragSourceDropEvent arg0) {
        removeDragCursor();
        return;
      }

      public void dragEnter(DragSourceDragEvent arg0) {
      }

      public void dragExit(DragSourceEvent arg0) {
      }

      public void dragOver(DragSourceDragEvent arg0) {
      }

      public void dropActionChanged(DragSourceDragEvent arg0) {
      }

      public void dragMouseMoved(DragSourceDragEvent event) {
        if (!event.getLocation().equals(lastDragLocation)) {
          lastDragLocation = event.getLocation();
          moveDragCursor(event.getX(), event.getY());
          if (dragCursor != null && !dragCursor.isVisible()) {
            dragCursor.setVisible(true);
          }
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
      
      /** Moves the drag cursor on the current draw window */
      protected void moveDragCursor(int dragX, int dragY) {
        if (drawWin != null) {
          dragCursor.setLocation(dragX - drawOffset.x, dragY - drawOffset.y);
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

          dragCursor.setVisible(true);
          drawWin.add(dragCursor, JLayeredPane.DRAG_LAYER);
        }
      }
      
      private void makeDragCursor() {
        // create the cursor if necessary
        if (dragCursor == null) {
          dragCursor = new JLabel();
          dragCursor.setVisible(false);
        }

        currentPieceOffsetX = originalPieceOffsetX;
        currentPieceOffsetY = originalPieceOffsetY;

        // Record sizing info and resize our cursor
        boundingBox = config.getSelectedBox();        
        drawOffset.move(dragStart.x - boundingBox.x, dragStart.y - boundingBox.y);
        
        BufferedImage cursorImage = new BufferedImage(boundingBox.width, boundingBox.height, BufferedImage.TYPE_4BYTE_ABGR);
        Graphics2D g = cursorImage.createGraphics();
 
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
         // Draw each region into the drag cursor in the correct place
        for (Region r : config.selectedRegions) {
          int x = -boundingBox.x*2;
          int y = -boundingBox.y*2;
          r.draw(g, boundingBox, getVisibleRect(), 1.0f, false, x, y);
        }
  
        g.dispose();       
 
        dragCursor.setSize(boundingBox.width, boundingBox.height);

        // store the bitmap in the cursor
        dragCursor.setIcon(new ImageIcon(cursorImage));
        
      }
    }
    /* ------------------------------------------------------------------
     * End View
     */
    
    /*
     * Mouse Listeners
     */

    // Mouse clicked, see if it is on a Region Point
    public void mouseClicked(MouseEvent e) {
      Point p = e.getPoint();
      lastClick = p;

      if (e.isMetaDown()) { // Right click...menu
        doPopupMenu(e);
      }
      else if (lastClickedRegion != null) {

        if (e.getClickCount() >= 2) { // Double click show properties
          if (lastClickedRegion.getConfigurer() != null) {
            Action a = new EditPropertiesAction(lastClickedRegion, null, this);
            if (a != null) {
              a.actionPerformed(
                  new ActionEvent(
                      e.getSource(),
                      ActionEvent.ACTION_PERFORMED,
                      "Edit"));
            }
          }
        }

      }
      view.repaint(); // Clean up selection
    }

    protected void doPopupMenu(MouseEvent e) {

      myPopup = new JPopupMenu();

      JMenuItem menuItem = new JMenuItem("Add Region");
      menuItem.addActionListener(this);
      menuItem.setEnabled(lastClickedRegion == null);
      myPopup.add(menuItem);

      menuItem = new JMenuItem("Delete Region");
      menuItem.addActionListener(this);
      menuItem.setEnabled(lastClickedRegion != null);

      myPopup.add(menuItem);

      myPopup.addSeparator();

      menuItem = new JMenuItem("Properties");
      menuItem.addActionListener(this);
      menuItem.setEnabled(lastClickedRegion != null);
      myPopup.add(menuItem);

      Point p = e.getPoint();

      myPopup
          .addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
            public void popupMenuCanceled(
                javax.swing.event.PopupMenuEvent evt) {
              view.repaint();
            }

            public void popupMenuWillBecomeInvisible(
                javax.swing.event.PopupMenuEvent evt) {
              view.repaint();
            }

            public void popupMenuWillBecomeVisible(
                javax.swing.event.PopupMenuEvent evt) {
            }
          });
      myPopup.show(e.getComponent(), p.x, p.y);
    }

    public void actionPerformed(ActionEvent e) {

      String command = e.getActionCommand();

      if (command.equals("close")) {
        this.setVisible(false);
      }
      else if (command.equals("showhide")) {
        grid.setVisible(!grid.isVisible());
        view.repaint();
      }
      else if (command.equals("Add Region")) {
        Region r = new Region(lastClick);
        r.addTo(grid);
        grid.add(r);
        select(r);
        lastClickedRegion = r;
        setDirty(true);
        Action a = new EditPropertiesAction(lastClickedRegion, null, this);
        if (a != null) {
          a.actionPerformed(
              new ActionEvent(
                  e.getSource(),
                  ActionEvent.ACTION_PERFORMED,
              "Edit"));
        }
        view.repaint();
      }
      else if (command.equals("Delete Region")) {
        for (Region r : selectedRegions) {
          r.removeFrom(grid);
          grid.remove(r);
          lastClickedRegion=null;
          setDirty(true);
        }
        selectedRegions.clear();
        view.repaint();
      }
      else if (command.equals("Properties")) {
        if (lastClickedRegion != null) {
          Action a = new EditRegionAction(lastClickedRegion, null, this);
          if (a != null) {
            a.actionPerformed(
                new ActionEvent(
                    e.getSource(),
                    ActionEvent.ACTION_PERFORMED,
                    "Edit"));
          }
        }
      }

    }
    
    /*
     * Version of EditProperties Action that repaints it's owning frame
     */
    class EditRegionAction extends EditPropertiesAction {

      Config owner;
      Region origRegion;
      Region region;
      
      private static final long serialVersionUID = 1L;
      
      public EditRegionAction(Region target, HelpWindow helpWindow, Config dialogOwner) {
        super(target, helpWindow, dialogOwner);
        owner = dialogOwner;
        origRegion = new Region(target);
        region = target;
       
      }

      public void actionPerformed(ActionEvent evt) {
        PropertiesWindow w = openWindows.get(target);
        if (w == null) {
          w = new PropertiesWindow(dialogOwner,false,target,helpWindow);
          w.addWindowListener(new WindowAdapter() {
            public void windowClosed(WindowEvent e) {
              openWindows.remove(target);
              owner.setDirty(
                  !region.getName().equals(origRegion.getName()) ||
                  !region.getOrigin().equals(origRegion.getOrigin()));
              owner.repaint();
            }
          });
          openWindows.put(target,w);
          w.setVisible(true);
        }
        w.toFront();
      }

      
    }
    
    protected void select(Region r) {
      r.setSelected(true);
      if (!selectedRegions.contains(r)) {
        selectedRegions.add(r);
      }
      view.repaint(r.getSelectionRect());
    }

    protected void unselect(Region r) {
      if (r != null) {
          r.setSelected(false);
          selectedRegions.remove(r);
          if (lastClickedRegion==r) {
            lastClickedRegion=null;
          }
          view.repaint(r.getSelectionRect());
      }
    }

    protected void unSelectAll() {
      for (Region r : selectedRegions) {
        r.setSelected(false);
        view.repaint(r.getSelectionRect());
      }
      selectedRegions.clear();
    }
    
    public Rectangle getSelectionRect() {
      return selectionRect;
    }
    
    public Rectangle getSelectedBox() {
      Rectangle rect = null;
      for (Region r : selectedRegions) {
        Rectangle sel = r.getSelectionRect();
        if (rect == null) {
          rect = sel;
        }
        else {
          rect = rect.union(sel);
        }
      }
      return rect;
    }
    
    public void mouseEntered(MouseEvent evPt) {
    }

    public void mouseExited(MouseEvent evPt) {
    }

    public void mousePressed(MouseEvent e) {
      Point p = e.getPoint();
      lastClick = p;
      lastClickedRegion = grid.getRegion(p);
      
      if (!e.isShiftDown() && !e.isControlDown() && 
          (lastClickedRegion==null || !lastClickedRegion.isSelected())) {
        unSelectAll();
      }

      if (lastClickedRegion == null) {
        anchor = p;
        selectionRect = new Rectangle(anchor.x, anchor.y, 0, 0);
      }
      else {
        if (e.isControlDown()) {
          unselect(lastClickedRegion);
        }
        else {
          select(lastClickedRegion); 
        }
      }

    }

    public void mouseReleased(MouseEvent evPt) {
      if (selectionRect != null) {
        for (Region r : grid.regionList.values()) {
          if (selectionRect.contains(r.getOrigin())) {
            if (evPt.isControlDown()) {
              unselect(r);
            }
            else {
              select(r);
            }
          }
        }
        selectionRect = null;
        view.repaint();
      }
    }

    public void mouseMoved(MouseEvent evPt) {
    }

    // Scroll map if necessary
    public void mouseDragged(MouseEvent e) {
      if (!e.isMetaDown()) {
        scrollAtEdge(e.getPoint(), 15);
      }
      if (selectionRect != null) {
        Rectangle repaintRect = new Rectangle(selectionRect.x-1, selectionRect.y-1, selectionRect.width+3, selectionRect.height+3);
        
        selectionRect.x = Math.min(e.getX(), anchor.x);
        selectionRect.y = Math.min(e.getY(), anchor.y);
        selectionRect.width = Math.abs(e.getX() - anchor.x);
        selectionRect.height = Math.abs(e.getY() - anchor.y);        
        view.repaint(repaintRect.union(new Rectangle(selectionRect.x-1, selectionRect.y-1, selectionRect.width+3, selectionRect.height+3)));
      }
    }

    public void keyPressed(KeyEvent e) {

      /*
       * Pass key onto window scroller if no region selected
       * or control key not used.
       */
      if (selectedRegions.isEmpty() || !e.isControlDown())
        return;

      int dx = 0, dy = 0, delta = 1;

      if (e.isShiftDown()) {
        delta = 5;
      }

      switch (e.getKeyCode()) {
        case KeyEvent.VK_UP:
          dy = -delta;
          break;
        case KeyEvent.VK_DOWN:
          dy = delta;
          break;
        case KeyEvent.VK_LEFT:
          dx = -delta;
          break;
        case KeyEvent.VK_RIGHT:
          dx = delta;
          break;
        default :
          return;
      }
      
      for (Region r : selectedRegions) {
        r.move(dx, dy, view);
      }

      view.repaint();
      e.consume();
    }

    public void keyReleased(KeyEvent e) {
    }

    public void keyTyped(KeyEvent e) {
    }
  }
}

