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

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.configure.Configurer;
import VASSAL.configure.EditPropertiesAction;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.AdjustableSpeedScrollPane;

public class RegionGrid extends AbstractConfigurable implements MapGrid {

  // AreaList is the table of Map areas
  // pointList is a cross-reference of points to Area names

  private Hashtable regionList = new Hashtable();
  private GridContainer container;
  private boolean visible = false;
  private static boolean inConfig = false;
  private int fontSize = 9; // Size square to display when configuring
  private boolean snapTo = true;
  private Config regionConfigurer;

  private GridNumbering gridNumbering;
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
      "Draw region names",
      "Font Size"};
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
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "IrregularGrid.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
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
	Point snapPoint, checkPoint;

    //
    // Enumerate through each grid point and determine the closest.
    //
    snapPoint = p;
    Enumeration e = regionList.keys();
    while (e.hasMoreElements()) {
      checkPoint = (Point) e.nextElement();
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

    Region region = (Region) regionList.get(checkPoint);
    return region != null ? region.getName() : null;

  }

  /**
   * Return Region selected by Point
   */
  public Region getRegion(Point p) {
    Region checkRegion, r = null;

    Enumeration e = regionList.elements();
    while (e.hasMoreElements()) {
      checkRegion = (Region) e.nextElement();
      if (checkRegion.contains(p))
        return checkRegion;
    }
    return r;
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

    Enumeration e = regionList.elements();
    while (e.hasMoreElements()) {
      Region r = (Region) e.nextElement();
      r.draw(g, bounds, visibleRect, scale, reversed);
    }

    return;
  }

  public void unSelectAll() {
    Enumeration e = regionList.elements();
    while (e.hasMoreElements()) {
      unSelect((Region) e.nextElement());
    }
  }

  public void unSelect(Region r) {
    r.setSelected(false);
  }

  public static class Config extends JFrame implements MouseListener, MouseMotionListener, ActionListener, KeyListener {

    protected RegionGrid grid;
    protected Board board;

    protected JPanel view;
    protected JScrollPane scroll;
    protected JPopupMenu myPopup;

    private Region selectedRegion = null;
    private Point lastClick;

    public Config(RegionGrid grid) {
      super("Regions for " + grid.container.getBoard().getName());
      board = grid.container.getBoard();
      this.grid = grid;
      initComponents();
    }

    // Main Entry Point
    private void initComponents() {
      setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

      view = new Config.View(board, grid);

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

      getContentPane().add(scroll, BorderLayout.CENTER);

      JButton okButton = new JButton("Ok");
      okButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          inConfig = false;
          Config.this.setVisible(false);
        }
      });
      JPanel p = new JPanel();
      p.add(okButton);
      getContentPane().add(p, BorderLayout.SOUTH);

      board.fixImage(view);
      scroll.revalidate();
      pack();
      repaint();
    }

    /*
     * Scrolls the map in the containing JScrollPane
     * @param dx number of pixels to scroll horizontally
     * @param dy number of pixels to scroll vertically
     */
    public void doScroll(int dx, int dy) {
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
    public void scrollAtEdge(Point evtPt, int dist) {
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

    /*
     * The scrollpane client
     */
    public static class View extends JPanel {
      protected Board myBoard;
      protected RegionGrid grid;

      public View(Board b, RegionGrid grid) {
        myBoard = b;
        this.grid = grid;
        setFocusTraversalKeysEnabled(false);
      }

      public void paint(Graphics g) {
        myBoard.draw(g, 0, 0, 1.0, this);
        Rectangle bounds = new Rectangle(new Point(),myBoard.bounds().getSize());
        grid.forceDraw(g,bounds,bounds,1.0,false);
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
    }

    /*
     * Mouse Listeners
     */

    // Mouse clicked, see if it is on a Region Point
    public void mouseClicked(MouseEvent e) {
      Point p = e.getPoint();
      lastClick = p;
      Region r = grid.getRegion(p);
      unselect(selectedRegion); // Unselect any previously selected region

      if (r != null) {
        select(r); // And select the new one
      }

      if (e.isMetaDown()) { // Right click...menu
        doPopupMenu(e, r);
      }
      else if (r != null) {

        if (e.getClickCount() >= 2) { // Double click show properties
          if (r.getConfigurer() != null) {
            Action a = new EditPropertiesAction(r, null, this);
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

    public void doPopupMenu(MouseEvent e, Region r) {

      myPopup = new JPopupMenu();

      JMenuItem menuItem = new JMenuItem("Add Region");
      menuItem.addActionListener(this);
      menuItem.setEnabled(selectedRegion == null);
      myPopup.add(menuItem);

      menuItem = new JMenuItem("Delete Region");
      menuItem.addActionListener(this);
      menuItem.setEnabled(selectedRegion != null);

      myPopup.add(menuItem);

      myPopup.addSeparator();

      menuItem = new JMenuItem("Properties");
      menuItem.addActionListener(this);
      menuItem.setEnabled(selectedRegion != null);
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
//			theMap.requestFocusInWindow();
      }
      else if (command.equals("Add Region")) {
        Region r = new Region(lastClick);
        r.addTo(grid);
        grid.add(r);
        select(r);
		Action a = new EditPropertiesAction(selectedRegion, null, this);
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
        selectedRegion.removeFrom(grid);
        grid.remove(selectedRegion);
        selectedRegion = null;
      }
      else if (command.equals("Properties")) {
        if (selectedRegion != null) {
          Action a = new EditPropertiesAction(selectedRegion, null, this);
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

    public void select(Region r) {
      r.setSelected(true);
      selectedRegion = r;
      view.repaint(r.getSelectionRect());
    }

    public void unselect(Region r) {
      if (r != null) {
        if (r == selectedRegion) {
          r.setSelected(false);
          selectedRegion = null;
          view.repaint(r.getSelectionRect());
        }
      }
    }

    public void mouseEntered(MouseEvent evPt) {
    }

    public void mouseExited(MouseEvent evPt) {
    }

    public void mousePressed(MouseEvent evPt) {
    }

    public void mouseReleased(MouseEvent evPt) {
    }

    public void mouseMoved(MouseEvent evPt) {
    }

    // Scroll map if necessary
    public void mouseDragged(MouseEvent e) {
      if (!e.isMetaDown()) {
        scrollAtEdge(e.getPoint(), 15);
      }
    }

    public void keyPressed(KeyEvent e) {

      /*
       * Pass key onto window scroller if no region selected
       * or control key not used.
       */
      if (selectedRegion == null || !e.isControlDown())
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
      selectedRegion.move(dx, dy, view);

      view.repaint();
      e.consume();
    }

    public void keyReleased(KeyEvent e) {
    }

    public void keyTyped(KeyEvent e) {
    }
  }
}

