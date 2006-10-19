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
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.util.StringTokenizer;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.RegionGrid;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.AdjustableSpeedScrollPane;

public class Zone extends AbstractConfigurable implements GridContainer {
  public static final String NAME = "name";
  public static final String PATH = "path";
  public static final String USE_PARENT_GRID = "useParentGrid";
  public static final String LOCATION_FORMAT = "locationFormat";
  public static final String GRID_LOCATION = "gridLocation";

  protected String locationFormat = "$" + NAME + "$";
  protected FormattedString format = new FormattedString();
  protected Polygon myPolygon;
  private MapGrid grid = null;
  private ZonedGrid parentGrid;
  private boolean useParentGrid;

  public Zone() {
    myPolygon = new Polygon();
  }

  public String getName() {
    return name;
  }

  public String[] getAttributeNames() {
    String s[] = {NAME, LOCATION_FORMAT, PATH, USE_PARENT_GRID};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Name",
      "Location Format",
      "Shape",
      "Use board's grid"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{
      String.class,
      LocationFormatConfig.class,
      ShapeEditor.class,
      Boolean.class};
  }

  public static class LocationFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{NAME, GRID_LOCATION});
    }
  }

  public static class ShapeEditor implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new Editor((Zone) c);
    }
  }

  public void addTo(Buildable b) {
    parentGrid = (ZonedGrid) b;
    parentGrid.addZone(this);
  }

  public void removeFrom(Buildable b) {
    ((ZonedGrid) b).removeZone(this);
  }

  public static String getConfigureTypeName() {
    return "Zone";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (PATH.equals(key)) {
      return PolygonEditor.polygonToString(myPolygon);
    }
    else if (LOCATION_FORMAT.equals(key)) {
      return locationFormat;
    }
    else if (USE_PARENT_GRID.equals(key)) {
      return String.valueOf(useParentGrid);
    }
    return null;
  }

  public void setAttribute(String key, Object val) {
    if (val == null)
      return;

    if (NAME.equals(key)) {
      setConfigureName((String) val);
    }
    else if (PATH.equals(key)) {
      PolygonEditor.reset(myPolygon,(String) val);
    }
    else if (LOCATION_FORMAT.equals(key)) {
      locationFormat = (String) val;
    }
    else if (USE_PARENT_GRID.equals(key)) {
      useParentGrid = "true".equals(val) || Boolean.TRUE.equals(val);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return useParentGrid ? new Class[0] : new Class[]{HexGrid.class, SquareGrid.class, RegionGrid.class};
  }

  public String locationName(Point p) {
    format.setFormat(locationFormat);
    format.setProperty(NAME, getConfigureName());
    String gridLocation = null;
    if (getGrid() != null) {
      gridLocation = getGrid().locationName(p);
    }
    format.setProperty(GRID_LOCATION, gridLocation);
    return format.getText();
  }

  public boolean contains(Point p) {
    return myPolygon.contains(p);
  }

  /**
   * Snap to the grid in this zone,
   */
  public Point snapTo(Point p) {
    Point snap = p;
    if (getGrid() != null) {
      snap = getGrid().snapTo(p);
    }
    return snap;
  }

  public Dimension getSize() {
    return myPolygon.getBounds().getSize();
  }

  public void removeGrid(MapGrid grid) {
    if (this.grid == grid) {
      grid = null;
    }
  }

  public Board getBoard() {
    return parentGrid.getBoard();
  }

  public void setGrid(MapGrid m) {
    grid = m;
  }

  public MapGrid getGrid() {
    if (useParentGrid) {
      return parentGrid != null ? parentGrid.getBackgroundGrid() : null;
    }
    return grid;
  }

  public Shape getShape() {
    return myPolygon;
  }

  public Rectangle getBounds() {
    Rectangle r = myPolygon.getBounds();
    return r;
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (getGrid() != null && getGrid().isVisible()) {
      Graphics2D g2d = (Graphics2D) g;
      Shape oldClip = g2d.getClip();
      Area newClip = new Area(visibleRect);
      AffineTransform transform = AffineTransform.getScaleInstance(scale, scale);
      transform.translate(bounds.x, bounds.y);
      Shape s = transform.createTransformedShape(myPolygon);
      newClip.intersect(new Area(s));
      g2d.setClip(newClip);
      getGrid().draw(g, bounds, visibleRect, scale, reversed);
      g2d.setClip(oldClip);
    }
  }

  public static class Editor extends Configurer {
    private JButton button;
    private PolygonEditor editor;
    private Board board;
    private JDialog frame;

    public Editor(final Zone zone) {
      super(PATH, null);
      button = new JButton("Define Shape");
      button.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          init(zone);
        }
      });
      editor = new PolygonEditor(new Polygon(zone.myPolygon.xpoints,zone.myPolygon.ypoints,zone.myPolygon.npoints)) {
        protected void paintBackground(Graphics g) {
          if (board != null) {
            board.draw(g, 0, 0, 1.0, editor);
          }
          else {
            super.paintBackground(g);
          }
        }
      };
      frame = new JDialog((Frame)null,zone.getConfigureName(),true);
      frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
      JPanel labels = new JPanel();
      labels.setLayout(new GridLayout(2,2));
      labels.add(new JLabel("Drag to create initial shape"));
      labels.add(new JLabel("Right-click to add point"));
      labels.add(new JLabel("Left-click to move points"));
      labels.add(new JLabel("DEL to remove point"));
      labels.setAlignmentX(0.0f);
      frame.getContentPane().add(labels);
      JButton direct = new JButton("Set Coordinates directly");
      direct.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          String newShape = JOptionPane.showInputDialog(frame,"Enter x,y coordinates of polygon vertices,\nseparated by spaces",
                                      PolygonEditor.polygonToString(editor.getPolygon()).replace(';',' '));
          if (newShape != null) {
            StringBuffer buffer = new StringBuffer();
            StringTokenizer st = new StringTokenizer(newShape);
            while (st.hasMoreTokens()) {
              buffer.append(st.nextToken());
              if (st.hasMoreTokens()) {
                buffer.append(';');
              }
            }
            newShape = buffer.toString();
            PolygonEditor.reset(editor.getPolygon(),newShape);
            editor.repaint();
          }
        }
      });
      direct.setAlignmentX(0.0f);
      frame.getContentPane().add(direct);
      frame.getContentPane().add(new AdjustableSpeedScrollPane(editor));
      JPanel buttonPanel = new JPanel();
      JButton closeButton = new JButton("Ok");
      closeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setValue((Object) getValueString());
          frame.setVisible(false);
          GameModule.getGameModule().getDataArchive().clearScaledImageCache();
        }
      });
      buttonPanel.add(closeButton);
      frame.getContentPane().add(buttonPanel);
    }

    private void init(Zone zone) {
      board = zone.getBoard();
      if (board != null) {
        board.fixImage(editor);
      }
      editor.setPreferredSize(board != null ? board.getSize() : new Dimension(600,600));
      frame.pack();
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      frame.setSize(Math.min(frame.getWidth(),d.width*2/3),Math.min(frame.getHeight(),d.height*2/3));
      frame.setVisible(true);
    }

    public Component getControls() {
      return button;
    }

    public String getValueString() {
      return PolygonEditor.polygonToString(editor.getPolygon());
    }

    public void setValue(String s) {
      PolygonEditor.reset(editor.getPolygon(),s);
    }
  }

}
