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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
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
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.RegionGrid;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.properties.GlobalPropertiesContainer;
import VASSAL.build.module.properties.GlobalProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.module.properties.ZoneProperty;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.FormattedString;

public class Zone extends AbstractConfigurable implements GridContainer, GlobalPropertiesContainer, PropertySource, GameComponent {
  public static final String NAME = "name";
  public static final String PATH = "path";
  public static final String USE_PARENT_GRID = "useParentGrid";
  public static final String LOCATION_FORMAT = "locationFormat";
  public static final String GRID_LOCATION = "gridLocation";
  public static final String USE_HIGHLIGHT = "useHighlight";
  public static final String HIGHLIGHT_PROPERTY = "highlightProperty";
  protected String locationFormat = "$" + NAME + "$";
  protected FormattedString format = new FormattedString();
  protected Polygon myPolygon;
  protected MapGrid grid = null;
  protected ZonedGrid parentGrid;
  protected boolean useParentGrid;
  protected java.util.Map globalProperties = new HashMap();
  protected PropertyChangeListener globalPropertyListener;
  /*
   * Cache as much as possible to minimise the number of Affine Transformations that need to be performed.
   */
  protected int lastBoundsX = -1;
  protected int lastBoundsY = -1;
  protected double lastScale = -1;
  protected Shape lastScaledShape = null;
  protected Shape lastTransformedShape = null;
  protected Polygon lastPolygon = null;
  /*
   * Record details of the current highlighter and the property that is controlling highlighting.
   */
  protected ZoneHighlight highlighter = null;
  protected boolean useHighlight = false;
  protected String highlightPropertyName = "";
  protected GlobalProperty highlightProperty = null;
  protected PropertyChangeListener highlightPropertyChangeListener = null;

  public Zone() {
    myPolygon = new Polygon();
  }

  public String getName() {
    return name;
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, LOCATION_FORMAT, PATH, USE_PARENT_GRID, USE_HIGHLIGHT, HIGHLIGHT_PROPERTY};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  ", "Location Format:  ", "Shape", "Use board's grid?", "Use Highlighting?", "Highlight Property:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, LocationFormatConfig.class, ShapeEditor.class, Boolean.class, Boolean.class, String.class};
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
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public void repaint() {
    if (getMap() != null) {
      getMap().repaint();
    }
  }

  public void removeFrom(Buildable b) {
    ((ZonedGrid) b).removeZone(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
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
    else if (USE_HIGHLIGHT.equals(key)) {
      return String.valueOf(useHighlight);
    }
    else if (HIGHLIGHT_PROPERTY.equals(key)) {
      return highlightPropertyName;
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
      PolygonEditor.reset(myPolygon, (String) val);
    }
    else if (LOCATION_FORMAT.equals(key)) {
      locationFormat = (String) val;
    }
    else if (USE_PARENT_GRID.equals(key)) {
      useParentGrid = "true".equals(val) || Boolean.TRUE.equals(val);
    }
    else if (USE_HIGHLIGHT.equals(key)) {
      useHighlight = "true".equals(val) || Boolean.TRUE.equals(val);
    }
    else if (HIGHLIGHT_PROPERTY.equals(key)) {
      highlightPropertyName = (String) val;
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (HIGHLIGHT_PROPERTY.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return useHighlight;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return useParentGrid ? new Class[]{ZoneProperty.class} : new Class[]{HexGrid.class, SquareGrid.class, RegionGrid.class, ZoneProperty.class};
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
    return parentGrid == null ? null : parentGrid.getBoard();
  }

  public Map getMap() {
    return parentGrid == null ? null : parentGrid.getBoard().getMap();
  }

  public ZonedGrid getParentGrid() {
    return parentGrid;
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

  public void setHighlight(ZoneHighlight h) {
    highlighter = h;
  }

  /*
   * Draw the grid if visible and the highlighter if set.
   */
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if ((getGrid() != null && getGrid().isVisible()) || highlighter != null) {
      Graphics2D g2d = (Graphics2D) g;
      Shape oldClip = g2d.getClip();
      Area newClip = new Area(visibleRect);
      Shape s = getCachedShape(myPolygon, bounds.x, bounds.y, scale);
      newClip.intersect(new Area(s));
      g2d.setClip(newClip);
      if (getGrid() != null && getGrid().isVisible()) {
        getGrid().draw(g, bounds, visibleRect, scale, reversed);
      }
      if (highlighter != null) {
        highlighter.draw(g2d, s, scale);
      }
      g2d.setClip(oldClip);
    }
  }

  /*
   * Calculate and cache the scaled zone shape
   */
  protected Shape getScaledShape(Polygon myPolygon, double scale) {
    if (scale == lastScale && lastPolygon == myPolygon && lastScaledShape != null) {
      return lastScaledShape;
    }
    AffineTransform transform = AffineTransform.getScaleInstance(scale, scale);
    lastScaledShape = transform.createTransformedShape(myPolygon);
    lastScale = scale;
    lastPolygon = myPolygon;
    return lastScaledShape;
  }

  /*
   * Calculate and cache the scaled, translated zone shape
   */
  protected Shape getCachedShape(Polygon poly, int x, int y, double scale) {
    if (poly.equals(lastPolygon) && x == lastBoundsX && y == lastBoundsY && scale == lastScale) {
      return lastTransformedShape;
    }
    Shape scaled = getScaledShape(myPolygon, scale);
    AffineTransform transform = AffineTransform.getTranslateInstance(x, y);
    lastTransformedShape = transform.createTransformedShape(scaled);
    lastPolygon = myPolygon;
    lastBoundsX = x;
    lastBoundsY = y;
    lastScale = scale;
    return lastTransformedShape;
  }

  /*
   * Get the value of a property. Pass the call up to the enclosing map if the zone doesn't know about it.
   * 
   * @see VASSAL.build.module.properties.PropertySource#getProperty(java.lang.Object)
   */
  public Object getProperty(Object key) {
    Object value = null;
    value = globalProperties.get(key);
    if (value == null) {
      value = getMap().getProperty(key);
    }
    return value;
  }

  /*
   * Return a named Global Property
   * 
   * @see VASSAL.build.module.properties.GlobalPropertiesContainer#getGlobalProperty(java.lang.String)
   */
  public GlobalProperty getGlobalProperty(String name) {
    GlobalProperty property = null;
    for (Enumeration e = getAllDescendantComponents(GlobalProperty.class); e.hasMoreElements() && property == null;) {
      GlobalProperty prop = (GlobalProperty) e.nextElement();
      if (prop.getConfigureName().equals(name)) {
        property = prop;
      }
    }
    return property;
  }

  /*
   * If using a highlighter, then locate the property and set a propertyListener when the game starts.
   */
  public void setup(boolean gameStarting) {
    if (gameStarting) {
      if (useHighlight && highlightPropertyName.length() > 0) {
        highlightProperty = GlobalProperty
            .findGlobalProperty(highlightPropertyName, Arrays.asList(new GlobalPropertiesContainer[]{this, getMap(), GameModule.getGameModule()}));
        if (highlightProperty != null) {
          if (highlightPropertyChangeListener == null) {
            highlightPropertyChangeListener = new PropertyChangeListener() {
              public void propertyChange(PropertyChangeEvent e) {
                setHighlighter((String) e.getNewValue());
              }
            };
          }
          highlightProperty.addPropertyChangeListener(highlightPropertyChangeListener);
          setHighlighter(highlightProperty.getPropertyValue());
        }
      }
    }
    else {
      if (highlightProperty != null && highlightPropertyChangeListener != null) {
        highlightProperty.removePropertyChangeListener(highlightPropertyChangeListener);
        highlightProperty = null;
      }
    }
  }

  /*
   * The Global Property controlling our highlighter has changed value, so find the new highlighter. Highlighters are
   * stored in the ZonedGrid containing this zone
   */
  public void setHighlighter(String highlightName) {
    highlighter = parentGrid.getZoneHighlight(highlightName);
    repaint();
  }

  public Command getRestoreCommand() {
    return null;
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
      editor = new PolygonEditor(new Polygon(zone.myPolygon.xpoints, zone.myPolygon.ypoints, zone.myPolygon.npoints)) {
        protected void paintBackground(Graphics g) {
          if (board != null) {
            board.draw(g, 0, 0, 1.0, editor);
          }
          else {
            super.paintBackground(g);
          }
        }
      };
      frame = new JDialog((Frame) null, zone.getConfigureName(), true);
      frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
      JPanel labels = new JPanel();
      labels.setLayout(new GridLayout(2, 2));
      labels.add(new JLabel("Drag to create initial shape"));
      labels.add(new JLabel("Right-click to add point"));
      labels.add(new JLabel("Left-click to move points"));
      labels.add(new JLabel("DEL to remove point"));
      labels.setAlignmentX(0.0f);
      frame.getContentPane().add(labels);
      JButton direct = new JButton("Set Coordinates directly");
      direct.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          String newShape = JOptionPane.showInputDialog(frame, "Enter x,y coordinates of polygon vertices,\nseparated by spaces", PolygonEditor
              .polygonToString(editor.getPolygon()).replace(';', ' '));
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
            PolygonEditor.reset(editor.getPolygon(), newShape);
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
      editor.setPreferredSize(board != null ? board.getSize() : new Dimension(600, 600));
      frame.pack();
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      frame.setSize(Math.min(frame.getWidth(), d.width * 2 / 3), Math.min(frame.getHeight(), d.height * 2 / 3));
      frame.setVisible(true);
    }

    public Component getControls() {
      return button;
    }

    public String getValueString() {
      return PolygonEditor.polygonToString(editor.getPolygon());
    }

    public void setValue(String s) {
      PolygonEditor.reset(editor.getPolygon(), s);
    }
  }
}
