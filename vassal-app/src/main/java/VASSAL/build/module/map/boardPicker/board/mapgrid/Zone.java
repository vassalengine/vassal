/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.RegionGrid;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.properties.ChangePropertyCommandEncoder;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.module.properties.ZoneProperty;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.script.expression.Auditable;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.swing.SwingUtils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import net.miginfocom.swing.MigLayout;

public class Zone extends AbstractConfigurable implements GridContainer, MutablePropertiesContainer, PropertySource, GameComponent {
  public static final String NAME = "name"; //NON-NLS
  public static final String PATH = "path"; //NON-NLS
  public static final String USE_PARENT_GRID = "useParentGrid"; //NON-NLS
  public static final String LOCATION_FORMAT = "locationFormat"; //NON-NLS
  public static final String GRID_LOCATION = "gridLocation"; //NON-NLS
  public static final String USE_HIGHLIGHT = "useHighlight"; //NON-NLS
  public static final String HIGHLIGHT_PROPERTY = "highlightProperty"; //NON-NLS
  protected static final Dimension DEFAULT_SIZE = new Dimension(600, 600);
  protected String locationFormat = "$" + NAME + "$";
  protected FormattedString format = new FormattedString();
  protected Polygon myPolygon;
  protected MapGrid grid = null;
  protected ZonedGrid parentGrid;
  protected boolean useParentGrid;
  protected PropertyChangeListener globalPropertyListener;
  protected MutablePropertiesContainer propsContainer = new Impl();
  protected PropertyChangeListener repaintOnPropertyChange = evt -> repaint();
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
  protected MutableProperty highlightProperty = null;
  protected PropertyChangeListener highlightPropertyChangeListener = null;

  public Zone() {
    myPolygon = new Polygon();
    setConfigureName("");
  }

  public String getName() {
    return getConfigureName();
  }

  public String getLocalizedName() {
    return getLocalizedConfigureName();
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      LOCATION_FORMAT,
      PATH,
      USE_PARENT_GRID,
      USE_HIGHLIGHT,
      HIGHLIGHT_PROPERTY
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.Zone.location_format"),
      Resources.getString("Editor.Zone.shape"),
      Resources.getString("Editor.Zone.use_boards_grid"),
      Resources.getString("Editor.Zone.use_highlighting"),
      Resources.getString("Editor.Zone.highlight_property")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      LocationFormatConfig.class,
      ShapeEditor.class,
      Boolean.class,
      Boolean.class,
      String.class
    };
  }

  public static class LocationFormatConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{NAME, GRID_LOCATION});
    }
  }

  public static class ShapeEditor implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new Editor((Zone) c);
    }
  }

  @Override
  public void addTo(Buildable b) {
    parentGrid = (ZonedGrid) b;
    parentGrid.addZone(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(new ChangePropertyCommandEncoder(this));
    setAttributeTranslatable(HIGHLIGHT_PROPERTY, false);
  }

  public void repaint() {
    if (getMap() != null) {
      getMap().repaint();
    }
  }

  @Override
  public void removeFrom(Buildable b) {
    ((ZonedGrid) b).removeZone(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Zone.component_type");
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
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

  @Override
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
      useParentGrid = "true".equals(val) || Boolean.TRUE.equals(val); //NON-NLS
    }
    else if (USE_HIGHLIGHT.equals(key)) {
      useHighlight = "true".equals(val) || Boolean.TRUE.equals(val); //NON-NLS
    }
    else if (HIGHLIGHT_PROPERTY.equals(key)) {
      highlightPropertyName = (String) val;
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (HIGHLIGHT_PROPERTY.equals(name)) {
      return () -> useHighlight;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return useParentGrid ?
      new Class<?>[]{ZoneProperty.class} :
      new Class<?>[]{
        HexGrid.class,
        SquareGrid.class,
        RegionGrid.class,
        ZoneProperty.class
      };
  }

  @Override
  public void addMutableProperty(String key, MutableProperty p) {
    p.addMutablePropertyChangeListener(repaintOnPropertyChange);
    propsContainer.addMutableProperty(key, p);
  }

  @Override
  public MutableProperty removeMutableProperty(String key) {
    final MutableProperty existing = propsContainer.removeMutableProperty(key);
    if (existing != null) {
      existing.removeMutablePropertyChangeListener(repaintOnPropertyChange);
    }
    return existing;
  }

  public Point getLocation(String location) throws BadCoords {
    final SequenceEncoder.Decoder se =
      new SequenceEncoder.Decoder(locationFormat, '$');
    boolean isProperty = true;
    final StringBuilder regex = new StringBuilder();
    int groupCount = 0;

    while (se.hasMoreTokens()) {
      final String token = se.nextToken();
      isProperty = !isProperty;

      if (token.length() > 0) {
        if (!isProperty || !se.hasMoreTokens()) {
          regex.append(Pattern.quote(token));
        }
        else if (token.equals(NAME)) {
          regex.append(Pattern.quote(getConfigureName()));
        }
        else if (token.equals(GRID_LOCATION) && getGrid() != null) {
          regex.append("(.*)");
          ++groupCount;
        }
      }
    }

    if (regex.length() == 0) {
      throw new BadCoords(); // nothing to match!
    }

    final Pattern pattern = Pattern.compile(regex.toString());
    final Matcher matcher = pattern.matcher(location);
    if (!matcher.matches()) {
      throw new BadCoords();
    }
    assert (matcher.groupCount() == groupCount);

    final Point p;
    if (groupCount > 0) {
      final String locationName = location.substring(matcher.start(groupCount), matcher.end(groupCount));
      p = getGrid().getLocation(locationName);
      if (p == null || !contains(p)) {
        throw new BadCoords();
      }
      else {
        return p;
      }
    }
    else {
      // no grid to match against
      // try the geographic mean
      p = new Point(0, 0);
      for (int i = 0; i < myPolygon.npoints; ++i) {
        p.translate(myPolygon.xpoints[i], myPolygon.ypoints[i]);
      }
      p.x /= myPolygon.npoints;
      p.y /= myPolygon.npoints;

      if (!contains(p)) {
        // concave polygon
        // default to the first point
        p.x = myPolygon.xpoints[0];
        p.y = myPolygon.ypoints[0];
      }
      return p;
    }
  }

  public String locationName(Point p) {
    format.setFormat(locationFormat);
    format.setProperty(NAME, getConfigureName());
    String gridLocation = null;
    if (getGrid() != null) {
      gridLocation = getGrid().locationName(p);
    }
    format.setProperty(GRID_LOCATION, gridLocation);
    return format.getText((Auditable) this, "Editor.Zone.location_format");
  }

  public String localizedLocationName(Point p) {
    format.setFormat(locationFormat);
    format.setProperty(NAME, getLocalizedConfigureName());
    String gridLocation = null;
    if (getGrid() != null) {
      gridLocation = getGrid().localizedLocationName(p);
    }
    format.setProperty(GRID_LOCATION, gridLocation);
    return format.getLocalizedText((Auditable) this, "Editor.Zone.location_format");
  }

  @Override
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

  @Override
  public Dimension getSize() {
    return myPolygon.getBounds().getSize();
  }

  @Override
  public void removeGrid(MapGrid grid) {
    if (this.grid == grid) {
      this.grid = null;
    }
  }

  @Override
  public Board getBoard() {
    return parentGrid == null ? null : parentGrid.getBoard();
  }

  public Map getMap() {
    return parentGrid == null ? null : parentGrid.getBoard().getMap();
  }

  public ZonedGrid getParentGrid() {
    return parentGrid;
  }

  @Override
  public void setGrid(MapGrid m) {
    grid = m;
  }

  public MapGrid getGrid() {
    if (useParentGrid) {
      return parentGrid != null ? parentGrid.getBackgroundGrid() : null;
    }
    return grid;
  }

  public boolean isUseParentGrid() {
    return useParentGrid;
  }

  public Shape getShape() {
    return myPolygon;
  }

  public Rectangle getBounds() {
    return myPolygon.getBounds();
  }

  public void setHighlight(ZoneHighlight h) {
    highlighter = h;
  }

  /*
   * Draw the grid if visible and the highlighter if set.
   */
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if ((getGrid() != null && getGrid().isVisible()) || highlighter != null) {
      final Graphics2D g2d = (Graphics2D) g;
      final Shape oldClip = g2d.getClip();
      final Area newClip = new Area(visibleRect);
      final Shape s = getCachedShape(myPolygon, bounds.x, bounds.y, scale);
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
    final AffineTransform transform =
      AffineTransform.getScaleInstance(scale, scale);
    lastScaledShape = transform.createTransformedShape(myPolygon);
    lastScale = scale;
    lastPolygon = myPolygon;
    return lastScaledShape;
  }

  /*
   * Calculate and cache the scaled, translated zone shape
   */
  protected Shape getCachedShape(Polygon poly, int x, int y, double scale) {
    if (poly.equals(lastPolygon) &&
        x == lastBoundsX &&
        y == lastBoundsY &&
        scale == lastScale) {
      return lastTransformedShape;
    }

    final Shape scaled = getScaledShape(myPolygon, scale);
    final AffineTransform transform =
      AffineTransform.getTranslateInstance(x, y);
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
  @Override
  public Object getProperty(Object key) {
    final Object value;
    final MutableProperty p =
      propsContainer.getMutableProperty(String.valueOf(key));
    if (p != null) {
      value = p.getPropertyValue();
    }
    else {
      value = getMap().getProperty(key);
    }
    return value;
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    Object value = null;
    final MutableProperty p =
      propsContainer.getMutableProperty(String.valueOf(key));
    if (p != null) {
      value = p.getPropertyValue();
    }
    if (value == null) {
      value = getMap().getLocalizedProperty(key);
    }
    return value;
  }

  /**
   * Implement PropertyNameSource - expose names of my ZoneProperties
   */
  @Override
  public List<String> getPropertyNames() {
    final List<String> l = new ArrayList<>();
    for (final ZoneProperty zp : getComponentsOf(ZoneProperty.class)) {
      l.add(zp.getConfigureName());
    }
    return l;
  }

  /*
   * Return a named Global Property
   *
   * @see VASSAL.build.module.properties.GlobalPropertiesContainer#getGlobalProperty(java.lang.String)
   */
  @Override
  public MutableProperty getMutableProperty(String name) {
    return propsContainer.getMutableProperty(name);
  }

  @Override
  public String getMutablePropertiesContainerId() {
    return (getMap() == null ? "" : getMap().getMapName()) + ":" + getConfigureName();
  }

  /*
   * If using a highlighter, then locate the property and set a propertyListener when the game starts.
   */
  @Override
  public void setup(boolean gameStarting) {
    if (gameStarting) {
      if (useHighlight && highlightPropertyName.length() > 0) {
        highlightProperty = MutableProperty.Util
            .findMutableProperty(highlightPropertyName, Arrays.asList(new MutablePropertiesContainer[]{this, getMap(), GameModule.getGameModule()}));
        if (highlightProperty != null) {
          if (highlightPropertyChangeListener == null) {
            highlightPropertyChangeListener = e -> setHighlighter((String) e.getNewValue());
          }
          highlightProperty.addMutablePropertyChangeListener(highlightPropertyChangeListener);
          setHighlighter(highlightProperty.getPropertyValue());
        }
      }
    }
    else {
      if (highlightProperty != null && highlightPropertyChangeListener != null) {
        highlightProperty.removeMutablePropertyChangeListener(highlightPropertyChangeListener);
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

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * Our configurer for the Zone, including ability to edit the polygon
   */
  public static class Editor extends Configurer implements PolygonConfigurer {
    private final JPanel buttonPanel;
    private final JButton button;
    private PolygonEditor editor;
    private Board board;
    private JDialog frame;
    protected AdjustableSpeedScrollPane scroll;
    protected Polygon savePoly;
    protected FlowLabel coordsLabel;
    protected JLabel coordLabel;
    protected Zone zone;
    protected final JLabel warning = new JLabel(Resources.getString("Editor.Zone.zone_has_not_been_defined"));

    public Editor(final Zone zone) {
      super(PATH, null);

      buttonPanel = new JPanel(new MigLayout("ins 0")); // NON-NLS
      button = new JButton(Resources.getString("Editor.Zone.define_shape"));
      buttonPanel.add(button);
      button.addActionListener(e -> init(zone));

      this.zone = zone;
    }

    @Override
    public void updateCoords(Polygon polygon) {
      final StringBuilder s = new StringBuilder("");
      if (polygon != null) {
        for (int p = 0; p < polygon.npoints; p++) {
          s.append('(');
          s.append(polygon.xpoints[p]);
          s.append(',');
          s.append(polygon.ypoints[p]);
          s.append(") ");
        }
      }
      coordsLabel.setText(s.toString());
      coordsLabel.repaint();
    }

    @Override
    public void updateCoords() {
      updateCoords(zone.myPolygon);
    }

    @Override
    public void updateCoord(String s) {
      coordLabel.setText(s);
      coordLabel.repaint();
    }

    @Override
    public void updateCoord(int x, int y) {
      updateCoord(x + "," + y);
    }

    private void init(Zone zone) {
      editor = new PolygonEditor(new Polygon(zone.myPolygon.xpoints, zone.myPolygon.ypoints, zone.myPolygon.npoints), new Point(0, 0)) {
        private static final long serialVersionUID = 1L;

        @Override
        protected void paintBackground(Graphics g) {
          if (board != null) {
            final Graphics2D g2d = (Graphics2D) g;
            final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
            final AffineTransform orig_t = g2d.getTransform();
            g2d.setTransform(SwingUtils.descaleTransform(orig_t));

            final Rectangle b = getVisibleRect();
            b.x *= os_scale;
            b.y *= os_scale;
            b.width *= os_scale;
            b.height *= os_scale;

            g.clearRect(b.x, b.y, b.width, b.height);
            board.draw(g, 0, 0, os_scale, editor);

            g2d.setTransform(orig_t);
          }
          else {
            super.paintBackground(g);
          }
          warning.setVisible(editor != null && (editor.getPolygon() == null || editor.getPolygon().npoints == 0));
        }
      };

      editor.setMyConfigurer(this);

      frame = new JDialog(GameModule.getGameModule().getPlayerWindow(), zone.getConfigureName(), true);
      frame.setFocusTraversalKeysEnabled(false);

      frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
      final JPanel labels = new JPanel();
      labels.setLayout(new GridLayout(3, 2));
      labels.add(new JLabel(Resources.getString("Editor.Zone.drag_to_create_initial_shape")));
      labels.add(new JLabel(Resources.getString("Editor.Zone.right_click_to_add_point")));
      labels.add(new JLabel(Resources.getString("Editor.Zone.left_drag_to_move_points")));
      labels.add(new JLabel(Resources.getString("Editor.Zone.del_to_remove_points")));
      warning.setForeground(Color.red);
      warning.setVisible(false);
      labels.add(warning);
      labels.setAlignmentX(0.0f);
      frame.add(labels);

      final JButton direct = new JButton(Resources.getString("Editor.Zone.set_coordinates_directly"));
      direct.setFocusable(false);
      direct.addActionListener(e -> {
        String newShape = JOptionPane.showInputDialog(frame, Resources.getString("Editor.Zone.enter_points_instructions"), PolygonEditor
          .polygonToString(editor.getPolygon()).replace(';', ' '));
        if (newShape != null) {
          final StringBuilder buffer = new StringBuilder();
          final StringTokenizer st = new StringTokenizer(newShape);
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
      });
      direct.setAlignmentX(0.0f);
      frame.add(direct);

      scroll = new AdjustableSpeedScrollPane(editor, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
      editor.setScroll(scroll);
      frame.add(scroll);

      final Box coordPanel = Box.createVerticalBox();
      coordsLabel = new FlowLabel("");
      coordLabel = new JLabel("");
      coordPanel.add(coordLabel, BorderLayout.CENTER);
      coordPanel.add(coordsLabel, BorderLayout.CENTER);
      updateCoords();
      updateCoord("");
      frame.add(coordPanel);

      final JPanel buttonPanel = new JPanel();

      final JButton closeButton = new JButton(Resources.getString("General.ok"));
      closeButton.setFocusable(false);
      closeButton.addActionListener(e -> {
        setValue((Object) getValueString());
        frame.setVisible(false);
      });

      final JButton canButton = new JButton(Resources.getString("General.cancel"));
      canButton.setFocusable(false);
      canButton.addActionListener(e -> {
        editor.setPolygon(savePoly);
        setValue((Object) getValueString());
        frame.setVisible(false);
      });

      buttonPanel.add(closeButton);
      buttonPanel.add(canButton);
      frame.add(buttonPanel);

      board = zone.getBoard();
      editor.setPreferredSize(board != null ? board.getSize() : DEFAULT_SIZE);
      editor.reset();
      savePoly = new Polygon(zone.myPolygon.xpoints, zone.myPolygon.ypoints, zone.myPolygon.npoints);
      editor.setPolygon((zone.myPolygon.npoints == 0) ? null : new Polygon(zone.myPolygon.xpoints, zone.myPolygon.ypoints, zone.myPolygon.npoints));
      if (editor.getPolygon() != null) {
        final Rectangle polyBounds = editor.getPolygon().getBounds();
        final Point polyCenter = new Point(polyBounds.x + polyBounds.width / 2,
          polyBounds.y + polyBounds.height / 2);
        if (!editor.getVisibleRect().contains(polyCenter)) {
          editor.center(polyCenter);
        }
      }
      frame.pack();
      final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      frame.setSize(Math.min(frame.getWidth(), d.width * 2 / 3), Math.min(frame.getHeight(), d.height * 2 / 3));
      frame.setTitle(zone.getConfigureName());
      frame.setVisible(true);
    }

    @Override
    public Component getControls() {
      return buttonPanel;
    }

    @Override
    public String getValueString() {
      if (editor != null) {
        return PolygonEditor.polygonToString(editor.getPolygon());
      }
      return "";
    }

    @Override
    public void setValue(String s) {
      if (editor != null) {
        PolygonEditor.reset(editor.getPolygon(), s);
      }
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(locationFormat);
  }
}
