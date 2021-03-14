/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney, Joel Uckelman
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

import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.search.ImageSearchTarget;
import VASSAL.search.SearchTarget;
import VASSAL.tools.ProblemDialog;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.Translatable;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.swing.SwingUtils;

/**
 * This is scaled version of a {@link Map} that gives an overview.
 * Users can navigate around the Map by clicking on the GlobalMap, which
 * draws a rectangular region of interest (ROI) indicating the current
 * viewable area in the map window.
 */
public class GlobalMap implements AutoConfigurable,
                                  GameComponent,
                                  Drawable,
                                  SearchTarget,
                                  ImageSearchTarget {
  private static final long serialVersionUID = 2L;

  protected Map map;
  protected double scale = 0.19444444; // Zoom factor

  protected Color rectColor = Color.black;
  protected final LaunchButton launch;

  protected CounterDetailViewer mouseOverViewer;
  protected final ScrollPane scroll;
  protected final View view;
  protected ComponentI18nData myI18nData;

  public GlobalMap() {
    view = new View();
    view.addMouseListener(view);

    scroll = new GlobalMapScrollPane(view);
    scroll.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
    scroll.setAlignmentX(0.0f);
    scroll.setAlignmentY(0.0f);

    final ActionListener al = e -> scroll.setVisible(!scroll.isVisible());

    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT,
                              HOTKEY, ICON_NAME, al);
    launch.setAttribute(TOOLTIP, Resources.getString("Editor.GlobalMap.show_hide_overview_window"));
    launch.setAttribute(HOTKEY,
      NamedKeyStroke.of(
        KeyEvent.VK_O, KeyEvent.CTRL_DOWN_MASK | KeyEvent.SHIFT_DOWN_MASK
      )
    );
  }

  /**
   * Expects to be added to a {@link Map}. Adds itself as a {@link
   * GameComponent} and a {@link Drawable}component
   */
  @Override
  public void addTo(Buildable b) {
    map = (Map) b;

    mouseOverViewer = new CounterViewer();

    GameModule.getGameModule().getGameState().addGameComponent(this);

    GameModule.getGameModule().addKeyStrokeSource(
      new KeyStrokeSource(view, JComponent.WHEN_FOCUSED));

    map.addDrawComponent(this);
    map.getToolBar().add(launch);

    if (b instanceof Translatable) {
      getI18nData().setOwningComponent((Translatable) b);
    }

    map.getLayeredPane().add(scroll, JLayeredPane.PALETTE_LAYER);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public void remove(Buildable b) {
  }

  @Override
  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.removeDrawComponent(this);
    map.getToolBar().remove(launch);
    GameModule.getGameModule().getGameState().removeGameComponent(this);

    map.getLayeredPane().remove(scroll);
  }

  @Override
  public void build(Element e) {
    AutoConfigurable.Util.buildAttributes(e, this);
  }

  protected static final String SCALE = "scale"; //NON-NLS
  protected static final String COLOR = "color"; //NON-NLS
  protected static final String HOTKEY = "hotkey"; //NON-NLS
  protected static final String ICON_NAME = "icon"; //NON-NLS
  protected static final String TOOLTIP = "tooltip"; //NON-NLS
  protected static final String BUTTON_TEXT = "buttonText"; //NON-NLS
  protected static final String DEFAULT_ICON = "/images/overview.gif"; //NON-NLS

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      TOOLTIP,
      BUTTON_TEXT,
      ICON_NAME,
      HOTKEY,
      SCALE,
      COLOR
    };
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (SCALE.equals(key)) {
      if (value instanceof String) {
        value = Double.valueOf((String) value);
      }
      scale = (Double) value;
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      rectColor = (Color) value;
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (SCALE.equals(key)) {
      return String.valueOf(scale);
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(rectColor);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString(Resources.TOOLTIP_TEXT),
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.BUTTON_ICON),
      Resources.getString("Editor.GlobalMap.show_hide"), //$NON-NLS-1$
      Resources.getString("Editor.GlobalMap.scale_factor"), //$NON-NLS-1$
      Resources.getString("Editor.GlobalMap.hilight"), //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class,
      Double.class,
      Color.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, DEFAULT_ICON);
    }
  }

  @Override
  public void draw(Graphics g, Map m) {
    view.repaint();
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  /**
   * Transform a point from Map coordinates to coordinates in the overview
   * window
   *
   * @param p Point
   * @return Transformed Point
   * @deprecated Use {@link #mapToComponent(Point)}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Point componentCoordinates(Point p) {
    ProblemDialog.showDeprecated("2020-08-06");
    return mapToComponent(p);
  }

  /**
   * Transform a point from coordinates in the overview window to Map
   * coordinates
   *
   * @param p Point
   * @return Transformed Point
   * @deprecated Use {@link #componentToMap(Point)}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Point mapCoordinates(Point p) {
    ProblemDialog.showDeprecated("2020-08-06");
    return componentToMap(p);
  }

  public Point componentToMap(Point p) {
    return new Point(
      (int) Math.round(p.x / scale) + map.getEdgeBuffer().width,
      (int) Math.round(p.y / scale) + map.getEdgeBuffer().height);
  }

  public Point mapToComponent(Point p) {
    return new Point((int) ((p.x - map.getEdgeBuffer().width) * scale),
                     (int) ((p.y - map.getEdgeBuffer().height) * scale));
  }

  public Point mapToDrawing(Point p, double os_scale) {
    final double dscale = scale * os_scale;
    return new Point((int) ((p.x - map.getEdgeBuffer().width) * dscale),
                     (int) ((p.y - map.getEdgeBuffer().height) * dscale));
  }

  public Rectangle mapToDrawing(Rectangle r, double os_scale) {
    final double dscale = scale * os_scale;
    return new Rectangle(
      (int) ((r.x - map.getEdgeBuffer().width) * dscale),
      (int) ((r.y - map.getEdgeBuffer().height) * dscale),
      (int) (r.width * dscale),
      (int) (r.height * dscale)
    );
  }

  public String getToolTipText(@SuppressWarnings("unused") MouseEvent e) {
    return null;
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  @Override
  public void setup(boolean show) {
    if (show) {
      scroll.setMaximumSize(scroll.getPreferredSize());
    }
    else {
      scroll.setVisible(false);
    }

    if (show && !map.getComponentsOf(CounterDetailViewer.class).isEmpty()) {
      view.addMouseListener(mouseOverViewer);
      view.addMouseMotionListener(mouseOverViewer);
      scroll.addKeyListener(mouseOverViewer);
    }
    else {
      view.removeMouseListener(mouseOverViewer);
      view.removeMouseMotionListener(mouseOverViewer);
      scroll.removeKeyListener(mouseOverViewer);
    }
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GlobalMap.component_type"); //$NON-NLS-1$
  }

  @Override
  public String getConfigureName() {
    return null;
  }

  @Override
  public Configurer getConfigurer() {
    return new AutoConfigurer(this);
  }

  @Override
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "OverviewWindow"); //NON-NLS
  }

  @Override
  public Element getBuildElement(Document doc) {
    return AutoConfigurable.Util.getBuildElement(doc, this);
  }

  protected class CounterViewer extends CounterDetailViewer {
    public CounterViewer() {
      this.map = GlobalMap.this.map;
      this.view = GlobalMap.this.view;
    }

    @Override
    protected List<GamePiece> getDisplayablePieces() {
      final Point oldPoint = currentMousePosition.getPoint();
      final Point mapPoint =
        GlobalMap.this.map.mapToComponent(componentToMap(oldPoint));

      currentMousePosition.translatePoint(mapPoint.x - oldPoint.x,
                                          mapPoint.y - oldPoint.y);
      final List<GamePiece> l = super.getDisplayablePieces();
      currentMousePosition.translatePoint(oldPoint.x - mapPoint.x,
                                          oldPoint.y - mapPoint.y);
      return l;
    }

    @Override
    protected double getZoom() {
      return scale;
    }
  }

  /**
   * The scroll pane in which the map {@link View} is displayed.
   */
  protected class GlobalMapScrollPane extends ScrollPane {
    private static final long serialVersionUID = 1L;

    public GlobalMapScrollPane(Component view) {
      super(view, ScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                  ScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }

    /**
     * @return The display size of the entire zoomed overview map
     */
    @Override
    public Dimension getPreferredSize() {
      final Dimension d = view.getPreferredSize();
      final Insets i = getInsets();
      d.width += i.left + i.right;
      d.height += i.top + i.bottom;
      return d;
    }

    /**
     * @return The maximum size of the zoomed overview map and scroll pane
     */
    @Override
    public Dimension getMaximumSize() {
      final Dimension d = getPreferredSize();

      if (verticalScrollBar.isVisible()) {
        d.width += verticalScrollBar.getPreferredSize().width;
      }
      if (horizontalScrollBar.isVisible()) {
        d.height += horizontalScrollBar.getPreferredSize().height;
      }

      return d;
    }

    @Override
    public void setBounds(Rectangle r) {
      final Dimension availSize = map.getView().getParent().getSize();
      final Dimension viewSize = view.getPreferredSize();
      final Insets i = getInsets();
      viewSize.width += i.left + i.right;
      viewSize.height += i.top + i.bottom;

      final boolean hsbNeeded = availSize.width < viewSize.width;
      final boolean vsbNeeded = availSize.height < viewSize.height;

      final Dimension realSize = new Dimension();

      if (availSize.width < viewSize.width) {
        realSize.width = availSize.width;
      }
      else if (vsbNeeded) {
        realSize.width = Math.min(availSize.width,
          viewSize.width + verticalScrollBar.getPreferredSize().width);
      }
      else {
        realSize.width = viewSize.width;
      }

      if (availSize.height < viewSize.height) {
        realSize.height = availSize.height;
      }
      else if (hsbNeeded) {
        realSize.height = Math.min(availSize.height,
        viewSize.height + horizontalScrollBar.getPreferredSize().height);
      }
      else {
        realSize.height = viewSize.height;
      }

      super.setBounds(0, 0, realSize.width, realSize.height);
    }

    /**
     * This function is overridden to make sure that the parent layout
     * is redone when the GlobalMap is shown.
     */
    @Override
    public void setVisible(boolean visible) {
      super.setVisible(visible);
      if (visible) {
        final LayoutManager l = getParent().getLayout();
        if (l instanceof Map.InsetLayout) {
          l.layoutContainer(getParent());
        }
      }
    }
  }

  /**
   * The Map view that appears inside the ScrollPane
   */
  protected class View extends JPanel implements MouseListener {
    private static final long serialVersionUID = 1L;

    @Override
    protected void paintComponent(Graphics g) {
      final Graphics2D g2d = (Graphics2D) g;

      g2d.addRenderingHints(SwingUtils.FONT_HINTS);
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON);

      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

      // HDPI: We may get a transform where scale != 1. This means we
      // are running on an HDPI system. We want to draw at the effective
      // scale factor to prevent poor quality upscaling, so reset the
      // transform to scale of 1 and multiply the map zoom by the OS scaling.
      final AffineTransform orig_t = g2d.getTransform();
      g2d.setTransform(SwingUtils.descaleTransform(orig_t));

      final double dscale = scale * os_scale;

      map.drawBoards(
        g,
        -Math.round((float) dscale * map.getEdgeBuffer().width),
        -Math.round((float) dscale * map.getEdgeBuffer().height),
        dscale,
        this
      );

      for (final GamePiece gp : map.getPieces()) {
        final Point p = mapToDrawing(gp.getPosition(), os_scale);
        gp.draw(g, p.x, p.y, this, dscale);
      }

      mouseOverViewer.draw(g, map);

      // Draw a rectangle indicating the present viewing area
      g.setColor(rectColor);

      final Rectangle vr = mapToDrawing(map.componentToMap(map.getView().getVisibleRect()), os_scale);
      g.drawRect(vr.x, vr.y, vr.width, vr.height);
      g.drawRect(vr.x - 1, vr.y - 1, vr.width + 2, vr.height + 2);

      g2d.setTransform(orig_t);
    }

    @Override
    public void mousePressed(MouseEvent e) {
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    // FIXME: mouseClicked()?
    @Override
    public void mouseReleased(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        map.centerAt(componentToMap(e.getPoint()));
      }
    }

    @Override
    public Dimension getPreferredSize() {
      return new Dimension(
        (int)((map.mapSize().width - 2 * map.getEdgeBuffer().width) * scale),
        (int)((map.mapSize().height - 2 * map.getEdgeBuffer().height) * scale)
      );
    }
  }

  @Override
  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, "GlobalMap");  //NON-NLS
    }
    return myI18nData;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(getAttributeValueString(BUTTON_TEXT), getAttributeValueString(TOOLTIP));
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(HOTKEY)));
  }

  /**
   * {@link SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Collections.emptyList();
  }


  /**
   * @return names of all images used by the component and any subcomponents
   */
  @Override
  public SortedSet<String> getAllImageNames() {
    final TreeSet<String> s =
      new TreeSet<>(String.CASE_INSENSITIVE_ORDER);

    addImageNamesRecursively(s);
    return s;
  }

  /**
   * Adds all images used by this component AND any children (or inner decorators/pieces) to the collection.
   * @param s Collection to add image names to
   */
  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    addLocalImageNames(s); // Default implementation just adds ours
  }

  /**
   * @return names of all images used by this component
   */
  @Override
  public SortedSet<String> getLocalImageNames() {
    final TreeSet<String> s =
      new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
    addLocalImageNames(s);
    return s;
  }

  /**
   * Classes extending {@link VASSAL.build.AbstractBuildable} should override this method in order to add
   * the names of any image files they use to the collection. For "find unused images" and "search".
   *
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    final String imageName = getAttributeValueString(ICON_NAME);
    if (imageName != null) {
      s.add(imageName);
    }
  }
}
