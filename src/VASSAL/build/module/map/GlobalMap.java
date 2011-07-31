/*
 * $Id$
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;

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

/**
 * This is scaled version of a {@link Map} that gives an overview.
 * Users can navigate around the Map by clicking on the GlobalMap, which
 * draws a rectangular region of interest (ROI) indicating the current
 * viewable area in the map window.
 */
public class GlobalMap implements AutoConfigurable,
                                  GameComponent,
                                  Drawable {
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

    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        scroll.setVisible(!scroll.isVisible());
      }
    };

    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT,
                              HOTKEY, ICON_NAME, al);
    launch.setAttribute(TOOLTIP, "Show/Hide overview window");
    launch.setAttribute(HOTKEY,
      NamedKeyStroke.getNamedKeyStroke(KeyEvent.VK_O,
                             KeyEvent.CTRL_MASK + KeyEvent.SHIFT_MASK));
  }

  /**
   * Expects to be added to a {@link Map}. Adds itself as a {@link
   * GameComponent} and a {@link Drawable}component
   */
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

  public void add(Buildable b) {
  }

  public void remove(Buildable b) {
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.removeDrawComponent(this);
    map.getToolBar().remove(launch);
    GameModule.getGameModule().getGameState().removeGameComponent(this);

    map.getLayeredPane().remove(scroll);
  }

  public void build(Element e) {
    AutoConfigurable.Util.buildAttributes(e, this);
  }

  protected static final String SCALE = "scale";
  protected static final String COLOR = "color";
  protected static final String HOTKEY = "hotkey";
  protected static final String ICON_NAME = "icon";
  protected static final String TOOLTIP = "tooltip";
  protected static final String BUTTON_TEXT = "buttonText";
  protected static final String DEFAULT_ICON = "/images/overview.gif";

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

  public VisibilityCondition getAttributeVisibility(String name) {
    return null;
  }

  public void setAttribute(String key, Object value) {
    if (SCALE.equals(key)) {
      if (value instanceof String) {
        value = Double.valueOf((String) value);
      }
      scale = ((Double) value).doubleValue();
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
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, DEFAULT_ICON);
    }
  }

  public void draw(Graphics g, Map m) {
    view.repaint();
  }

  public boolean drawAboveCounters() {
    return true;
  }

  /**
   * Transform a point from Map coordinates to coordinates in the overview
   * window
   *
   * @param p
   * @return
   */
  public Point componentCoordinates(Point p) {
    return new Point((int) ((p.x - map.getEdgeBuffer().width) * scale),
                     (int) ((p.y - map.getEdgeBuffer().height) * scale));
  }

  /**
   * Transform a point from coordinates in the overview window to Map
   * coordinates
   *
   * @param p
   * @return
   */
  public Point mapCoordinates(Point p) {
    return new Point(
      (int) Math.round(p.x / scale) + map.getEdgeBuffer().width,
      (int) Math.round(p.y / scale) + map.getEdgeBuffer().height);
  }

  public String getToolTipText(MouseEvent e) {
    return null;
  }

  public Command getRestoreCommand() {
    return null;
  }

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

  public String getConfigureName() {
    return null;
  }

  public Configurer getConfigurer() {
    return new AutoConfigurer(this);
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "OverviewWindow");
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return AutoConfigurable.Util.getBuildElement(doc, this);
  }

  protected class CounterViewer extends CounterDetailViewer {
    public CounterViewer() {
      this.map = GlobalMap.this.map;
      this.view = GlobalMap.this.view;
    }

    protected List<GamePiece> getDisplayablePieces() {
      final Point oldPoint = currentMousePosition.getPoint();
      final Point mapPoint =
        GlobalMap.this.map.componentCoordinates(mapCoordinates(oldPoint));

      currentMousePosition.translatePoint(mapPoint.x - oldPoint.x,
                                          mapPoint.y - oldPoint.y);
      final List<GamePiece> l = super.getDisplayablePieces();
      currentMousePosition.translatePoint(oldPoint.x - mapPoint.x,
                                          oldPoint.y - mapPoint.y);
      return l;
    }

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

      super.setBounds(0,0,realSize.width,realSize.height);
    }

    /**
     * This funcion is overridden to make sure that the parent layout
     * is redone when the GlobalMap is shown.
     */
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
      map.drawBoards(g,
                     -Math.round((float) scale * map.getEdgeBuffer().width),
                     -Math.round((float) scale * map.getEdgeBuffer().height),
                     scale, this);

      for (GamePiece gp : map.getPieces()) {
        Point p = componentCoordinates(gp.getPosition());
        gp.draw(g, p.x, p.y, this, scale);
      }

      mouseOverViewer.draw(g, map);

// FIXME: use a Graphics2D for this
      // Draw a rectangle indicating the present viewing area
      g.setColor(rectColor);

      final Rectangle r = map.getView().getVisibleRect();
      final Point ul =
        componentCoordinates(map.mapCoordinates(r.getLocation()));
      final int w = (int) (scale * r.width / map.getZoom());
      final int h = (int) (scale * r.height / map.getZoom());
      g.drawRect(ul.x, ul.y, w, h);
      g.drawRect(ul.x - 1, ul.y - 1, w + 2, h + 2);
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mouseClicked(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
      map.centerAt(mapCoordinates(e.getPoint()));
    }

    public Dimension getPreferredSize() {
      return new Dimension(
        (int)((map.mapSize().width - 2*map.getEdgeBuffer().width) * scale),
        (int)((map.mapSize().height - 2*map.getEdgeBuffer().height) * scale));
    }
  }

  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, "GlobalMap");
    }
    return myI18nData;
  }
}
