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
package VASSAL.build.module.map;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyBoundsAdapter;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
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
import VASSAL.i18n.Translatable;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.ScrollPane;

/**
 * This is scaled version of a {@link Map}that gives an overview. Users can
 * navigate around the Map by clicking on the GlobalMap, which draws a square
 * indicating the current viewable area in the map window
 */
public class GlobalMap extends JPanel implements AutoConfigurable, GameComponent, Drawable {
  private static final long serialVersionUID = 1L;

  protected Map map;
  protected double scale = 0.19444444; // Zoom factor
  protected Color rectColor = Color.black;
  protected LaunchButton launch;

  protected JWindow f;
  protected CounterDetailViewer mouseOverViewer;
  protected View view;
  protected JPanel borderPanel;
  protected JScrollPane scroll;
  protected boolean scrollVisible;
  protected ComponentI18nData myI18nData;

  public GlobalMap() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setWindowVisible(!f.isVisible());
      }
    };
    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON_NAME, al);
    launch.setAttribute(TOOLTIP, "Show/Hide overview window");
    launch.setAttribute(HOTKEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, KeyEvent.CTRL_MASK + KeyEvent.SHIFT_MASK));

    view = new View();
    view.addMouseListener(view);

    scroll = new ScrollPane(view);
    borderPanel = new JPanel(new BorderLayout());
    borderPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
    borderPanel.add(scroll, BorderLayout.CENTER);

  }

  private void initWindow() {
    f = new JWindow(SwingUtilities.getWindowAncestor(map.getView()));
    f.getContentPane().add(borderPanel);

    view.setSize(getPreferredSize());
    view.setPreferredSize(getPreferredSize());

    scroll.setPreferredSize(getViewableSize());
    f.pack();
    map.getView().addHierarchyBoundsListener(new HierarchyBoundsAdapter() {
      public void ancestorMoved(HierarchyEvent e) {
        adjustWindowLocation();
      }

      public void ancestorResized(HierarchyEvent e) {
        adjustWindowLocation();
      }
    });
  }

  /**
   * Expects to be added to a {@link Map}. Adds itself as a {@link
   * GameComponent} and a {@link Drawable}component
   */
  public void addTo(Buildable b) {
    map = (Map) b;

    mouseOverViewer = new CounterViewer();

    GameModule.getGameModule().getGameState().addGameComponent(this);

    GameModule.getGameModule().addKeyStrokeSource(new KeyStrokeSource(view, JComponent.WHEN_FOCUSED));

    map.addDrawComponent(this);

    map.getToolBar().add(launch);
    
    if (b instanceof Translatable) {
      getI18nData().setOwningComponent((Translatable) b);
    }

    if (SwingUtilities.getWindowAncestor(map.getView()) != null) {
      initWindow();
    }
    else {
      f = new JWindow(); // Dummy window until the parent map is added to a
      // Window of its own
      map.getView().addHierarchyListener(new HierarchyListener() {
        public void hierarchyChanged(HierarchyEvent e) {
          if (SwingUtilities.getWindowAncestor(map.getView()) != null) {
            initWindow();
            map.getView().removeHierarchyListener(this);
          }
        }
      });
    }
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
    if (f != null) {
      f.dispose();
    }
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
    return new String[] {TOOLTIP, BUTTON_TEXT, ICON_NAME, HOTKEY, SCALE, COLOR};
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
      return "" + scale;
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(rectColor);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Tooltip text", "Button text", "Button icon", "Hotkey to show/hide", "Scale factor", "Visible rectangle highlight color"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, IconConfig.class, KeyStroke.class, Double.class, Color.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, DEFAULT_ICON);
    }
  }

  public void draw(Graphics g, Map m) {
    view.repaint();
  }

  public boolean drawAboveCounters() {
    return false;
  }

  /**
   * Transform a point from Map coordinates to coordinates in the overview
   * window
   * 
   * @param p
   * @return
   */
  public Point componentCoordinates(Point p) {
    p = new Point(p.x - map.getEdgeBuffer().width, p.y - map.getEdgeBuffer().height);
    p.x *= scale;
    p.y *= scale;
    return p;
  }

  /**
   * Transform a point from coordinates in the overview window to Map
   * coordinates
   * 
   * @param p
   * @return
   */
  public Point mapCoordinates(Point p) {
    p = new Point((int) Math.round(p.x / scale), (int) Math.round(p.y / scale));
    p.translate(map.getEdgeBuffer().width, map.getEdgeBuffer().height);
    return p;
  }

  public String getToolTipText(MouseEvent e) {
    return null;
  }

  public Command getRestoreCommand() {
    return null;
  }

  /*
   * Return the display size of the entire zoomed overview map
   */
  public Dimension getPreferredSize() {
    return new Dimension((int) ((map.mapSize().width - 2 * map.getEdgeBuffer().width) * scale),
        (int) ((map.mapSize().height - 2 * map.getEdgeBuffer().height) * scale));
  }

  /*
   * Return the maximum size we have to display the overview map, keeping it
   * inside the main control window and taking into account the width of the
   * border on the panel enclosing the map.
   */
  public Dimension getViewableSize() {
    Dimension d = getPreferredSize();
    Rectangle r = map.getView().getVisibleRect();
    r.width -= 2 * borderPanel.getBorder().getBorderInsets(borderPanel).right;
    r.height -= 2 * borderPanel.getBorder().getBorderInsets(borderPanel).bottom;
    setScrollVisible(false);
    if (r.width > 0 && r.width < d.width) {
      d.width = r.width;
      setScrollVisible(true);
    }
    else {
      d.width += 1 - map.getBoardPicker().getColumnCount();
      ;
    }

    if (r.height > 0 && r.height < d.height) {
      d.height = r.height;
      setScrollVisible(true);
    }
    else {
      d.height += 1 - map.getBoardPicker().getRowCount();
    }

    return d;
  }

  public boolean isScrollVisible() {
    return scrollVisible;
  }

  public void setScrollVisible(boolean b) {
    scrollVisible = b;
  }

  public void setup(boolean show) {
    if (!show) {
      f.setVisible(false);
    }
    else {
      scroll.setPreferredSize(getViewableSize());
    }

    if (show && map.getComponents(CounterDetailViewer.class).hasMoreElements()) {
      view.addMouseMotionListener(mouseOverViewer);
      f.addKeyListener(mouseOverViewer);
    }
    else {
      view.removeMouseMotionListener(mouseOverViewer);
      f.removeKeyListener(mouseOverViewer);
    }
  }

  public void setWindowVisible(final boolean visible) {
    if (visible) {
      adjustWindowLocation();
    }
    f.setVisible(visible);
  }

  /*
   * The main map window has either moved or resized. Recalculate the size of
   * all components. JScrollPane AS_NEEDED scroll bar policy forces scroll bars
   * to appear when client is exactly the right size, so need to override this
   * and force Scrollbar visiblity off when entire minimap is showing.
   */
  protected void adjustWindowLocation() {
    if (map.getView().isShowing()) {
      Point p = map.getView().getLocationOnScreen();
      p.translate(map.getView().getVisibleRect().x, map.getView().getVisibleRect().y);
      f.setLocation(p);
      f.setSize(getViewableSize());

      view.setSize(getPreferredSize());
      view.setPreferredSize(getPreferredSize());

      scroll.setPreferredSize(getViewableSize());
      scroll.setSize(getViewableSize());

      if (isScrollVisible()) {
        scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
      }
      else {
        scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
      }
      f.invalidate();
      f.pack();
    }
  }

  public static String getConfigureTypeName() {
    return "Overview Window";
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

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
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
    }

    public void draw(Graphics g, Map map) {
      if (currentMousePosition != null) {
        this.draw(g, currentMousePosition.getPoint(), GlobalMap.this);
      }
    }

    protected ArrayList<GamePiece> getDisplayablePieces() {
      Point oldPoint = currentMousePosition.getPoint();
      Point mapPoint = GlobalMap.this.map.componentCoordinates(mapCoordinates(oldPoint));

      currentMousePosition.translatePoint(mapPoint.x - oldPoint.x, mapPoint.y - oldPoint.y);
      ArrayList<GamePiece> l = super.getDisplayablePieces();
      currentMousePosition.translatePoint(oldPoint.x - mapPoint.x, oldPoint.y - mapPoint.y);
      return l;
    }

    protected double getZoom() {
      return scale;
    }

  }

  /*
   * Map view that appears inside the Scrollpane
   */
  protected class View extends JPanel implements MouseListener {
    private static final long serialVersionUID = 1L;

    public void paint(Graphics g) {
      g.clearRect(0, 0, getViewableSize().width, getViewableSize().height);
      map.drawBoards(g, -Math.round((float) scale * map.getEdgeBuffer().width), -Math.round((float) scale * map.getEdgeBuffer().height), scale, this);
      GamePiece stack[] = map.getPieces();
      for (int i = 0; i < stack.length; i++) {
        Point p = componentCoordinates(stack[i].getPosition());
        stack[i].draw(g, p.x, p.y, this, scale);
      }
      mouseOverViewer.draw(g, map);

      // Draw a rectangle indicating the present viewing area
      g.setColor(rectColor);

      Rectangle r = map.getView().getVisibleRect();
      Point ul = map.mapCoordinates(r.getLocation());
      ul = componentCoordinates(ul);
      int w = (int) (scale * r.width / map.getZoom());
      int h = (int) (scale * r.height / map.getZoom());
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
      map.repaint();
    }

  }
  
  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, "GlobalMap");
    }
    return myI18nData;
  }
}
