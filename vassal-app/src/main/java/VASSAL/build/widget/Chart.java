/*
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
package VASSAL.build.widget;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLayer;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.plaf.LayerUI;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.LevelConfigurer;
import VASSAL.configure.Levels;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.image.TileRenderer;
import VASSAL.tools.imageop.ScaleOp;
import VASSAL.tools.imageop.SourceOp;
import VASSAL.tools.swing.SwingUtils;

/**
 * A Chart is used for displaying charts and tables for the module.
 * The charts are loaded as images stored in the DataArchive. As a subclass
 * of Widget, a Chart may be added to any Widget, but it may not contain
 * children of its own.
 */
public class Chart extends Widget {
  public static final String NAME = "chartName"; //NON-NLS
  public static final String FILE = "fileName"; //NON-NLS
  public static final String ZOOM_START = "zoomStart"; //$NON-NLS-1$
  public static final String ZOOM_LEVELS = "zoomLevels"; //$NON-NLS-1$
  public static final String ZOOM_IN_KEY = "zoomInKey"; //NON-NLS
  public static final String ZOOM_OUT_KEY = "zoomOutKey"; //NON-NLS
  public static final String ZOOM_START_KEY = "zoomStartKey"; //NON-NLS
  public static final String ZOOM_BUTTONS = "zoomButtons"; //NON-NLS
  public static final String ZOOM_BUTTON_OPACITY = "zoomButtonOpacity"; //NON-NLS
  public static final String ZOOM_BUTTON_WIDTH = "zoomButtonWidth"; //NON-N
  public static final String ZOOM_BUTTON_HEIGHT = "zoomButtonHeight"; //NON-N
  public static final String ZOOM_BUTTON_BACKGROUND = "zoomButtonBackground"; //NON-N
  public static final String ZOOM_BUTTON_FOREGROUND = "zoomButtonForeground"; //NON-N
  
  private JLayer<JScrollPane> layer;
  private JScrollPane scroll;
  private String fileName;
  private SourceOp srcOp;
  private ScaleOp scaleOp;
  private View view;
  private State state = new State(defaultZoomLevels, defaultInitialZoomLevel);

  private static final Point2D ORIGIN = new Point2D.Double(0.0, 0.0);

/*
  // zoom levels produced by this:

  private static final double[] defaultZoomLevels = IntStream.range(-8, 12)
    .mapToDouble(i -> Math.round(Math.exp(i / 10.0) * 100) / 100.0)
    .toArray();

  // except that e^0.7 and e^1.1 are rounded to 2.0 and 3.0, respectively
*/

  private static final double[] defaultZoomLevels = {
    0.45,
    0.5,
    0.55,
    0.61,
    0.67,
    0.74,
    0.82,
    0.9,
    1.0,
    1.11,
    1.22,
    1.35,
    1.49,
    1.65,
    1.82,
    2.0,
    2.23,
    2.46,
    2.72,
    3.0
  };

  protected static final int defaultInitialZoomLevel = 8;

  private static class State implements Levels {
    private double[] levels;
    private int cur;
    private int initial;

    public State(double[] levels, int initial) {
      this.levels = levels;
      Arrays.sort(this.levels);

      cur = this.initial = initial;
    }

    public State(Collection<Double> l, int initial) {
      levels = new double[l.size()];

      int i = 0;
      for (final Double d : l) levels[i++] = d;
      Arrays.sort(levels);

      cur = this.initial = initial;
    }

    @Override
    public int getInitialLevel() {
      return initial;
    }

    @Override
    public int getLevelCount() {
      return levels.length;
    }

    @Override
    public List<Double> getLevels() {
      final List<Double> l = new ArrayList<>(levels.length);
      for (final double d : levels) l.add(d);
      return l;
    }

    @Override
    public void reset(List<Double> l, int i) {
      levels = l.stream().mapToDouble(d -> d).toArray();
      Arrays.sort(levels);
      cur = initial = i;
    }

    public void lowerLevel() {
      cur = Math.max(0, cur - 1);
    }

    public void higherLevel() {
      cur = Math.min(cur + 1, levels.length - 1);
    }

    public void initialLevel() {
      cur = initial;
    }

    public double getZoom() {
      return levels[cur];
    }
  }

  private final Rectangle boundaries = new Rectangle();

  private final TileRenderer renderer = new TileRenderer();

  // nobody wants to hit shift to get plus
  private NamedKeyStroke zoomInKey = NamedKeyStroke.of(
    KeyEvent.VK_EQUALS,
    InputEvent.CTRL_DOWN_MASK
  );

  private NamedKeyStroke zoomOutKey = NamedKeyStroke.of(
    KeyEvent.VK_MINUS,
    InputEvent.CTRL_DOWN_MASK
  );
  
  private NamedKeyStroke zoomStartKey = NamedKeyStroke.of(
    KeyEvent.VK_0,
    InputEvent.CTRL_DOWN_MASK
  );

  private boolean zoomButtons = true;
  private int zoomButtonOpacity = 32;
  private int zoomButtonWidth = 20;
  private int zoomButtonHeight = 20;
  private Color zoomButtonBackground = new Color(128, 128, 128);
  private Color zoomButtonForeground = new Color(212, 212, 212);
  

  /**
   * Base class for zoom buttons
   */
  abstract class AbstractZoomButton extends JButton {
    protected static int serialVersionUID = -1;
    private int alpha = zoomButtonOpacity;
    private Shape shape;
    private final String text;
    private final int yoff;
    
    public AbstractZoomButton(String          text,
                              int             yoff,
                              Dimension       size,
                              NamedKeyStroke  keyStroke) {
      this.text = text;
      this.yoff = yoff;
      
      setPreferredSize(size);
      setMaximumSize(size);
      setMinimumSize(size);
      setAlignmentX(CENTER_ALIGNMENT);
      setOpaque(false);
      setContentAreaFilled(false);
      setBorderPainted(false);
      setFocusPainted(false);
      setForeground(zoomButtonForeground);
      setBackground(zoomButtonBackground);

      if (!keyStroke.isNamed()) {
        setToolTipText("[" + NamedHotKeyConfigurer.getString(keyStroke) + "]");
      }
    }

    public void setAlpha(int alpha) {
      this.alpha = alpha;
      repaint();
    }

    public abstract Shape getShape(int w, int h);

    @Override
    public boolean contains(int x, int y) {
      if (shape == null) {
        shape = getShape(getWidth(), getHeight());
      }
      return shape.contains(x, y);
    }

    protected Color getBG(boolean pressed,
                          boolean rollover) {
      final Color bg = getBackground();

      if (pressed) {
        final Color dbg = bg.darker();
        return new Color(dbg.getRed(),
                         dbg.getGreen(),
                         dbg.getBlue(),
                         Math.min(255, alpha + 30));
      }

      if (rollover) {
        final Color lbg = bg.brighter();
        return new Color(lbg.getRed(),
                         lbg.getGreen(),
                         lbg.getBlue(),
                         Math.min(255, alpha + 20));
      }

      return new Color(bg.getRed(),
                       bg.getGreen(),
                       bg.getBlue(),
                       alpha);
    }
      
      
      
    @Override
    protected void paintComponent(Graphics g) {
      final Graphics2D g2 = (Graphics2D) g.create();
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                          RenderingHints.VALUE_ANTIALIAS_ON);

      final int w = getWidth();
      final int h = getHeight();
      shape       = getShape(w, h);

      final Color bgColor = getBG(getModel().isPressed(),
                                  getModel().isRollover());

      // Fill vector shape
      g2.setColor(bgColor);
      g2.fill(shape);

      // White outline for contrast
      final Color fg = getForeground();
      g2.setColor(new Color(fg.getRed(), fg.getGreen(), fg.getBlue(),
                            Math.min(255, alpha + 50)));
      g2.setStroke(new BasicStroke(1.2f));
      g2.draw(shape);

      // Draw large centered text
      g2.setColor(new Color(fg.getRed(), fg.getGreen(), fg.getBlue(),
                            Math.min(255, alpha + 80)));
      g2.setFont(new Font("SansSerif", Font.BOLD, 22));

      final FontMetrics fm      = g2.getFontMetrics();
      final int         width   = fm.stringWidth(text);
      final int         ascent  = fm.getAscent();
      final int         descent = fm.getDescent();
      final int         x       = (w - width) / 2;
      final int         y       = (h + (ascent - descent)) / 2 + yoff;

      g2.drawString(text, x, y);
      g2.dispose();
    }
  }

  /**
   * Zoom in button
   */
  public class ZoomInButton extends AbstractZoomButton {
    protected static int serialVersionUID = -1;
    public ZoomInButton(Dimension       size,
                        NamedKeyStroke  keyStroke) {
      super("+", 2, size, keyStroke);
    }

    @Override
    public Shape getShape(int w, int h) {
      final int gap = 1; 
      // Half-circle top dome with flat, open bottom
      final Path2D path = new Path2D.Float();
      path.moveTo(0, h - gap);
      path.lineTo(0, w / 2.0);
      path.curveTo(0, 0, w, 0, w, w / 2.0);
      path.lineTo(w, h - gap);
      path.closePath();
      return path;      
    }
  }
  
  /**
   * Zoom out button
   */
  public class ZoomOutButton extends AbstractZoomButton {
    protected static int serialVersionUID = -1;
    public ZoomOutButton(Dimension       size,
                         NamedKeyStroke  keyStroke) {
      super("-", -2, size, keyStroke);
    }

    @Override
    public Shape getShape(int w, int h) {
      final int gap = 1;
      final Path2D path = new Path2D.Float();
      path.moveTo(0, gap);
      path.lineTo(w, gap);
      path.lineTo(w, h - (w / 2.0));
      path.curveTo(w, h, 0, h, 0, h - (w / 2.0));
      path.closePath();
      return path;      
    }
  }
  
  /**
   * Zoom reset button
   */
  public class ZoomResetButton extends AbstractZoomButton {
    protected static int serialVersionUID = -1;
    public ZoomResetButton(Dimension       size,
                           NamedKeyStroke  keyStroke) {
      super("=", 0, size, keyStroke);
    }

    @Override
    public Shape getShape(int w, int h) {
      final int gap = 1;
      return new Rectangle2D.Float(0, gap, w, h - (2 * gap));    }
  }
  

  
  /**
   * Panel with zoom buttons.
   */
  class OverlayControlPanel extends JPanel {
    protected static int serialVersionUID = -1;
    // private static final int TRANSPARENT = 64;
    private static final int OPAQUE      = 192;
    private int alpha = zoomButtonOpacity;

    public OverlayControlPanel() {
      setOpaque(false);
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      
      final MouseAdapter hoverListener = new MouseAdapter() {
        @Override
        public void mouseEntered(MouseEvent e) {
          setAlpha(OPAQUE);
        }

        @Override
        public void mouseExited(MouseEvent e) {
          if (!getBounds().contains(e.getPoint())) {
            setAlpha(zoomButtonOpacity);
          }
        }
      };

      addMouseListener(hoverListener);
 
      final Dimension          btnSize   = new Dimension(zoomButtonWidth,
                                                         zoomButtonHeight);
      final AbstractZoomButton zoomIn    = new ZoomInButton(btnSize, zoomInKey);
      final AbstractZoomButton zoomOut   = new ZoomOutButton(btnSize, zoomOutKey);
      final AbstractZoomButton zoomReset = new ZoomResetButton(btnSize, zoomStartKey);

      // Attach your zoom actions here
      zoomIn.addActionListener(e -> zoom(+1));
      zoomOut.addActionListener(e -> zoom(-1));
      zoomReset.addActionListener(e -> zoom(0));
      zoomIn.addMouseListener(hoverListener);
      zoomOut.addMouseListener(hoverListener);
      zoomReset.addMouseListener(hoverListener);

      // Add buttons with spacing
      add(zoomIn);
      add(zoomReset);
      add(zoomOut);

      setAlpha(alpha);
      setVisible(zoomButtons);
    }

    /**
     * Sets the transparency of the zoom interface
     */
    private void setAlpha(int alpha) {
      this.alpha = alpha;
      for (final Component c : getComponents()) {
        if (c instanceof AbstractZoomButton) {
          ((AbstractZoomButton)c).setAlpha(alpha);
        }
      }
      repaint();
    }
  }

  /**
   * LayerUI that puts the zoom buttons on the overlay at the
   * bottom-right corner of the view.
   */
  static class OverlayLayerUI extends LayerUI<JScrollPane> {
    protected static int serialVersionUID = -1;
    private final JPanel overlay;
    private static final int MARGIN = 8; // Distance to edge

    public OverlayLayerUI(JPanel overlay) {
      this.overlay = overlay;
    }

    @Override
    public void installUI(JComponent c) {
      super.installUI(c);
      final JLayer<?> layer = (JLayer<?>) c;
      layer.getGlassPane().setLayout(null);
      layer.getGlassPane().add(overlay);
      layer.getGlassPane().setVisible(true);
    }

    @Override
    public void uninstallUI(JComponent c) {
      final JLayer<?> layer = (JLayer<?>) c;
      layer.getGlassPane().remove(overlay);
      super.uninstallUI(c);
    }

    @Override
    public void doLayout(JLayer<? extends JScrollPane> layer) {
      super.doLayout(layer);

      final JScrollPane scroll = layer.getView();
      if (scroll == null) {
        return;
      }

      final Dimension  size = overlay.getPreferredSize();
      final JScrollBar vBar = scroll.getVerticalScrollBar();
      final JScrollBar hBar = scroll.getHorizontalScrollBar();

      final int        vW   = (vBar != null && vBar.isVisible()) ? vBar.getWidth() : 0;
      final int        hH   = (hBar != null && hBar.isVisible()) ? hBar.getHeight() : 0;

      final int x           = layer.getWidth() - size.width - MARGIN - vW;
      final int y           = layer.getHeight() - size.height - MARGIN - hH;

      overlay.setBounds(x, y, size.width, size.height);
    }
  }
  
  /**
   * View of the image
   */
  private class View extends JPanel {
    private static final long serialVersionUID = 1L;

    @Override
    public Dimension getPreferredSize() {
      if (srcOp == null) {
        return new Dimension(0, 0);
      }

      final Dimension size = srcOp.getSize();
      size.width *= state.getZoom();
      size.height *= state.getZoom();
      return size;
    }

    @Override
    public void paint(Graphics g) {
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

      final Rectangle vr = getVisibleRect();

      final Rectangle r = new Rectangle(
        (int)(vr.x * os_scale),
        (int)(vr.y * os_scale),
        (int)(vr.width * os_scale),
        (int)(vr.height * os_scale)
      );

      g2d.setColor(getBackground());
      g2d.fillRect(r.x, r.y, r.width, r.height);

      final double dzoom = state.getZoom() * os_scale;

      scaleOp = renderer.renderRegion(
        g,
        ORIGIN,
        r,
        dzoom,
        boundaries,
        1.0,
        srcOp,
        scaleOp,
        null,
        false,
        false,
        null,
        this,
        this
      );

      g2d.setTransform(orig_t);
    }

    @Override
    public void update(Graphics g) {
      // To avoid flicker, don't clear the display first
      paint(g);
    }
  }

  @Override
  public Component getComponent() {
    if (layer == null) {
      view = new View();
      boundaries.setSize(srcOp != null ? srcOp.getSize() : new Dimension(0, 0));

      final Dimension vpref = view.getPreferredSize();

      scroll = new AdjustableSpeedScrollPane(view);
      scroll.getViewport().setPreferredSize(vpref);
      scroll.getViewport().setAlignmentY(0.0F);

      final OverlayControlPanel buttons = new OverlayControlPanel();
      final OverlayLayerUI      layerui = new OverlayLayerUI(buttons);
      layer                             = new JLayer<>(scroll, layerui);

      // Note, we filter on whether the event originated in this
      // window (actually it's parent).
      final NamedKeyStrokeListener zoomIn  = new NamedKeyStrokeListener(e -> {
        if (fromThisWindow(e)) {
          zoom(1);
        }
      });

      final NamedKeyStrokeListener zoomOut = new NamedKeyStrokeListener(e -> {
        if (fromThisWindow(e)) {
          zoom(-1);
        }
      });

      final NamedKeyStrokeListener zoomStart = new NamedKeyStrokeListener(e -> {
        if (fromThisWindow(e)) {
          zoom(0);
        }
      });

      zoomIn.setKeyStroke(zoomInKey);
      zoomOut.setKeyStroke(zoomOutKey);
      zoomStart.setKeyStroke(zoomStartKey);

      GameModule.getGameModule().addKeyStrokeListener(zoomIn);
      GameModule.getGameModule().addKeyStrokeListener(zoomOut);
      GameModule.getGameModule().addKeyStrokeListener(zoomStart);

      view.addMouseWheelListener(new MouseWheelListener() {
        @Override
        public void mouseWheelMoved(MouseWheelEvent e) {
          if (e.getScrollAmount() == 0) {
            return;
          }

          if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL && SwingUtils.isSelectionToggle(e)) {
            final int units = e.getUnitsToScroll();

            if (units < 0) {
              zoom(1);
            }
            else if (units > 0) {
              zoom(-1);
            }
          }

          scroll.getViewport().dispatchEvent(e); // So that the scrollbars can still find our event.
        }
      });

    }
    return layer;
  }

  private void zoom(int direction) {
    // ensure that we maintain the center of the view, if possible
    Rectangle vr = view.getVisibleRect();
    double cx = vr.getCenterX();
    double cy = vr.getCenterY();

    double zoom = state.getZoom();

    cx /= zoom;
    cy /= zoom;

    if (direction > 0) {
      state.higherLevel();
    }
    else if (direction < 0) {
      state.lowerLevel();
    }
    else {
      state.initialLevel();
    }

    zoom = state.getZoom();

    cx *= zoom;
    cy *= zoom;

    final Dimension d = view.getPreferredSize();
    view.setBounds(0, 0, d.width, d.height);

    vr = view.getVisibleRect();

    cx = Math.round(cx - vr.width / 2.0);
    cy = Math.round(cy - vr.height / 2.0);
    
    scroll.getViewport().setViewPosition(new Point((int) cx, (int) cy));

    view.repaint();
  }

  /**
   * Check if event originated in this component.
   *
   * @param e Event to check
   */
  protected boolean fromThisWindow(ActionEvent e) {
    return e.getSource() instanceof Component &&
      SwingUtilities.getWindowAncestor((Component) e.getSource()) ==
      SwingUtilities.getWindowAncestor(view);
  }

  public String getFileName() {
    return fileName;
  }

  @Override
  public void addTo(Buildable parent) {
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.html", "Chart"); //NON-NLS
  }

  public static class LevelConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new LevelConfigurer(((Chart) c).state, key, name);
    }
  }

  @Override
  public void setAttribute(String key, Object val) {
    if (NAME.equals(key)) {
      setConfigureName((String) val);
    }
    else if (FILE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
      }
      fileName = (String) val;
      srcOp = renderer.loadImage(fileName);

      if (view != null) {
        view.revalidate();
      }
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) val;
    }
    else if (ZOOM_START.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }

      if (val != null) {
        // Notes:
        //
        // 1. ZOOM_START is one-based, not zero-based.
        // 2. The levels in state run from zoomed out to zoomed in,
        // while the levels coming from outside Zoomer run from
        // zoomed in to zoomed out. Hence we reverse the initial
        // zoom level being set here.
        //
        final List<Double> levels = state.getLevels();
        final int initial =
          Math.max(0, Math.min(levels.size() - 1, levels.size() - (Integer) val));

        state = new State(levels, initial);
      }
    }
    else if (ZOOM_LEVELS.equals(key)) {
      if (val instanceof String) {
        val = StringArrayConfigurer.stringToArray((String) val);
      }

      if (val != null) {
        // dump into a set to remove duplicates
        final Set<Double> levels = new HashSet<>();
        for (final String s : (String[]) val) {
          levels.add(Double.valueOf(s));
        }

        state = new State(levels,
          Math.min(state.getInitialLevel(), levels.size() - 1));
      }
    }
    else if (ZOOM_IN_KEY.equals(key)) {
      if (val instanceof String) {
        val = NamedHotKeyConfigurer.decode((String) val);
      }
      zoomInKey = (NamedKeyStroke) val;
    }
    else if (ZOOM_OUT_KEY.equals(key)) {
      if (val instanceof String) {
        val = NamedHotKeyConfigurer.decode((String) val);
      }
      zoomOutKey = (NamedKeyStroke) val;
    }
    else if (ZOOM_START_KEY.equals(key)) {
      if (val instanceof String) {
        val = NamedHotKeyConfigurer.decode((String) val);
      }
      zoomStartKey = (NamedKeyStroke) val;
    }
    else if (ZOOM_BUTTONS.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String)val);
      }
      zoomButtons = (Boolean)val;
    }
    else if (ZOOM_BUTTON_OPACITY.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String)val);
      }
      zoomButtonOpacity = (Integer)val;
    }
    else if (ZOOM_BUTTON_WIDTH.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String)val);
      }
      zoomButtonWidth = (Integer)val;
    }
    else if (ZOOM_BUTTON_HEIGHT.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String)val);
      }
      zoomButtonHeight = (Integer)val;
    }
    else if (ZOOM_BUTTON_BACKGROUND.equals(key)) {
      if (val instanceof String) {
        val = ColorConfigurer.stringToColor((String)val);
      }
      zoomButtonBackground = (Color)val;
    }
    else if (ZOOM_BUTTON_FOREGROUND.equals(key)) {
      if (val instanceof String) {
        val = ColorConfigurer.stringToColor((String)val);
      }
      zoomButtonForeground = (Color)val;
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (ZOOM_BUTTON_OPACITY.equals(name) ||
        ZOOM_BUTTON_WIDTH.equals(name) ||
        ZOOM_BUTTON_HEIGHT.equals(name) ||
        ZOOM_BUTTON_BACKGROUND.equals(name) ||
        ZOOM_BUTTON_FOREGROUND.equals(name) 
        ) {
      return () -> (zoomButtons);
    }
    return super.getAttributeVisibility(name);
  }
      
  /**
   * The Attributes of a Chart are:
   *
   * <pre>
   * <code>
   * NAME
   * </code>
   *  for the name of the chart
   * <code>
   * FILE
   * </code>
   *  for the name of the image in the {@link DataArchive}
   * <code>
   * ZOOM_START
   * </code>
   * for the initial zoom level
   * <code>
   * ZOOM_LEVELS
   * </code>
   * for the zoom levels
   * <code>
   * ZOOM_IN_KEY
   * </code>
   * for zoom in hotkey
   * <code>
   * ZOOM_OUT_KEY
   * </code>
   * for zoom out hotkey
   * <code>
   * ZOOM_START_KEY
   * </code>
   * for zoom initial hotkey
   * <code>
   * ZOOM_BUTTONS
   * </code>
   * to enable zoom buttons drawn over image
   * <code>
   * ZOOM_BUTTON_OPACITY
   * </code>
   * to set zoom button opacity when not hovering over buttons
   * <code>
   * ZOOM_BUTTON_WIDTH
   * </code>
   * to set the width of each zoom button
   * <code>
   * ZOOM_BUTTON_HEIGHT
   * </code>
   * to set the height of each zoom buttons
   * <code>
   * ZOOM_BUTTON_BACKGROUND
   * </code>
   * to set background colour of zoom buttons
   * <code>
   * ZOOM_BUTTON_FOREGROUND
   * </code>
   * to set background colour of zoom buttons
   * </pre>
   */
  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      DESCRIPTION,
      FILE,
      ZOOM_START,
      ZOOM_LEVELS,
      ZOOM_IN_KEY,
      ZOOM_OUT_KEY,
      ZOOM_START_KEY,
      ZOOM_BUTTONS,
      ZOOM_BUTTON_OPACITY,
      ZOOM_BUTTON_WIDTH,
      ZOOM_BUTTON_HEIGHT,
      ZOOM_BUTTON_BACKGROUND,
      ZOOM_BUTTON_FOREGROUND      
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString(Resources.DESCRIPTION),
      Resources.getString("Editor.image_label"),
      "",
      Resources.getString("Editor.Zoom.preset"),
      Resources.getString("Editor.Zoom.in_key"),
      Resources.getString("Editor.Zoom.out_key"),
      Resources.getString("Editor.Zoom.initial_key"),
      Resources.getString("Editor.Zoom.buttons_enabled"),
      Resources.getString("Editor.Zoom.button_opacity"),
      Resources.getString("Editor.Zoom.button_width"),
      Resources.getString("Editor.Zoom.button_height"),
      Resources.getString("Editor.Zoom.button_background"),
      Resources.getString("Editor.Zoom.button_foreground")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      Image.class,
      null,   // ZOOM_START is handled by the LevelConfigurer
      LevelConfig.class,
      NamedKeyStroke.class,
      NamedKeyStroke.class,
      NamedKeyStroke.class,
      Boolean.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Color.class,
      Color.class
    };
  }

  @Override
  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (FILE.equals(name)) {
      return fileName;
    }
    else if (DESCRIPTION.equals(name)) {
      return description;
    }
    else if (ZOOM_START.equals(name)) {
      // Notes:
      //
      // 1. ZOOM_START is one-based, not zero-based.
      // 2. The levels in state run from zoomed out to zoomed in,
      // while the levels coming from outside Zoomer run from
      // zoomed in to zoomed out. Hence we reverse the initial
      // zoom level being returned here.
      //
      return String.valueOf(state.getLevelCount() - state.getInitialLevel());
    }
    else if (ZOOM_LEVELS.equals(name)) {
      final List<Double> levels = state.getLevels();
      final String[] s = new String[levels.size()];
      for (int i = 0; i < s.length; ++i) {
        s[i] = levels.get(i).toString();
      }

      return StringArrayConfigurer.arrayToString(s);
    }
    else if (ZOOM_IN_KEY.equals(name)) {
      return NamedHotKeyConfigurer.encode(zoomInKey);
    }
    else if (ZOOM_OUT_KEY.equals(name)) {
      return  NamedHotKeyConfigurer.encode(zoomOutKey);
    }
    else if (ZOOM_START_KEY.equals(name)) {
      return  NamedHotKeyConfigurer.encode(zoomStartKey);
    }
    else if (ZOOM_BUTTONS.equals(name)) {
      return  Boolean.toString(zoomButtons);
    }
    else if (ZOOM_BUTTON_OPACITY.equals(name)) {
      return  Integer.toString(zoomButtonOpacity);
    }
    else if (ZOOM_BUTTON_WIDTH.equals(name)) {
      return  Integer.toString(zoomButtonWidth);
    }
    else if (ZOOM_BUTTON_HEIGHT.equals(name)) {
      return  Integer.toString(zoomButtonHeight);
    }
    else if (ZOOM_BUTTON_BACKGROUND.equals(name)) {
      return ColorConfigurer.colorToString(zoomButtonBackground);
    }
    else if (ZOOM_BUTTON_FOREGROUND.equals(name)) {
      return ColorConfigurer.colorToString(zoomButtonForeground);
    }

    return null;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Chart.component_type");
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (fileName != null) s.add(fileName);
  }
}
