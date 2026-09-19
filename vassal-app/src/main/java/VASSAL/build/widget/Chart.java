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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.LevelConfigurer;
import VASSAL.configure.Levels;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringArrayConfigurer;
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

  private JScrollPane scroll;
  private String fileName;
  private SourceOp srcOp;
  private ScaleOp scaleOp;
  private View view;
  private State state = new State(defaultZoomLevels, defaultInitialZoomLevel);

  private static final Point2D ORIGIN = new Point2D.Double(0.0, 0.0);

  private static final double e_n01 = Math.exp(-0.1);
  private static final double e_p01 = Math.exp(0.1);

  private static final double[] defaultZoomLevels = {
    e_n01 * e_n01 * e_n01 * e_n01 * e_n01,
    e_n01 * e_n01 * e_n01 * e_n01,
    e_n01 * e_n01 * e_n01,
    e_n01 * e_n01,
    e_n01,
    1.0,
    e_p01,
    e_p01 * e_p01,
    e_p01 * e_p01 * e_p01,
    e_p01 * e_p01 * e_p01 * e_p01,
    e_p01 * e_p01 * e_p01 * e_p01 * e_p01,
    e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01,
    e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01,
    e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01,
    e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01 * e_p01,
  };

  protected static final int defaultInitialZoomLevel = 5;

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
      initial = i;
    }

    public void lowerLevel() {
      cur = Math.max(0, cur - 1);
    }

    public void higherLevel() {
      cur = Math.min(cur + 1, levels.length - 1);
    }

    public double getZoom() {
      return levels[cur];
    }
  }

  private final Rectangle boundaries = new Rectangle();

  private final TileRenderer renderer = new TileRenderer();

  private NamedKeyStroke zoomInKey = NamedKeyStroke.of(
    KeyEvent.VK_EQUALS,
    InputEvent.SHIFT_DOWN_MASK
  );
  private NamedKeyStroke zoomOutKey = NamedKeyStroke.of(KeyEvent.VK_MINUS, 0);

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
    if (scroll == null) {
      view = new View();
      boundaries.setSize(srcOp != null ? srcOp.getSize() : new Dimension(0, 0));

      final Dimension vpref = view.getPreferredSize();

      scroll = new AdjustableSpeedScrollPane(view);
      scroll.getViewport().setPreferredSize(vpref);
      scroll.getViewport().setAlignmentY(0.0F);

      // Note, we filter on whether the event originated in this
      // window (actually it's parent).
      final NamedKeyStrokeListener zoomIn  = new NamedKeyStrokeListener(e -> {
        if (!fromThisWindow(e)) {
          return;
        }
        zoom(+1);
      });

      final NamedKeyStrokeListener zoomOut = new NamedKeyStrokeListener(e -> {
        if (!fromThisWindow(e)) {
          return;
        }
        zoom(-1);
      });

      zoomIn.setKeyStroke(zoomInKey);
      zoomOut.setKeyStroke(zoomOutKey);

      GameModule.getGameModule().addKeyStrokeListener(zoomOut);
      GameModule.getGameModule().addKeyStrokeListener(zoomIn);

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
    return scroll;
  }

  private void zoom(int direction) {
    if (direction == 0) {
      return;
    }

    Rectangle vr = view.getVisibleRect();
    double cx = vr.getCenterX();
    double cy = vr.getCenterY();

    double zoom = state.getZoom();

    cx /= zoom;
    cy /= zoom;

    if (direction > 0) {
      state.higherLevel();
    }
    else {
      state.lowerLevel();
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
      zoomInKey = (NamedKeyStroke)val;
    }
    else if (ZOOM_OUT_KEY.equals(key)) {
      if (val instanceof String) {
        val = NamedHotKeyConfigurer.decode((String) val);
      }
      zoomOutKey = (NamedKeyStroke)val;
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
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
      ZOOM_OUT_KEY
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
      Resources.getString("Editor.Zoom.out_key")
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
      NamedKeyStroke.class
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
