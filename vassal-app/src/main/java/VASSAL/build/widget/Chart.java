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
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.io.File;
import java.util.Collection;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.DataArchive;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.image.TileRenderer;
import VASSAL.tools.imageop.ScaleOp;
import VASSAL.tools.imageop.SourceOp;
import VASSAL.tools.swing.SwingUtils;

/**
 * A Chart is used for displaying charts and tables for the module.
 * The charts are loaded as images stored in the DataArchive. As a subclass
 * of Widget, a Chart may be added to any Widget, but it may not contain
 * children of its own.
 *
 * Scrollbars and zoom capability is only added if the embedded image
 * is larger than 300x300 pixels.
 * 
 * The widget allows zooming in and out on the embedded image either
 * using the mouse scroll key, with the control key held down, or by
 * configurable keyboard short cuts, defaults to '+' and '-'.
 *
 * TODO:
 * - Possibly make a toolbar with zoom in/out buttons, in which case
 *   their icons should be configurable too.
 */
public class Chart extends Widget {
  public static final String NAME = "chartName"; //NON-NLS
  public static final String FILE = "fileName"; //NON-NLS
  public static final String MIN_ZOOM = "minZoom"; //NON-NLS
  public static final String MAX_ZOOM = "maxZoom"; //NON-NLS
  public static final String ENABLE_ZOOM = "enableZoom"; //NON-NLS
  public static final String ZOOM_IN_KEY = "zoomInKey"; //NON-NLS
  public static final String ZOOM_OUT_KEY = "zoomOutKey"; //NON-NLS
  private Component chart;
  private String fileName;
  private SourceOp srcOp;
  private ScaleOp scaleOp;
  private View view;

  private static final Point2D ORIGIN = new Point2D.Double(0.0, 0.0);

  private double zoom = 1.0;
  private final Rectangle boundaries = new Rectangle();

  private final TileRenderer renderer = new TileRenderer();


  private class View extends JPanel {
    private static final long serialVersionUID = 1L;

    @Override
    public Dimension getPreferredSize() {
      if (srcOp == null) {
        return new Dimension(0, 0);
      }

      final Dimension size = srcOp.getSize();
      size.width *= zoom;
      size.height *= zoom;
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

      final double dzoom = zoom * os_scale;

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
  private Boolean enableZoom = true;
  private Double minZoom = .3;
  private Double maxZoom = 3.;
  private NamedKeyStroke zoomInKey = NamedKeyStroke.of(KeyEvent.VK_EQUALS,
                                                       InputEvent.SHIFT_DOWN_MASK);
  private NamedKeyStroke zoomOutKey = NamedKeyStroke.of(KeyEvent.VK_MINUS, 0);

  /**
   * Construct the swing component.
   */
  @Override
  public Component getComponent() {
    if (chart == null) {
      view = new View();
      boundaries.setSize(srcOp != null ? srcOp.getSize() : new Dimension(0, 0));

      final Dimension vpref = view.getPreferredSize();
      if (vpref.width > 300 || vpref.height > 300) {
        final JScrollPane scroll = new AdjustableSpeedScrollPane(view);
        scroll.getViewport().setPreferredSize(vpref);
        scroll.getViewport().setAlignmentY(0.0F);

        if (enableZoom) {
          // Note, we filter on whether the event originated in this
          // window (actually it's parent).
          final NamedKeyStrokeListener zoomIn  = new NamedKeyStrokeListener(e -> {
            if (!fromThisWindow(e)) return;
            zoom(+1);
          });
          final NamedKeyStrokeListener zoomOut = new NamedKeyStrokeListener(e -> {
            if (!fromThisWindow(e)) return;
            zoom(-1);
          });
          zoomIn.setKeyStroke(zoomInKey);
          zoomOut.setKeyStroke(zoomOutKey);

          GameModule.getGameModule().addKeyStrokeListener(zoomOut);
          GameModule.getGameModule().addKeyStrokeListener(zoomIn);
        
          // Listen for control-mouse-wheel events. 
          scroll.getViewport().addMouseWheelListener(new MouseWheelListener() {
            /**
             * Zoom view of image when mouse wheel is rolled up or
             * down
             */
            @Override
            public void mouseWheelMoved(MouseWheelEvent e) {
              if (!SwingUtils.isSelectionToggle(e)) return;
              
              if (e.getScrollType() != MouseWheelEvent.WHEEL_UNIT_SCROLL)
                return;
              
              zoom(e.getUnitsToScroll() < 0 ? 1 : -1);
            }
          });
        }
        chart = scroll;
      }
      else {
        chart = view;
      }
    }
    return chart;
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

  /**
   * Zoom in or out on the embedded image.
   *
   * @param direction Direction in which to zoom. Positive increases
   * the zoom level (zoom in), and negative decreases the zoom level.
   */
  public void zoom(int direction) {
    if (direction == 0)
      return;

    final double old = zoom;
    zoom += direction * .1;
    zoom =  Math.max(Math.min(zoom, maxZoom), minZoom);

    if (old == zoom)
      return;
    
    view.repaint();
    view.revalidate();
  }
  /**
   * Get the filename (relative to the archive image directory) of the
   * image to show.
   */
  public String getFileName() {
    return fileName;
  }
  /**
   * No-op
   */
  @Override
  public void addTo(Buildable parent) {
  }

  /**
   * No-op
   */
  @Override
  public void removeFrom(Buildable parent) {
  }

  /**
   * Get the HTML help file for this element. 
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.html", "Chart"); //NON-NLS
  }

  /**
   * Set an attribute to value
   *
   * @param key  Attribute name (key) to set
   * @param val Value to set the attribute to
   */
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
    else if (MAX_ZOOM.equals(key)) {
      if (val instanceof String) 
        val = Double.valueOf((String) val);
      maxZoom = (Double)val;
    }
    else if (MIN_ZOOM.equals(key)) {
      if (val instanceof String) 
        val = Double.valueOf((String) val);
      minZoom = (Double)val;
    }
    else if (ENABLE_ZOOM.equals(key)) {
      if (val instanceof String)
        val = Boolean.valueOf((String) val);
      enableZoom = (Boolean)val;
    }
    else if (ZOOM_IN_KEY.equals(key)) {
      if (val instanceof String)
        val = NamedHotKeyConfigurer.decode((String) val);
      zoomInKey = (NamedKeyStroke)val;
    }
    else if (ZOOM_OUT_KEY.equals(key)) {
      if (val instanceof String)
        val = NamedHotKeyConfigurer.decode((String) val);
      zoomOutKey = (NamedKeyStroke)val;
    }
  }

  /**
   * Do not allow any child elements
   */
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
   * ENABLE_ZOOM
   * </code>
   * for enabling zoom, or unconditionally enable scroll bars
   * <code>
   * MIN_ZOOM
   * </code>
   * for the least zoom level
   * <code>
   * MAX_ZOOM
   * </code>
   * for the largest zoom level
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
      ENABLE_ZOOM,
      MIN_ZOOM,
      MAX_ZOOM,
      ZOOM_IN_KEY,
      ZOOM_OUT_KEY };
  }

  /**
   * Get description strings of the attributes
   *
   * @return Array of strings
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString(Resources.DESCRIPTION),
      Resources.getString("Editor.image_label"),
      Resources.getString("Editor.Chart.enable_zoom"),
      Resources.getString("Editor.Chart.least_zoom"),
      Resources.getString("Editor.Chart.largest_zoom"),
      Resources.getString("Editor.Zoom.in_key"),
      Resources.getString("Editor.Zoom.out_key")
    }; 
  }

  /**
   * Get the types of the attributesw
   *
   * @return Array of attribute types
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      Image.class,
      Boolean.class,
      Double.class,
      Double.class,
      NamedKeyStroke.class,
      NamedKeyStroke.class
    };
  }


  /**
   * Get attribute value as a string
   *
   * @param name  Name of the attribute
   *
   * @return The attribute value as a string, or null
   */
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
    else if (ENABLE_ZOOM.equals(name)) {
      return enableZoom.toString();
    }
    else if (MIN_ZOOM.equals(name)) {
      return minZoom.toString();
    }
    else if (MAX_ZOOM.equals(name)) {
      return maxZoom.toString();
    }
    else if (ZOOM_IN_KEY.equals(name)) {
      return NamedHotKeyConfigurer.encode(zoomInKey);
    }
    else if (ZOOM_OUT_KEY.equals(name)) {
      return  NamedHotKeyConfigurer.encode(zoomOutKey);
    }
    return null;
  }

  /**
   * Only show zoom configurations if zoom is enabled
   */
  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(NAME, FILE, DESCRIPTION, ENABLE_ZOOM).contains(name)) {
      return () -> true;
    }
    else if (List.of(MIN_ZOOM, MAX_ZOOM, ZOOM_IN_KEY, ZOOM_OUT_KEY).contains(name)) {
      return () -> enableZoom;
    }
    return super.getAttributeVisibility(name);
  }
  
  /**
   * Get the title of this element.
   *
   * @return A string describing the element
   */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Chart.component_type");
  }

  /**
   * Find images used by this element and add to collection
   *
   * @param s Collection of strings
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (fileName != null) s.add(fileName);
  }
}
