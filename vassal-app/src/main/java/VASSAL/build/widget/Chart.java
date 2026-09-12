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

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

import javax.swing.JLabel;
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
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpIcon;
import VASSAL.tools.imageop.SourceOp;
import VASSAL.tools.swing.SwingUtils;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;

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
  private JLabel label;
  private double currentScale = 1;
  private Boolean enableZoom = true;
  private Double minZoom = .3;
  private Double maxZoom = 3.;
  private NamedKeyStroke zoomInKey = NamedKeyStroke.of('=', InputEvent.SHIFT_MASK);
  private NamedKeyStroke zoomOutKey = NamedKeyStroke.of('-');

  /**
   * Construct the swing component.
   */
  @Override
  public Component getComponent() {
    if (chart == null) {
      label = new JLabel();
      srcOp = (fileName == null || fileName.isBlank()) ? null : Op.load(fileName);
      if (srcOp != null) {
        label.setIcon(new OpIcon(srcOp) {
            /**
             * This version _does not_ reset the scaling done in the
             * graphics context.
             *
             * It seems that OpIcon rescales the images so that it
             * fits with the expectations of the toolbar UI.  However,
             * when we are dealing with charts, we are not constrained
             * to the toolbar UI, and we should _not_ reset to some
             * default, system dependent, scale.
             *
             * Instead, we use the current scaling to draw the chart.
             */
            @Override 
            public void paintIcon(Component c, Graphics g, int x, int y) {
              final Graphics2D g2d = (Graphics2D) g;
              final AffineTransform orig_t = g2d.getTransform();
              
              try {
                g.drawImage(Op.scale(sop, currentScale).getImage(null), 0, 0, c);
              }
              catch (CancellationException | InterruptedException e) {
                ErrorDialog.bug(e);
              }
              catch (ExecutionException e) {
                if (!Op.handleException(e)) ErrorDialog.bug(e);
              }
              
              g2d.setTransform(orig_t);
            }
          });
      }
      final Dimension d = label.getPreferredSize();
      if (d.width > 300 || d.height > 300 || enableZoom) {
        final JScrollPane scroll = new AdjustableSpeedScrollPane(label);
        scroll.getViewport().setPreferredSize(label.getPreferredSize());
        scroll.getViewport().setAlignmentY(0.0F);

        if (enableZoom) {
          // Key handling.  This should probably be configurable,
          // Note, we filter on whether the event originated in this
          // window (actually it's parent).
          final NamedKeyStrokeListener zoomIn  = new NamedKeyStrokeListener(e -> {
            if (!fromThisWindow(e)) return;
            zoom(+1); });
          final NamedKeyStrokeListener zoomOut = new NamedKeyStrokeListener(e -> {
            if (!fromThisWindow(e)) return;
            zoom(-1); });
          zoomIn.setKeyStroke(zoomInKey);
          zoomOut.setKeyStroke(zoomOutKey);

          GameModule.getGameModule().addKeyStrokeListener(zoomIn);
          GameModule.getGameModule().addKeyStrokeListener(zoomOut);
        
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
        chart = label;
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
      SwingUtilities.getWindowAncestor(label);
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
    
    currentScale += direction * .1;
    currentScale =  Math.max(Math.min(currentScale, maxZoom), minZoom);
    
    final Dimension d = new Dimension();
    d.setSize(currentScale * srcOp.getWidth(),
              currentScale * srcOp.getHeight());
    label.setPreferredSize(d);
    label.repaint();
    label.revalidate();
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
      if (label != null) {
        srcOp = (fileName == null || fileName.isBlank()) ? null : Op.load(fileName);
        if (srcOp != null) {
          label.setIcon(new OpIcon(srcOp));
          label.revalidate();
        }
      }
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String)val;
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
      return NamedHotKeyConfigurer.encode(zoomOutKey);
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
