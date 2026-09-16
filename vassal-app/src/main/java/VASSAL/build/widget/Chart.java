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
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.io.File;
import java.util.Collection;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.DataArchive;
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
  private JScrollPane scroll;
  private String fileName;
  private SourceOp srcOp;
  private ScaleOp scaleOp;
  private View view;

  private static final Point2D ORIGIN = new Point2D.Double(0.0, 0.0);

//  private double zoom = 1.0;
  private static final double zoom = 1.0;
  private final Rectangle boundaries = new Rectangle();

  private final TileRenderer renderer = new TileRenderer();

/*
  public void setZoom(double z) {
    zoom = z;
  }
*/

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

  @Override
  public Component getComponent() {
    if (scroll == null) {
      view = new View();
      boundaries.setSize(srcOp != null ? srcOp.getSize() : new Dimension(0, 0));

      final Dimension vpref = view.getPreferredSize();

      scroll = new AdjustableSpeedScrollPane(view);
      scroll.getViewport().setPreferredSize(vpref);
      scroll.getViewport().setAlignmentY(0.0F);
    }
    return scroll;
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
   * </pre>
   */
  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, DESCRIPTION, FILE};
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{Resources.getString("Editor.name_label"), Resources.getString(Resources.DESCRIPTION), Resources.getString("Editor.image_label")};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, String.class, Image.class};
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
