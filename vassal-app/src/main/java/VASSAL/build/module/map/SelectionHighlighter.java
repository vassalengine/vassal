/*
 *
 * Copyright (c) 2006-2007 by Brent Easton, Joel Uckelman
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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.util.Collection;
import java.util.List;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Highlighter;
import VASSAL.i18n.Resources;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.ScaledImagePainter;

public class SelectionHighlighter extends AbstractConfigurable implements Highlighter {
  public static final String NAME = "name"; //NON-NLS
  public static final String MATCH = "match"; //NON-NLS
  public static final String COLOR = "color"; //NON-NLS
  public static final String THICKNESS = "thickness"; //NON-NLS
  public static final String USE_IMAGE = "useImage"; //NON-NLS
  public static final String IMAGE = "image"; //NON-NLS
  public static final String X_OFFSET = "xoffset"; //NON-NLS
  public static final String Y_OFFSET = "yoffset"; //NON-NLS
  protected PropertyExpression matchProperties = new PropertyExpression();
  protected Color color = Color.RED;
  protected int thickness = 3;
  protected boolean useImage = false;
  protected String imageName = "";

  protected int xOff = 0;
  protected int yOff = 0;

  @Deprecated(since = "2021-04-05", forRemoval = true)
  protected int x = 0;
  @Deprecated(since = "2021-04-05", forRemoval = true)
  protected int y = 0;
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected Image image;

  protected VisibilityCondition visibilityCondition;

  protected ScaledImagePainter imagePainter = new ScaledImagePainter();

  @Override
  public void draw(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
    final Graphics2D g2d = (Graphics2D) g;
    if (accept(p)) {
      if (useImage) {
        final int x1 = x + xOff - (int) (imagePainter.getImageSize().width * zoom / 2);
        final int y1 = y + yOff - (int) (imagePainter.getImageSize().height * zoom / 2);
        imagePainter.draw(g, x1, y1, zoom, obs);
      }
      else {
        if (color == null || thickness <= 0) {
          return;
        }
        final Shape s = p.getShape();
        final Stroke str = g2d.getStroke();
        g2d.setStroke(new BasicStroke(Math.max(1, Math.round(zoom * thickness))));
        g2d.setColor(color);
        final AffineTransform t = AffineTransform.getScaleInstance(zoom, zoom);
        t.translate(x / zoom, y / zoom);
        g2d.draw(t.createTransformedShape(s));
        g2d.setStroke(str);
      }
    }
  }

  @Override
  public Rectangle boundingBox(GamePiece p) {
    final Rectangle r = p.getShape().getBounds();
    if (accept(p)) {
      if (useImage) {
        r.add(ImageUtils.getBounds(imagePainter.getImageSize()));
      }
      else {
        r.translate(-thickness, -thickness);
        r.setSize(r.width + 2 * thickness, r.height + 2 * thickness);
      }
    }
    return r;
  }

  protected boolean accept(GamePiece p) {
    return matchProperties.isNull() || matchProperties.accept(p);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.SelectionHighlight.component_type"); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString(Resources.NAME_LABEL),
        Resources.getString("Editor.SelectionHighlight.active_property"), //$NON-NLS-1$
        Resources.getString("Editor.SelectionHighlight.use_image"), //$NON-NLS-1$
        Resources.getString("Editor.border_color"), //$NON-NLS-1$
        Resources.getString("Editor.border_thickness"), //$NON-NLS-1$
        Resources.getString("Editor.image_label"), //$NON-NLS-1$
        Resources.getString("Editor.x_offset"), //$NON-NLS-1$
        Resources.getString("Editor.y_offset"), //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, PropertyExpression.class, Boolean.class, Color.class, Integer.class, IconConfig.class, Integer.class, Integer.class};
  }
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((SelectionHighlighter) c).imageName);
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, MATCH, USE_IMAGE, COLOR, THICKNESS, IMAGE, X_OFFSET, Y_OFFSET};
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name) || THICKNESS.equals(name)) {
      return () -> !useImage;
    }
    else if (List.of(IMAGE, X_OFFSET, Y_OFFSET).contains(name)) {
      return () -> useImage;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (key.equals(NAME)) {
      setConfigureName((String) value);
    }
    else if (key.equals(MATCH)) {
      matchProperties.setExpression((String) value);
    }
    else if (key.equals(USE_IMAGE)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      useImage = (Boolean) value;
    }
    else if (key.equals(COLOR)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      if (value != null) {
        color = ((Color) value);
      }
    }
    else if (key.equals(THICKNESS)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      thickness = (Integer) value;
    }
    else if (key.equals(IMAGE)) {
      imageName = (String) value;
      imagePainter.setImageName(imageName);
    }
    else if (key.equals(X_OFFSET)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      try {
        xOff = x = (Integer) value;
      }
      catch (NumberFormatException ex) {
        throw new IllegalBuildException(ex);
      }
    }
    else if (key.equals(Y_OFFSET)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      try {
        yOff = y = (Integer) value;
      }
      catch (NumberFormatException ex) {
        throw new IllegalBuildException(ex);
      }
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (key.equals(NAME)) {
      return getConfigureName();
    }
    else if (key.equals(MATCH)) {
      return matchProperties.getExpression();
    }
    else if (key.equals(USE_IMAGE)) {
      return String.valueOf(useImage);
    }
    else if (key.equals(COLOR)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (key.equals(THICKNESS)) {
      return String.valueOf(thickness);
    }
    else if (key.equals(IMAGE)) {
      return imageName;
    }
    else if (key.equals(X_OFFSET)) {
      return String.valueOf(xOff);
    }
    else if (key.equals(Y_OFFSET)) {
      return String.valueOf(yOff);
    }
    return null;
  }

  @Override
  public void removeFrom(Buildable parent) {
    ((SelectionHighlighters) parent).removeHighlighter(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "SelectionHighlighter"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void addTo(Buildable parent) {
    ((SelectionHighlighters) parent).addHighlighter(this);
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (imageName != null) s.add(imageName);
  }
}
