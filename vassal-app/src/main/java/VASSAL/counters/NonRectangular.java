/*
 *
 * Copyright (c) 2003 by Rodney Kinney
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
package VASSAL.counters;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.DoubleConfigurer;
import VASSAL.configure.ImageSelector;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.Op;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.StringTokenizer;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.tuple.Pair;

/**
 * A trait for assigning an arbitrary shape to a {@link GamePiece}
 *
 * @see GamePiece#getShape
 */
public class NonRectangular extends Decorator implements EditablePiece {
  public static final String ID = "nonRect2;"; //NON-NLS
  public static final String OLD_ID = "nonRect;"; // NON-NLS
  private static final Map<String, Pair<String, Shape>> cache = new HashMap<>();

  private String shapeSpec;
  private Shape shape;
  private Shape scaledShape = null;
  private String imageName = "";
  private double scale = 1.0;

  public NonRectangular() {
    this(ID + "1.0;", null);
  }

  public NonRectangular(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');

    se.append(scale)
      .append(shapeSpec);

    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public Shape getShape() {
    if (shape == null) return piece.getShape();

    if (scale == 1.0) {
      return shape;
    }

    //BR// Scale the shape based on our designated scale factor
    if (scaledShape == null) {
      final AffineTransform tx = new AffineTransform();
      tx.scale(scale, scale);
      scaledShape = tx.createTransformedShape(shape);
    }

    return scaledShape;
  }

  public void setScale(double scale) {
    this.scale = scale;
    scaledShape = null; //BR// Clear cached scaled-shape
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public String getDescription() {
    return Resources.getString("Editor.NonRectangular.trait_description");
  }

  @Override
  public void mySetType(String type) {
    if (type.startsWith(OLD_ID)) {
      setScale(1.0);
      shapeSpec = type.substring(OLD_ID.length());
    }
    else {
      type = type.substring(ID.length());
      final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
      setScale(st.nextDouble(1.0));
      shapeSpec = st.getRemaining(); //BR// Everything else is our shape spec
    }
    shape = buildPath(shapeSpec);
  }

  private Shape buildPath(String spec) {
    final Pair<String, Shape> p = cache.get(spec);
    Shape sh = p == null ?  null : p.getRight();
    imageName = p == null ? "" : p.getLeft();

    if (sh == null) {
      final GeneralPath path = new GeneralPath();
      final StringTokenizer st = new StringTokenizer(spec, ",");
      if (st.hasMoreTokens()) {
        while (st.hasMoreTokens()) {
          final String token = st.nextToken();
          switch (token.charAt(0)) {
          case 'c':
            path.closePath();
            break;
          case 'm':
            path.moveTo(Integer.parseInt(st.nextToken()),
              Integer.parseInt(st.nextToken()));
            break;
          case 'l':
            path.lineTo(Integer.parseInt(st.nextToken()),
              Integer.parseInt(st.nextToken()));
            break;
          // Note the image name is stored as a single token so older clients will ignore it.
          case 'n':
            imageName = token.length() > 1 ? token.substring(1) : "";
            break;
          }
        }
        sh = new Area(path);
        cache.put(spec, Pair.of(imageName, sh));
      }
    }

    return sh;
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    Collections.addAll(s, imageName);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("NonRectangular.html"); // NON-NLS
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof NonRectangular)) return false;
    final NonRectangular c = (NonRectangular) o;
    if (!Objects.equals(scale, c.scale)) return false;
    return Objects.equals(shapeSpec, c.shapeSpec);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private class Ed implements PieceEditor {
    private Shape shape;
    private final JPanel controls;
    private final ImageSelector picker;
    final DoubleConfigurer scaleConfig;

    private Ed(NonRectangular p) {
      shape = p.shape;
      controls = new JPanel(new MigLayout("ins 0", "[grow][]", "[grow]")); // NON-NLS

      controls.add(new JLabel(Resources.getString("Editor.NonRectangular.scale")));
      scaleConfig = new DoubleConfigurer(p.scale);
      controls.add(scaleConfig.getControls(), "wrap"); //NON-NLS

      final JPanel shapePanel = new JPanel() {
        private static final long serialVersionUID = 1L;

        @Override
        public void paint(Graphics g) {
          final Graphics2D g2d = (Graphics2D) g;
          g2d.setColor(Color.white);
          g2d.fillRect(0, 0, getWidth(), getHeight());
          if (shape != null) {
            g2d.translate(getWidth() / 2, getHeight() / 2);
            g2d.setColor(Color.black);
            g2d.fill(shape);
          }
        }

        @Override
        public Dimension getPreferredSize() {
          final Dimension d = shape == null
            ? new Dimension(60, 60) : shape.getBounds().getSize();
          d.width = Math.max(d.width, 60);
          d.height = Math.max(d.height, 60);
          return d;
        }
      };
      controls.add(shapePanel, "grow"); // NON-NLS

      picker = new ImageSelector(p.imageName);
      picker.addPropertyChangeListener(e -> {
        final String imageName = picker.getImageName();
        if (imageName == null || imageName.isEmpty()) {
          shape = null;
          controls.revalidate();
          repack(controls);
        }
        else {
          final Image img = Op.load(picker.getImageName()).getImage();
          if (img != null)
            setShapeFromImage(img);
        }
      });

      controls.add(picker.getControls());
    }

    public void setShapeFromImage(Image im) {
      controls.getTopLevelAncestor()
              .setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

      final BufferedImage bi = ImageUtils.toBufferedImage(im);
      final int w = bi.getWidth();
      final int h = bi.getHeight();
      final int[] pixels = bi.getRGB(0, 0, w, h, new int[w * h], 0, w);

      // build the outline in strips
      final Area outline = new Area();
      for (int y = 0; y < h; ++y) {
        int left = -1;
        for (int x = 0; x < w; ++x) {
          if (((pixels[x + y * w] >>> 24) & 0xff) > 0) {
            if (left < 0) {
              left = x;
            }
          }
          else if (left > -1) {
            outline.add(new Area(new Rectangle(left, y, x - left, 1)));
            left = -1;
          }
        }

        if (left > -1) {
          outline.add(new Area(new Rectangle(left, y, w - left, 1)));
        }
      }

      shape = AffineTransform.getTranslateInstance(-w / 2.0, -h / 2.0)
                             .createTransformedShape(outline);

      repack(controls);

      controls.getTopLevelAncestor().setCursor(null);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final StringBuilder buffer = new StringBuilder();
      if (shape != null) {
        final PathIterator it = shape.getPathIterator(new AffineTransform());
        final float[] pts = new float[6];

        buffer.append("n"); // NON-NLS Store the imageName in a form that will be ignored by older clients.
        buffer.append(picker.getImageName());
        buffer.append(",");

        while (!it.isDone()) {
          switch (it.currentSegment(pts)) {
          case PathIterator.SEG_MOVETO:
            buffer.append("m,") //NON-NLS
                  .append(Math.round(pts[0]))
                  .append(',')
                  .append(Math.round(pts[1]));
            break;
          case PathIterator.SEG_LINETO:
          case PathIterator.SEG_CUBICTO:
          case PathIterator.SEG_QUADTO:
            buffer.append("l,") //NON-NLS
                  .append(Math.round(pts[0]))
                  .append(',')
                  .append(Math.round(pts[1]));
            break;
          case PathIterator.SEG_CLOSE:
            buffer.append('c');
            break;
          }
          it.next();
          if (!it.isDone()) {
            buffer.append(',');
          }
        }
      }

      final SequenceEncoder se = new SequenceEncoder(';');

      se.append(scaleConfig.getValueString())
        .append(buffer.toString());

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
