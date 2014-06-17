/*
 * $Id$
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.StringTokenizer;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.Op;

/**
 * A trait for assigning an arbitrary shape to a {@link GamePiece}
 *
 * @see GamePiece#getShape
 */
public class NonRectangular extends Decorator implements EditablePiece {
  public static final String ID = "nonRect;";
  private static HashMap<String,Shape> shapeCache = new HashMap<String,Shape>();

  private String type;
  private Shape shape;

  public NonRectangular() {
    this(ID, null);
  }

  public NonRectangular(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetState(String newState) {
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return type;
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return shape != null ? shape : piece.getShape();
  }

  public String getName() {
    return piece.getName();
  }

  public String getDescription() {
    return "Non-Rectangular";
  }

  public void mySetType(String type) {
    this.type = type;
    final String shapeSpec = type.substring(ID.length());
    shape = buildPath(shapeSpec);
  }

  private Shape buildPath(String spec) {
    Shape sh = shapeCache.get(spec);

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
          }
        }
        sh = new Area(path);
        shapeCache.put(spec, sh);
      }
    }

    return sh;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("NonRectangular.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private class Ed implements PieceEditor {
    private Shape shape;
    private JPanel controls;

    private Ed(NonRectangular p) {
      shape = p.shape;
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.X_AXIS));

      final JPanel shapePanel = new JPanel() {
        private static final long serialVersionUID = 1L;

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

        public Dimension getPreferredSize() {
          final Dimension d = shape == null
            ? new Dimension(60, 60) : shape.getBounds().getSize();
          d.width = Math.max(d.width, 60);
          d.height = Math.max(d.height, 60);
          return d;
        }
      };
      controls.add(shapePanel);

      final ImagePicker picker = new ImagePicker() {
        private static final long serialVersionUID = 1L;

        public void setImageName(String name) {
          super.setImageName(name);

          final Image img = Op.load(name).getImage();
          if (img != null) setShapeFromImage(img);
        }
      };
      picker.setBorder(new TitledBorder("Use image shape"));
      controls.add(picker);
    }

    public void setShapeFromImage(Image im) {
      controls.getTopLevelAncestor()
              .setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

      final BufferedImage bi = ImageUtils.toBufferedImage(im);
      final int w = bi.getWidth();
      final int h = bi.getHeight();
      final int[] pixels = bi.getRGB(0, 0, w, h, new int[w*h], 0, w);

      // build the outline in strips
      final Area outline = new Area();
      for (int y = 0; y < h; ++y) {
        int left = -1;
        for (int x = 0; x < w; ++x) {
          if (((pixels[x + y*w] >>> 24) & 0xff) > 0) {
            if (left < 0) {
              left = x;
            }
          }
          else if (left > -1) {
            outline.add(new Area(new Rectangle(left, y, x-left, 1)));
            left = -1;
          }
        }

        if (left > -1) {
          outline.add(new Area(new Rectangle(left, y, w-left, 1)));
        }
      }

// FIXME: should be 2.0 to avoid integer arithemtic?
      shape = AffineTransform.getTranslateInstance(-w / 2, -h / 2)
                             .createTransformedShape(outline);

      final  Window wd = SwingUtilities.getWindowAncestor(controls);
      if (wd != null) wd.pack();

      controls.getTopLevelAncestor().setCursor(null);
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      final StringBuilder buffer = new StringBuilder();
      if (shape != null) {
        final PathIterator it = shape.getPathIterator(new AffineTransform());
        final float[] pts = new float[6];
        while (!it.isDone()) {
          switch (it.currentSegment(pts)) {
          case PathIterator.SEG_MOVETO:
            buffer.append('m')
                  .append(',')
                  .append(Math.round(pts[0]))
                  .append(',')
                  .append(Math.round(pts[1]));
            break;
          case PathIterator.SEG_LINETO:
          case PathIterator.SEG_CUBICTO:
          case PathIterator.SEG_QUADTO:
            buffer.append('l')
                  .append(',')
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
      return ID + buffer.toString();
    }

    public String getState() {
      return "";
    }
  }
}
