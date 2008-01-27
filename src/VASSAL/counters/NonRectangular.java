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
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.image.PixelGrabber;
import java.io.IOException;
import java.util.HashMap;
import java.util.StringTokenizer;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;

/**
 * A trait for assigning an arbitrary shape to a {@link GamePiece}
 * 
 * @see GamePiece#getShape
 */
public class NonRectangular extends Decorator implements EditablePiece {
  public static final String ID = "nonRect;";
  private static HashMap<String,GeneralPath> shapeCache =
    new HashMap<String,GeneralPath>();
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
    String shapeSpec = type.substring(ID.length());
    shape = buildPath(shapeSpec);
  }

  private GeneralPath buildPath(String spec) {
    GeneralPath path = shapeCache.get(spec);
    if (path == null && !shapeCache.containsKey(spec)) {
      StringTokenizer st = new StringTokenizer(spec, ",");
      if (st.hasMoreTokens()) {
        path = new GeneralPath();
        while (st.hasMoreTokens()) {
          String token = st.nextToken();
          switch (token.charAt(0)) {
          case 'c':
            path.closePath();
            break;
          case 'm':
            path.moveTo(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
            break;
          case 'l':
            path.lineTo(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
            break;
          }
        }
      }
      shapeCache.put(spec, path);
    }
    return path;
  }

  public HelpFile getHelpFile() {
    return null;
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
          Graphics2D g2d = (Graphics2D) g;
          g.setColor(Color.white);
          g.fillRect(0, 0, getWidth(), getHeight());
          if (shape != null) {
            g2d.translate(getWidth() / 2, getHeight() / 2);
            g.setColor(Color.black);
            g2d.fill(shape);
          }
        }

        public Dimension getPreferredSize() {
          Dimension d = shape == null ? new Dimension(60, 60) : shape.getBounds().getSize();
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
          try {
            setShapeFromImage(GameModule.getGameModule().getDataArchive().getCachedImage(name));
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
      };
      picker.setBorder(new TitledBorder("Use image shape"));
      controls.add(picker);
    }

    public void setShapeFromImage(Image im) {
      controls.getTopLevelAncestor().setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
      try {
        MediaTracker t = new MediaTracker(controls);
        t.addImage(im, 0);
        t.waitForAll();
        int width = im.getWidth(controls);
        int height = im.getHeight(controls);
        int[] pixels = new int[width * height];
        PixelGrabber pg = new PixelGrabber(im, 0, 0, width, height, pixels, 0, width);
        pg.grabPixels();
        Area outline = new Area();
        for (int j = 0; j < height; ++j) {
          for (int i = 0; i < width; ++i) {
            if (((pixels[i + j * width] >> 24) & 0xff) > 0) {
              outline.add(new Area(new Rectangle(i, j, 1, 1)));
            }
          }
        }
        shape = AffineTransform.getTranslateInstance(-width / 2, -height / 2).createTransformedShape(outline);
      }
      catch (InterruptedException e) {
        shape = null;
      }
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
      controls.getTopLevelAncestor().setCursor(null);
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      StringBuffer buffer = null;
      buffer = new StringBuffer();
      if (shape != null) {
        PathIterator it = shape.getPathIterator(new AffineTransform());
        float[] pts = new float[6];
        while (!it.isDone()) {
          switch (it.currentSegment(pts)) {
          case PathIterator.SEG_MOVETO:
            buffer.append('m').append(',').append(Math.round(pts[0])).append(',').append(Math.round(pts[1]));
            break;
          case PathIterator.SEG_LINETO:
          case PathIterator.SEG_CUBICTO:
          case PathIterator.SEG_QUADTO:
            buffer.append('l').append(',').append(Math.round(pts[0])).append(',').append(Math.round(pts[1]));
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
