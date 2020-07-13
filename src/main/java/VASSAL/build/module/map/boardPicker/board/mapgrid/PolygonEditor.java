/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.event.MouseInputAdapter;

import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.SwingUtils;

public class PolygonEditor extends JPanel {
  private static final long serialVersionUID = 1L;

  private Polygon polygon;
  private int selected = -1;
  protected JScrollPane myScroll;

  public PolygonEditor(Polygon p) {
    polygon = p;
    // reset(); // too much of this happening
  }

  protected void reset() {
    MouseListener[] ml = getMouseListeners();

    // get rid of all the mouse listeners floating around
    for (MouseListener i: ml)
      removeMouseListener(i);

    MouseMotionListener[] mml = getMouseMotionListeners();

    for (MouseMotionListener i: mml)
      removeMouseMotionListener(i);

    if (polygon == null || polygon.npoints == 0) {
      setupForCreate();
    }
    else {
      setupForEdit();
    }
  }

  public Polygon getPolygon() {
    return polygon;
  }

  public Polygon clonePolygon() {
    return new Polygon(polygon.xpoints, polygon.ypoints, polygon.npoints);
  }

  public void setPolygon(Polygon polygon) {
    this.polygon = polygon;
  }

  public void setScroll(JScrollPane scroll) {
    myScroll = scroll;
  }

  private void setupForCreate() {
    DefineRectangle dr = new DefineRectangle();
    addMouseListener(dr);
  }

  private void setupForEdit() {
    ModifyPolygon mp = new ModifyPolygon();
    addMouseListener(mp);
    addMouseMotionListener(mp);
    ActionListener l = new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        if (selected >= 0) {
          for (int i = selected; i < polygon.npoints - 1; ++i) {
            polygon.xpoints[i] = polygon.xpoints[i + 1];
            polygon.ypoints[i] = polygon.ypoints[i + 1];
          }
          polygon.npoints--;
          selected = -1;
          repaint();
        }
      }
    };
    registerKeyboardAction(l, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE,0), WHEN_IN_FOCUSED_WINDOW);
    registerKeyboardAction(l, KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE,0), WHEN_IN_FOCUSED_WINDOW);
    requestFocus();
    selected = 2;
    repaint();
  }

  public void center(Point p) {
    Rectangle r = this.getVisibleRect();
    if (r.width == 0) {
      r.width = 600;
      r.height = 600;
    }
    int x = p.x-r.width/2;
    int y = p.y-r.height/2;
    if (x < 0) x = 0;
    if (y < 0) y = 0;
    scrollRectToVisible(new Rectangle(x, y, r.width, r.height));
  }

  public static void reset(Polygon p, String path) {
    p.reset();
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(path, ';');
    while (sd.hasMoreTokens()) {
      String s = sd.nextToken();
      SequenceEncoder.Decoder pd = new SequenceEncoder.Decoder(s, ',');
      if (pd.hasMoreTokens()) {
        try {
          int x = Integer.parseInt(pd.nextToken().trim());
          if (pd.hasMoreTokens()) {
            int y = Integer.parseInt(pd.nextToken().trim());
            p.addPoint(x, y);
          }
        }
        // FIXME: review error message
        catch (NumberFormatException e) {
        }
      }
    }
  }

  public static String polygonToString(Polygon p) {
    final StringBuilder s = new StringBuilder();
    for (int i = 0; i < p.npoints; i++) {
      s.append(Math.round(p.xpoints[i])).append(',').append(Math.round(p.ypoints[i]));
      if (i < (p.npoints - 1)) {
        s.append(';');
      }
    }
    return s.toString();
  }

  @Override
  public void paint(Graphics g) {
    paintBackground(g);

    if (polygon == null || polygon.npoints == 0) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;

    g2d.setColor(Color.white);
    g2d.setComposite(
      AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));
    g2d.fill(polygon);

    if (selected >= 0 && selected < polygon.xpoints.length) {
      g2d.setColor(Color.red);
      int x = polygon.xpoints[selected];
      int y = polygon.ypoints[selected];
      g2d.fillOval(x - 10, y - 10, 20, 20);
    }

    g2d.setComposite(AlphaComposite.SrcAtop);
    g2d.setColor(Color.black);
    g2d.setStroke(new BasicStroke(2.0F));
    g2d.drawPolygon(polygon);
  }

  protected void paintBackground(Graphics g) {
    super.paint(g);
  }

  private class ModifyPolygon extends MouseInputAdapter {
    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isLeftMouseButton(e)) {
        moveSelectedPoint(e);
        scrollAtEdge(e.getPoint(), 15);
        repaint();
      }
    }

    private void moveSelectedPoint(MouseEvent e) {
      if (selected >= 0 && selected < polygon.xpoints.length) {
        polygon.xpoints[selected] = e.getX();
        polygon.ypoints[selected] = e.getY();
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (SwingUtils.isLeftMouseButton(e)) {
        moveSelectedPoint(e);
        repaint();
      }
    }

    @Override
    public void mouseClicked(MouseEvent e) {
      if (!SwingUtils.isRightMouseButton(e)) {
        return;
      }

      // find closest segment/vertex
      selected = -1;
      double minDist = Float.MAX_VALUE;
      boolean isVertex = false;

      int x0 = e.getX();
      int y0 = e.getY();

      for (int i = 0; i < polygon.npoints; ++i) {
        int x1 = polygon.xpoints[i];
        int y1 = polygon.ypoints[i];
        int x2, y2;
        if (i == polygon.npoints-1) {
          x2 = polygon.xpoints[0];
          y2 = polygon.ypoints[0];
        }
        else {
          x2 = polygon.xpoints[i+1];
          y2 = polygon.ypoints[i+1];
        }

        if (y2 == y1 && x2 == x1) // two verteces on top of each other: skip
          continue;

        double d = Point2D.distance(x1, y1, x2, y2); // segment length
        double comp = ((x2-x1)*(x0-x1) + (y2-y1)*(y0-y1)) / d; // component of projection of selection on segment
        double dist; // orthogonal distance to segment

        if (comp <= 0.0) { // too far out beyond first vertex: just move that vertex if it's closest
          dist = Point2D.distance(x1, y1, x0, y0);
          if (dist < minDist) {
            isVertex = true;
            minDist = dist;
            selected = i;
          }
        }
        else if (comp >= d) { // too far out beyond second vertex: just move that vertex: just move that virtex if it's closest
          dist = Point2D.distance(x0, y0, x2, y2);
          if (dist < minDist) {
            isVertex = true;
            minDist = dist;
            selected = i+1;
          }
        }
        else { // calculate orthogonal distance to segment
          dist = Math.abs((y2-y1)*e.getX() - (x2-x1)*e.getY() + x2*y1 - y2*x1) / Math.sqrt((y2-y1)*(y2-y1) + (x2-x1)*(x2-x1));
          if (dist < minDist) {
            isVertex = false;
            minDist = dist;
            selected = i+1;
          }
        }
      }

      if (!isVertex) { // insert a point near segment
        polygon.addPoint(e.getX(), e.getY());
        if (selected >= 0) {
          for (int i = polygon.npoints - 1; i > selected; --i) {
            polygon.xpoints[i] = polygon.xpoints[i - 1];
            polygon.ypoints[i] = polygon.ypoints[i - 1];
          }
          polygon.xpoints[selected] = e.getX();
          polygon.ypoints[selected] = e.getY();
        }
      }

      repaint();
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (SwingUtils.isLeftMouseButton(e)) {
        selected = -1;
        double minDist = Float.MAX_VALUE;

        // move an existing vertex
        for (int i = 0; i < polygon.npoints; ++i) {
          double dist = Point2D.distance(
            polygon.xpoints[i], polygon.ypoints[i], e.getX(), e.getY()
          );
          if (dist < minDist) {
            minDist = dist;
            selected = i;
          }
        }

        repaint();
      }
    }

    public void scrollAtEdge(Point evtPt, int dist) {
      Point p = new Point(
        evtPt.x - myScroll.getViewport().getViewPosition().x,
        evtPt.y - myScroll.getViewport().getViewPosition().y
      );
      int dx = 0, dy = 0;
      if (p.x < dist && p.x >= 0)
        dx = -1;
      if (p.x >= myScroll.getViewport().getSize().width - dist
          && p.x < myScroll.getViewport().getSize().width)
        dx = 1;
      if (p.y < dist && p.y >= 0)
        dy = -1;
      if (p.y >= myScroll.getViewport().getSize().height - dist
          && p.y < myScroll.getViewport().getSize().height)
        dy = 1;

      if (dx != 0 || dy != 0) {
        Rectangle r = new Rectangle(myScroll.getViewport().getViewRect());
        r.translate(2 * dist * dx, 2 * dist * dy);
        r = r.intersection(new Rectangle(new Point(0, 0), getPreferredSize()));
        scrollRectToVisible(r);
      }
    }
  }

  private class DefineRectangle extends MouseInputAdapter {
    @Override
    public void mousePressed(MouseEvent e) {
      if (SwingUtils.isLeftMouseButton(e)) {
        polygon = new Polygon();
        polygon.addPoint(e.getX(), e.getY());
        polygon.addPoint(e.getX(), e.getY());
        polygon.addPoint(e.getX(), e.getY());
        polygon.addPoint(e.getX(), e.getY());
        addMouseMotionListener(this);
      }
    }

    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isLeftMouseButton(e)) {
        polygon.xpoints[1] = e.getX();
        polygon.xpoints[2] = e.getX();
        polygon.ypoints[2] = e.getY();
        polygon.ypoints[3] = e.getY();
        repaint();
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (SwingUtils.isLeftMouseButton(e)) {
        removeMouseListener(this);
        removeMouseMotionListener(this);
        setupForEdit();
      }
    }
  }

  public static void main(String[] args) {
    JFrame f = new JFrame();
    f.add(new PolygonEditor(null));
    f.setSize(500, 500);
    f.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });
    f.setVisible(true);
  }
}
