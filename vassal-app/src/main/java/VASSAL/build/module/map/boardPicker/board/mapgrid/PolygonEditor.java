/*
 * Copyright (c) 2004-2020 by Rodney Kinney, Joel Uckelman
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
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.InputMap;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.event.MouseInputAdapter;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.SwingUtils;

public class PolygonEditor extends JPanel {
  private static final long serialVersionUID = 1L;

  private Polygon polygon;
  private int selected = -1;

  private List<Point> path;

  protected JScrollPane myScroll;

  private static final String DELETE = "Delete";
  private static final String ESCAPE = "Escape";

  private static final int POINT_RADIUS = 10;
  private static final int CLICK_THRESHOLD = 10;

  public PolygonEditor(Polygon p) {
    polygon = p;
  }

  protected void reset() {
    // clear all the listeners
    final MouseListener[] ml = getMouseListeners();
    for (final MouseListener i: ml) {
      removeMouseListener(i);
    }

    final MouseMotionListener[] mml = getMouseMotionListeners();
    for (final MouseMotionListener i: mml) {
      removeMouseMotionListener(i);
    }

    final InputMap im = getInputMap(WHEN_IN_FOCUSED_WINDOW);
    im.remove(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
    im.remove(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0));

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
    final DefinePicker dp = new DefinePicker();
    addMouseListener(dp);
    addMouseMotionListener(dp);

    path = new ArrayList<>();
    requestFocus();
    repaint();
  }

  private void setupForEdit() {
    final ModifyPolygon mp = new ModifyPolygon();
    addMouseListener(mp);
    addMouseMotionListener(mp);

    getInputMap(WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), DELETE);
    getActionMap().put(DELETE, new AbstractAction() {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        mp.deleteKeyPressed();
      }
    });

    requestFocus();
    repaint();
  }

  public void center(Point p) {
    final Rectangle r = this.getVisibleRect();
    if (r.width == 0) {
      r.width = 600;
      r.height = 600;
    }
    int x = p.x - r.width / 2;
    int y = p.y - r.height / 2;
    if (x < 0) x = 0;
    if (y < 0) y = 0;
    scrollRectToVisible(new Rectangle(x, y, r.width, r.height));
  }

  public static void reset(Polygon p, String pathStr) {
    p.reset();
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(pathStr, ';');
    while (sd.hasMoreTokens()) {
      final String s = sd.nextToken();
      final SequenceEncoder.Decoder pd = new SequenceEncoder.Decoder(s, ',');
      if (pd.hasMoreTokens()) {
        try {
          final int x = Integer.parseInt(pd.nextToken().trim());
          if (pd.hasMoreTokens()) {
            final int y = Integer.parseInt(pd.nextToken().trim());
            p.addPoint(x, y);
          }
        }
        // FIXME: review error message
        catch (final NumberFormatException e) {
        }
      }
    }
  }

  public static String polygonToString(Polygon p) {
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < p.npoints; ++i) {
      sb.append(Math.round(p.xpoints[i]))
        .append(',')
        .append(Math.round(p.ypoints[i]));
      if (i < (p.npoints - 1)) {
        sb.append(';');
      }
    }
    return sb.toString();
  }

  @Override
  public void paint(Graphics g) {
    paintBackground(g);

    if ((polygon == null || polygon.npoints == 0) &&
        (path == null || path.isEmpty())) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    g2d.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    );

    g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));

    final int r = POINT_RADIUS;
    final int d = 2 * r;

    if (polygon != null && polygon.npoints > 0) {
      // fill the zone
      g2d.setColor(Color.WHITE);
      g2d.fill(polygon);

      // draw the vertex markers
      g2d.setColor(Color.BLACK);
      for (int i = 0; i < polygon.npoints; ++i) {
        final int x = polygon.xpoints[i];
        final int y = polygon.ypoints[i];
        g2d.drawOval(x - r, y - r, d, d);
      }

      // draw the selected vertex
      if (selected >= 0 && selected < polygon.xpoints.length) {
        g2d.setColor(Color.RED);
        final int x = polygon.xpoints[selected];
        final int y = polygon.ypoints[selected];
        g2d.fillOval(x - r, y - r, d, d);
      }

      // draw the zone
      g2d.setComposite(AlphaComposite.SrcAtop);
      g2d.setColor(Color.BLACK);
      g2d.setStroke(new BasicStroke(2.0F));
      g2d.drawPolygon(polygon);
    }
    else if (path != null && !path.isEmpty()) {
      final int ps = path.size();
      Point p1, p2;

      // draw the vertex markers
      g2d.setColor(Color.BLACK);
      for (int i = 0; i < ps; ++i) {
        p1 = path.get(i);
        g2d.drawOval(p1.x - r, p1.y - r, d, d);
      }

      // highlight the initial vertex if the active vertex overlaps it
      p1 = path.get(0);
      p2 = path.get(ps - 1);
      final double dp = Point2D.distance(p1.x, p1.y, p2.x, p2.y);
      if (dp <= 2 * CLICK_THRESHOLD) {
        g2d.setColor(Color.YELLOW);
        g2d.fillOval(p1.x - r, p1.y - r, d, d);
      }

      // draw the active vertex
      g2d.setColor(Color.RED);
      p1 = path.get(ps - 1);
      g2d.fillOval(p1.x - r, p1.y - r, d, d);

      // draw the path
      p1 = path.get(0);
      g2d.setComposite(AlphaComposite.SrcAtop);
      g2d.setColor(Color.BLACK);
      g2d.setStroke(new BasicStroke(2.0F));

      for (int i = 1; i < ps; ++i) {
        p2 = path.get(i);
        g2d.drawLine(p1.x, p1.y, p2.x, p2.y);
        p1 = p2;
      }
    }
  }

  protected void paintBackground(Graphics g) {
    super.paint(g);
  }

  protected static Pair<Integer, Double> nearestVertex(Polygon p, int x, int y) {
    int idx = -1;
    double minDist = Double.MAX_VALUE;

    for (int i = 0; i < p.npoints; ++i) {
      final int x1 = p.xpoints[i];
      final int y1 = p.ypoints[i];

      final double d = Point2D.distance(x, y, x1, y1);
      if (d < minDist) {
        minDist = d;
        idx = i;
      }
    }

    return Pair.of(idx, minDist);
  }

  protected static Triple<Integer, Point, Double> nearestSegment(Polygon p, int x, int y) {
    int idx = -1;
    int min_x = 0;
    int min_y = 0;
    double minDist = Double.MAX_VALUE;

    for (int i = 0; i < p.npoints; ++i) {
      final int j = (i + 1) % p.npoints;

      final int x1 = p.xpoints[i];
      final int y1 = p.ypoints[i];

      final int x2 = p.xpoints[j];
      final int y2 = p.ypoints[j];

      final int px = x2 - x1;
      final int py = y2 - y1;

      final int norm = px * px + py * py;

      double u = ((x - x1) * px + (y - y1) * py) / (double) norm;
      u = u > 1.0 ? 1.0 : (u < 0.0 ? 0.0 : u);

      // x3,y3 is the point nearest to x,y on x1,y1 - x2,y2
      final int x3 = (int) Math.round(x1 + u * px);
      final int y3 = (int) Math.round(y1 + u * py);

      final double d = Point2D.distance(x, y, x3, y3);
      if (d < minDist) {
        minDist = d;
        min_x = x3;
        min_y = y3;
        idx = i;
      }
    }

    return Triple.of(idx, new Point(min_x, min_y), minDist);
  }

  protected static void deleteVertex(Polygon p, int i) {
    p.xpoints = ArrayUtils.remove(p.xpoints, i);
    p.ypoints = ArrayUtils.remove(p.ypoints, i);
    --p.npoints;
    p.invalidate();
  }

  protected static void insertVertex(Polygon p, int i, int x, int y) {
    p.xpoints = ArrayUtils.insert(i, p.xpoints, x);
    p.ypoints = ArrayUtils.insert(i, p.ypoints, y);
    ++p.npoints;
    p.invalidate();
  }

  protected static void moveVertex(Polygon p, int i, int x, int y) {
    p.xpoints[i] = x;
    p.ypoints[i] = y;
    p.invalidate();
  }

  private class ModifyPolygon extends MouseInputAdapter {
    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        if (selected >= 0 && selected < polygon.xpoints.length) {
          moveVertex(polygon, selected, e.getX(), e.getY());
        }
        scrollAtEdge(e.getPoint(), 15);
        repaint();
      }
    }

    @Override
    public void mouseClicked(MouseEvent e) {
      if (!SwingUtils.isMainMouseButtonDown(e) || e.getClickCount() == 1) {
        return;
      }

      final Triple<Integer, Point, Double> t = nearestSegment(polygon, e.getX(), e.getY());
      final double d = t.getRight();
      if (d <= CLICK_THRESHOLD) {
        final int ins = t.getLeft() + 1;
        final Point np = t.getMiddle();
        insertVertex(polygon, ins, np.x, np.y);
        selected = ins;
        repaint();
      }
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (!SwingUtils.isMainMouseButtonDown(e)) {
        return;
      }

      // On left button press, select nearest vertex within the threshold.
      final Pair<Integer, Double> n = nearestVertex(polygon, e.getX(), e.getY());
      final double d = n.getRight();
      selected = d <= CLICK_THRESHOLD ? n.getLeft() : -1;

      repaint();
    }

    public void scrollAtEdge(Point evtPt, int dist) {
      final Point p = new Point(
        evtPt.x - myScroll.getViewport().getViewPosition().x,
        evtPt.y - myScroll.getViewport().getViewPosition().y
      );
      int dx = 0, dy = 0;
      if (p.x < dist && p.x >= 0) {
        dx = -1;
      }
      if (p.x >= myScroll.getViewport().getSize().width - dist
          && p.x < myScroll.getViewport().getSize().width) {
        dx = 1;
      }
      if (p.y < dist && p.y >= 0) {
        dy = -1;
      }
      if (p.y >= myScroll.getViewport().getSize().height - dist
          && p.y < myScroll.getViewport().getSize().height) {
        dy = 1;
      }

      if (dx != 0 || dy != 0) {
        Rectangle r = new Rectangle(myScroll.getViewport().getViewRect());
        r.translate(2 * dist * dx, 2 * dist * dy);
        r = r.intersection(new Rectangle(new Point(0, 0), getPreferredSize()));
        scrollRectToVisible(r);
      }
    }

    public void deleteKeyPressed() {
      if (selected >= 0) {
        deleteVertex(polygon, selected);
        selected = -1;

        // It's not possible to add a point when there's no segment to click
        // so remove the whole polygon
        if (polygon.npoints < 2) {
          polygon = null;

          removeMouseListener(this);
          removeMouseMotionListener(this);
          getInputMap(WHEN_IN_FOCUSED_WINDOW).remove(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
          setupForCreate();
        }

        repaint();
      }
    }
  }

  private static Polygon pathToPolygon(List<Point> pl) {
    final int ps = pl.size();
    final int[] xpoints = new int[ps];
    final int[] ypoints = new int[ps];

    for (int i = 0; i < ps; ++i) {
      final Point p = pl.get(i);
      xpoints[i] = p.x;
      ypoints[i] = p.y;
    }

    return new Polygon(xpoints, ypoints, ps);
  }

  private class DefinePicker extends MouseInputAdapter {
    @Override
    public void mousePressed(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        path.add(new Point(e.getPoint()));
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        removeMouseListener(this);
        removeMouseMotionListener(this);

        final DefinePolygon dp = new DefinePolygon();
        addMouseListener(dp);
        addMouseMotionListener(dp);

        getInputMap(WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), ESCAPE);
        getActionMap().put(ESCAPE, new AbstractAction() {
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            dp.escapeKeyPressed();
          }
        });

        path.add(new Point(path.get(0)));

        repaint();
      }
    }

    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        removeMouseListener(this);
        removeMouseMotionListener(this);

        final DefineRectangle dr = new DefineRectangle();
        addMouseListener(dr);
        addMouseMotionListener(dr);

        final int x = path.get(0).x;
        final int[] xpoints = { x, x, x, x };

        final int y = path.get(0).y;
        final int[] ypoints = { y, y, y, y };

        polygon = new Polygon(xpoints, ypoints, 4);

        repaint();
      }
    }
  }

  private class DefinePolygon extends MouseInputAdapter {
    @Override
    public void mouseReleased(MouseEvent e) {
      if (!SwingUtils.isMainMouseButtonDown(e) || path.isEmpty()) {
        return;
      }

      if (path.size() > 2) {
        final Point beg = path.get(0);
        final double d = Point2D.distance(e.getX(), e.getY(), beg.x, beg.y);
        if (d <= CLICK_THRESHOLD) {
          path.remove(path.size() - 1);

          polygon = pathToPolygon(path);
          selected = 0;
          path = null;

          removeMouseListener(this);
          removeMouseMotionListener(this);
          getInputMap(WHEN_IN_FOCUSED_WINDOW).remove(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0));
          setupForEdit();
          return;
        }
      }

      path.add(new Point(e.getPoint()));
      repaint();
    }

    private void moveEndpoint(Point p) {
      if (!path.isEmpty()) {
        path.get(path.size() - 1).setLocation(p);
        repaint();
      }
    }

    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        moveEndpoint(e.getPoint());
      }
    }

    @Override
    public void mouseMoved(MouseEvent e) {
      moveEndpoint(e.getPoint());
    }

    public void escapeKeyPressed() {
      path.clear();
      removeMouseListener(this);
      removeMouseMotionListener(this);
      getInputMap(WHEN_IN_FOCUSED_WINDOW).remove(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0));
      setupForCreate();
    }
  }

  private class DefineRectangle extends MouseInputAdapter {
    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        polygon.xpoints[1] = polygon.xpoints[2] = e.getX();
        polygon.ypoints[2] = polygon.ypoints[3] = e.getY();
        repaint();
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        selected = nearestVertex(polygon, e.getX(), e.getY()).getLeft();

        removeMouseListener(this);
        removeMouseMotionListener(this);
        setupForEdit();
      }
    }
  }

  public static void main(String[] args) {
    final JFrame f = new JFrame();
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
