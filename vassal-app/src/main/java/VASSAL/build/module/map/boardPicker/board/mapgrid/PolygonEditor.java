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
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;
import java.util.Arrays;

import javax.swing.AbstractAction;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JDialog;
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

  protected JScrollPane myScroll;

  private static final String DELETE = "Delete";

  private static final int POINT_RADIUS = 10;
  private static final int CLICK_THRESHOLD = 10;

  private Zone.Editor myConfigurer;
  private JDialog myFrame;

  public PolygonEditor(Polygon p) {
    polygon = p;
    setFocusable(true);
    setFocusTraversalKeysEnabled(false);
  }

  public void setMyConfigurer(Zone.Editor myConfigurer) {
    this.myConfigurer = myConfigurer;
  }

  public void setMyFrame(JDialog frame) {
    this.myFrame = frame;
    myFrame.setFocusable(true);
    myFrame.setFocusTraversalKeysEnabled(false);
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

    if (myFrame != null) {
      final KeyListener[] kl = myFrame.getKeyListeners();
      for (final KeyListener i : kl) {
        removeKeyListener(i);
      }
    }

    final InputMap im = getInputMap(WHEN_IN_FOCUSED_WINDOW);
    im.remove(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));

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

  private void updateAllCoords() {
    if (myConfigurer != null) {
      myConfigurer.updateCoords(polygon);
      if (selected >= 0) {
        myConfigurer.updateCoord(polygon.xpoints[selected], polygon.ypoints[selected]);
      }
      else {
        myConfigurer.updateCoord("");
      }
    }
  }

  private void setupForCreate() {
    new DefineRectangle();
    selected = -1;
    updateAllCoords();
    requestFocus();
    repaint();
  }

  private void setupForEdit() {
    new ModifyPolygon();
    if (selected >= polygon.npoints) selected = -1;
    updateAllCoords();
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

  public void scrollAtEdge(Point p, int dist) {
    p = new Point(
      p.x - myScroll.getViewport().getViewPosition().x,
      p.y - myScroll.getViewport().getViewPosition().y
    );
    int dx = 0, dy = 0;

    if (p.x < dist && p.x >= 0) {
      dx = -1;
    }

    if (p.x >= myScroll.getViewport().getSize().width - dist &&
      p.x < myScroll.getViewport().getSize().width) {
      dx = 1;
    }

    if (p.y < dist && p.y >= 0) {
      dy = -1;
    }

    if (p.y >= myScroll.getViewport().getSize().height - dist &&
      p.y < myScroll.getViewport().getSize().height) {
      dy = 1;
    }

    if (dx != 0 || dy != 0) {
      Rectangle r = new Rectangle(myScroll.getViewport().getViewRect());
      r.translate(2 * dist * dx, 2 * dist * dy);
      r = r.intersection(new Rectangle(new Point(0, 0), getPreferredSize()));
      scrollRectToVisible(r);
    }
  }

  public static void reset(Polygon p, String pathStr) {
    if (p == null) {
      p = new Polygon();
    }
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
    if (p.npoints == 0) {
      p = null;
    }
  }

  public static String polygonToString(Polygon p) {
    // Sometimes people delete all the points from the polygon. Because of course they do.
    if ((p == null) || p.npoints == 0) {
      return "";
    }

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

    if (polygon == null || polygon.npoints == 0) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    g2d.addRenderingHints(SwingUtils.FONT_HINTS);
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));

    final int r = POINT_RADIUS;
    final int d = 2 * r;

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

  private class ModifyPolygon extends MouseInputAdapter implements KeyListener {
    public ModifyPolygon() {
      addMouseListener(this);
      addMouseMotionListener(this);
      if (myFrame != null) {
        myFrame.addKeyListener(this);

        final InputMap inputMap = myScroll.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        inputMap.put(KeyStroke.getKeyStroke("RIGHT"), "do-nothing");
        inputMap.put(KeyStroke.getKeyStroke("LEFT"), "do-nothing");
        inputMap.put(KeyStroke.getKeyStroke("UP"), "do-nothing");
        inputMap.put(KeyStroke.getKeyStroke("DOWN"), "do-nothing");
      }

      getInputMap(WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), DELETE);
      getActionMap().put(DELETE, new AbstractAction() {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          deleteKeyPressed();
        }
      });
    }

    private void remove() {
      if (myFrame != null) {
        removeKeyListener(this);
      }
      removeMouseListener(this);
      removeMouseMotionListener(this);
      getInputMap(WHEN_IN_FOCUSED_WINDOW).remove(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
    }

    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        if (selected >= 0 && selected < polygon.xpoints.length) {
          moveVertex(polygon, selected, e.getX(), e.getY());
          if (myConfigurer != null) {
            myConfigurer.updateCoord(e.getX(), e.getY());
          }
        }
        scrollAtEdge(e.getPoint(), 15);
        repaint();

        if (myConfigurer != null) {
          myConfigurer.updateCoords(polygon);
        }
      }
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        // On left button press, select nearest vertex within the threshold.
        final Pair<Integer, Double> n = nearestVertex(polygon, e.getX(), e.getY());
        final double d = n.getRight();
        selected = d <= CLICK_THRESHOLD ? n.getLeft() : -1;

        if (myConfigurer != null) {
          if (selected >= 0) {
            myConfigurer.updateCoord(polygon.xpoints[selected], polygon.ypoints[selected]);
          }
          else {
            myConfigurer.updateCoord("");
          }
        }

        repaint();
      }
      else if (SwingUtils.isContextMouseButtonDown(e)) {
        // On right button press, create a new vertex.
        final int ins = nearestSegment(polygon, e.getX(), e.getY()).getLeft() + 1;
        insertVertex(polygon, ins, e.getX(), e.getY());
        selected = ins;
        repaint();

        if (myConfigurer != null) {
          myConfigurer.updateCoords(polygon);
          if (selected >= 0) {
            myConfigurer.updateCoord(polygon.xpoints[selected], polygon.ypoints[selected]);
          }
          else {
            myConfigurer.updateCoord("");
          }
        }
      }
    }

    @Override
    public void keyReleased(KeyEvent e) {
    }

    @Override
    public void keyTyped(KeyEvent e) {
    }

    @Override
    public void keyPressed(KeyEvent e) {
      if (selected < 0) {
        return;
      }

      int dx = 0, dy = 0, delta = 1;

      if (e.isShiftDown()) {
        delta = 5;
      }

      switch (e.getKeyCode()) {
      case KeyEvent.VK_UP:
        dy = -delta;
        break;
      case KeyEvent.VK_DOWN:
        dy = delta;
        break;
      case KeyEvent.VK_LEFT:
        dx = -delta;
        break;
      case KeyEvent.VK_RIGHT:
        dx = delta;
        break;
      default:
        return;
      }

      moveVertex(polygon, selected, polygon.xpoints[selected] + dx, polygon.ypoints[selected] + dy);
      repaint();

      if (myConfigurer != null) {
        if (selected >= 0) {
          myConfigurer.updateCoord(polygon.xpoints[selected], polygon.ypoints[selected]);
        }
        myConfigurer.updateCoords();
      }
    }

    public void deleteKeyPressed() {
      if (selected >= 0) {
        deleteVertex(polygon, selected);
        selected = -1;

        if (polygon.npoints == 0) {
          // Back to create mode if all points are gone.
          polygon = null;
          remove();
          setupForCreate();
        }

        repaint();

        if (myConfigurer != null) {
          myConfigurer.updateCoord("");
          myConfigurer.updateCoords(polygon);
        }
      }
    }
  }

  private class DefineRectangle extends MouseInputAdapter {
    public DefineRectangle() {
      addMouseListener(this);
    }

    private void remove() {
      removeMouseListener(this);
      removeMouseMotionListener(this);
    }

    private void resetPolygon(int x, int y, int n) {
      final int[] xpoints = new int[n];
      Arrays.fill(xpoints, x);
      final int[] ypoints = new int[n];
      Arrays.fill(ypoints, y);
      polygon = new Polygon(xpoints, ypoints, n);
    }

    @Override
    public void mousePressed(MouseEvent e) {
      if (polygon == null || polygon.npoints == 0) {
        if (SwingUtils.isMainMouseButtonDown(e)) {
          resetPolygon(e.getX(), e.getY(), 4);
          addMouseMotionListener(this);
          repaint();
          if (myConfigurer != null) {
            myConfigurer.updateCoords(polygon);
            myConfigurer.updateCoord(e.getX(), e.getY());
          }
        }
        else if (SwingUtils.isContextMouseButtonDown(e)) {
          remove();
          resetPolygon(e.getX(), e.getY(), 1);
          selected = 0;
          setupForEdit();
          if (myConfigurer != null) {
            myConfigurer.updateCoords(polygon);
            myConfigurer.updateCoord(polygon.xpoints[selected], polygon.ypoints[selected]);
          }
        }
      }
    }

    @Override
    public void mouseDragged(MouseEvent e) {
      if (SwingUtils.isMainMouseButtonDown(e)) {
        polygon.xpoints[1] = polygon.xpoints[2] = e.getX();
        polygon.ypoints[2] = polygon.ypoints[3] = e.getY();
        repaint();
        if (myConfigurer != null) {
          myConfigurer.updateCoords(polygon);

          final String s = polygon.xpoints[0] +
            "," +
            polygon.ypoints[0] +
            " => " +
            polygon.xpoints[2] +
            "," +
            polygon.ypoints[2];
          myConfigurer.updateCoord(s);
        }
      }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
      if (polygon != null && polygon.npoints == 4 &&
        SwingUtils.isMainMouseButtonDown(e)) {
        if ((Math.abs(polygon.xpoints[0] - polygon.xpoints[2]) +
          Math.abs(polygon.ypoints[0] - polygon.ypoints[2])) < 20) {
          polygon.xpoints[1] = polygon.xpoints[2] = polygon.xpoints[0] + 25;
          polygon.ypoints[2] = polygon.ypoints[3] = polygon.ypoints[0] + 25;
        }

        remove();
        selected = nearestVertex(polygon, e.getX(), e.getY()).getLeft();
        setupForEdit();
        if (myConfigurer != null) {
          myConfigurer.updateCoords(polygon);
          myConfigurer.updateCoord("");
        }
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