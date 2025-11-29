package VASSAL.build.module.map;

import VASSAL.build.module.Map;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Point;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.concurrent.CountDownLatch;

/**
 * Low-level interaction handler for drawing and adjusting a rectangle on a map view.
 * Attaches mouse/key listeners to the map view, draws the translucent rectangle,
 * and resolves when the user presses Enter (accept) or Esc (cancel).
 * <p>
 * This class owns no lifecycle beyond its own listeners; a higher-level helper
 * (see {@link RectangularSelector}) is expected to add/remove the overlay and
 * handle concerns like disabling the key buffer.
 */
class RectangularSelectionOverlay extends MouseAdapter implements MouseWheelListener, Drawable {
  private final Map map;
  private final CountDownLatch latch = new CountDownLatch(1);
  private Rectangle selection;
  private Rectangle initialSelection;
  private Point anchorMap;
  private DragMode dragMode = DragMode.NONE;
  private ResizeMode resizeMode = ResizeMode.NONE;

  enum DragMode { NONE, CREATE, MOVE, RESIZE }

  enum ResizeMode {
    NONE(false, false, false, false),
    LEFT(true, false, false, false),
    RIGHT(false, true, false, false),
    TOP(false, false, true, false),
    BOTTOM(false, false, false, true),
    TOP_LEFT(true, false, true, false),
    TOP_RIGHT(false, true, true, false),
    BOTTOM_LEFT(true, false, false, true),
    BOTTOM_RIGHT(false, true, false, true);

    final boolean left;
    final boolean right;
    final boolean top;
    final boolean bottom;

    ResizeMode(boolean l, boolean r, boolean t, boolean b) {
      this.left = l;
      this.right = r;
      this.top = t;
      this.bottom = b;
    }
  }

  RectangularSelectionOverlay(Map map, Rectangle initial) {
    this.map = map;
    this.selection = initial != null ? new Rectangle(initial) : null;
  }

  Rectangle awaitSelection() {
    map.addDrawComponent(this);
    map.getView().addMouseListener(this);
    map.getView().addMouseMotionListener(this);
    map.getView().addMouseWheelListener(this);
    map.getView().addKeyListener(keyForwarder);
    map.getView().requestFocusInWindow();
    try {
      latch.await();
    }
    catch (InterruptedException ie) {
      Thread.currentThread().interrupt();
    }
    finally {
      map.removeDrawComponent(this);
      map.getView().removeMouseListener(this);
      map.getView().removeMouseMotionListener(this);
      map.getView().removeMouseWheelListener(this);
      map.getView().removeKeyListener(keyForwarder);
    }
    return selection != null && selection.width > 0 && selection.height > 0 ? new Rectangle(selection) : null;
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (selection == null) {
      return;
    }
    final Graphics2D g2 = (Graphics2D) g.create();
    final Rectangle r = map.mapToDrawing(selection, 1.0);
    g2.setColor(new Color(0, 120, 215, 60));
    g2.fill(r);
    g2.setColor(new Color(0, 120, 215, 160));
    g2.setStroke(new BasicStroke(2f));
    g2.draw(r);
    g2.dispose();
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  @Override
  public void mousePressed(MouseEvent e) {
    final java.awt.Point compPt = e.getPoint();
    anchorMap = map.componentToMap(compPt);
    initialSelection = selection != null ? new Rectangle(selection) : null;

    ResizeMode hit = hitTest(compPt);
    if (hit != ResizeMode.NONE) {
      dragMode = DragMode.RESIZE;
      resizeMode = hit;
      return;
    }
    if (selection != null) {
      Rectangle compSel = map.mapToComponent(selection);
      if (compSel.contains(compPt)) {
        dragMode = DragMode.MOVE;
        return;
      }
    }
    dragMode = DragMode.CREATE;
    selection = new Rectangle(anchorMap.x, anchorMap.y, 0, 0);
    resizeMode = ResizeMode.NONE;
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    if (anchorMap == null) {
      return;
    }
    final java.awt.Point mapPt = map.componentToMap(e.getPoint());
    switch (dragMode) {
    case CREATE: {
      Rectangle r = new Rectangle(
        Math.min(anchorMap.x, mapPt.x),
        Math.min(anchorMap.y, mapPt.y),
        Math.abs(anchorMap.x - mapPt.x),
        Math.abs(anchorMap.y - mapPt.y)
      );
      selection = clamp(r);
      break;
    }
    case MOVE: {
      if (initialSelection != null) {
        int dx = mapPt.x - anchorMap.x;
        int dy = mapPt.y - anchorMap.y;
        Rectangle moved = new Rectangle(
          initialSelection.x + dx,
          initialSelection.y + dy,
          initialSelection.width,
          initialSelection.height
        );
        selection = clamp(moved);
      }
      break;
    }
    case RESIZE: {
      if (initialSelection != null) {
        Rectangle r = new Rectangle(initialSelection);
        int dx = mapPt.x - anchorMap.x;
        int dy = mapPt.y - anchorMap.y;
        if (resizeMode.left) {
          r.x += dx;
          r.width -= dx;
        }
        if (resizeMode.right) {
          r.width += dx;
        }
        if (resizeMode.top) {
          r.y += dy;
          r.height -= dy;
        }
        if (resizeMode.bottom) {
          r.height += dy;
        }
        selection = clamp(r);
      }
      break;
    }
    case NONE:
      break;
    }
    map.repaint();
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    anchorMap = null;
    initialSelection = null;
    dragMode = DragMode.NONE;
    resizeMode = ResizeMode.NONE;
  }

  @Override
  public void mouseWheelMoved(MouseWheelEvent e) {
    // Let the map handle wheel normally; do not consume.
  }

  private ResizeMode hitTest(java.awt.Point compPt) {
    if (selection == null) {
      return ResizeMode.NONE;
    }
    final int margin = 6;
    Rectangle compRect = map.mapToComponent(selection);
    boolean left = Math.abs(compPt.x - compRect.x) <= margin;
    boolean right = Math.abs(compPt.x - (compRect.x + compRect.width)) <= margin;
    boolean top = Math.abs(compPt.y - compRect.y) <= margin;
    boolean bottom = Math.abs(compPt.y - (compRect.y + compRect.height)) <= margin;

    if (left && top) return ResizeMode.TOP_LEFT;
    if (right && top) return ResizeMode.TOP_RIGHT;
    if (left && bottom) return ResizeMode.BOTTOM_LEFT;
    if (right && bottom) return ResizeMode.BOTTOM_RIGHT;
    if (left) return ResizeMode.LEFT;
    if (right) return ResizeMode.RIGHT;
    if (top) return ResizeMode.TOP;
    if (bottom) return ResizeMode.BOTTOM;
    return ResizeMode.NONE;
  }

  private Rectangle clamp(Rectangle r) {
    Rectangle clamped = new Rectangle(r);
    clamped.x = Math.max(0, clamped.x);
    clamped.y = Math.max(0, clamped.y);
    clamped.width = Math.min(clamped.width, map.mapSize().width - clamped.x);
    clamped.height = Math.min(clamped.height, map.mapSize().height - clamped.y);
    if (clamped.width < 1) clamped.width = 1;
    if (clamped.height < 1) clamped.height = 1;
    return clamped;
  }

  private final KeyAdapter keyForwarder = new KeyAdapter() {
    @Override
    public void keyPressed(KeyEvent e) {
      if (e.getKeyCode() == KeyEvent.VK_ENTER) {
        latch.countDown();
      }
      else if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
        selection = null;
        latch.countDown();
      }
    }
  };
}
