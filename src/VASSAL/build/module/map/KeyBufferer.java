/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney, Joel Uckelman
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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Stroke;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Immobilized;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

/**
 * This component listens for mouse clicks on a map and draws the selection
 * rectangle.
 *
 * If the user clicks on a {@link GamePiece}, that piece is added to the
 * {@link KeyBuffer}. {@link #draw(Graphics, Map)} is responsible for
 * drawing the mouse selection rectangle, and
 * {@link #mouseDragged(MouseEvent)} is responsible for triggering repaint
 * events as the selection rectangle is moved.
 *
 * @see Map#addLocalMouseListener
 */
public class KeyBufferer extends MouseAdapter implements Buildable, MouseMotionListener, Drawable {
  protected Map map;
  protected Rectangle selection;
  protected Point anchor;
  protected Color color = Color.black;
  protected int thickness = 3;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListenerFirst(this);
    map.getView().addMouseMotionListener(this);
    map.addDrawComponent(this);
  }

  public void add(Buildable b) {
  }

  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  public void build(Element e) {
  }

  public void mousePressed(MouseEvent e) {
    if (e.isConsumed()) {
      return;
    }
    GamePiece p = map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK);
    // Don't clear the buffer until we find the clicked-on piece
    // Because selecting a piece affects its visibility
    EventFilter filter = null;
    if (p != null) {
      filter = (EventFilter) p.getProperty(Properties.SELECT_EVENT_FILTER);
    }
    boolean ignoreEvent = filter != null && filter.rejectEvent(e);
    if (p != null && !ignoreEvent) {
      boolean movingStacksPickupUnits = ((Boolean) GameModule.getGameModule().getPrefs().getValue(Map.MOVING_STACKS_PICKUP_UNITS)).booleanValue();
      if (!KeyBuffer.getBuffer().contains(p)) {
        if (!e.isShiftDown() && !e.isControlDown()) {
          KeyBuffer.getBuffer().clear();
        }
        // RFE 1629255 - If the top piece of an unexpanded stack is left-clicked
        // while not selected, then select all of the pieces in the stack
        // RFE 1659481 - Control clicking only deselects
        if (!e.isControlDown()) {
          if (movingStacksPickupUnits || p.getParent() == null || p.getParent().isExpanded() || e.isMetaDown()
              || Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
            KeyBuffer.getBuffer().add(p);
          }
          else {
            Stack s = p.getParent();
            for (int i = 0; i < s.getPieceCount(); i++) {
              KeyBuffer.getBuffer().add(s.getPieceAt(i));
            }
          }
        }
        // End RFE 1629255
      }
      else {
        // RFE 1659481 Ctrl-click deselects clicked units
        if (e.isControlDown() && Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
          Stack s = p.getParent();
          if (s == null) {
            KeyBuffer.getBuffer().remove(p);
          }
          else if (!s.isExpanded()) {
            for (int i = 0; i < s.getPieceCount(); i++) {
              KeyBuffer.getBuffer().remove(s.getPieceAt(i));
            }
          }
        }
        // End RFE 1659481
      }
      if (p.getParent() != null) {
        map.getPieceCollection().moveToFront(p.getParent());
      }
      else {
        map.getPieceCollection().moveToFront(p);
      }
    }
    else {
      if (!e.isShiftDown() && !e.isControlDown()) { // No deselect if shift key down
        KeyBuffer.getBuffer().clear();
      }
      anchor = map.componentCoordinates(e.getPoint());
      selection = new Rectangle(anchor.x, anchor.y, 0, 0);
      if (map.getHighlighter() instanceof ColoredBorder) {
        ColoredBorder b = (ColoredBorder) map.getHighlighter();
        color = b.getColor();
        thickness = b.getThickness();
      }
    }
  }

  public void mouseReleased(MouseEvent evt) {
    if (selection != null) {
      selection.setLocation(map.mapCoordinates(selection.getLocation()));
      selection.width /= map.getZoom();
      selection.height /= map.getZoom();
      PieceVisitorDispatcher d = createDragSelector(!evt.isControlDown(), evt.isAltDown());
      // RFE 1659481 Don't clear the entire selection buffer if either shift
      // or control is down - we select/deselect lassoed counters instead
      if (!evt.isShiftDown() && !evt.isControlDown()) {
        KeyBuffer.getBuffer().clear();
      }
      map.apply(d);
      repaintSelectionRect();
    }
    selection = null;
  }

  /**
   * This PieceVisitorDispatcher determines what to do with pieces on the
   * map when the player finished dragging a rectangle to select pieces
   *
   * @return
   */
  protected PieceVisitorDispatcher createDragSelector(boolean selecting, boolean altDown) {
    return new PieceVisitorDispatcher(new KBDeckVisitor(selecting, altDown));
  }

  public class KBDeckVisitor implements DeckVisitor {
    boolean selecting = false;
    boolean altDown = false;

    public KBDeckVisitor(boolean b, boolean c) {
      selecting = b;
      altDown = c;
    }

    public Object visitDeck(Deck d) {
      return null;
    }

    public Object visitStack(Stack s) {
      if (s.topPiece() != null) {
        if (s.isExpanded()) {
          Point[] pos = new Point[s.getPieceCount()];
          map.getStackMetrics().getContents(s, pos, null, null, s.getPosition().x, s.getPosition().y);
          for (int i = 0; i < pos.length; ++i) {
            if (selection.contains(pos[i])) {
              if (selecting) {
                KeyBuffer.getBuffer().add(s.getPieceAt(i));
              }
              else {
                KeyBuffer.getBuffer().remove(s.getPieceAt(i));
              }
            }
          }
        }
        else if (selection.contains(s.getPosition())) {
          for (int i = 0, n = s.getPieceCount(); i < n; ++i) {
            if (selecting) {
              KeyBuffer.getBuffer().add(s.getPieceAt(i));
            }
            else {
              KeyBuffer.getBuffer().remove(s.getPieceAt(i));
            }
          }
        }
      }
      return null;
    }

    // Handle non-stacked units, including Does Not Stack units
    // Does Not Stack units deselect normally once selected
    public Object visitDefault(GamePiece p) {
      if (selection.contains(p.getPosition()) && !Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))) {
        if (selecting) {
          final EventFilter filter = (EventFilter) p.getProperty(Properties.SELECT_EVENT_FILTER);
          final boolean altSelect = (altDown && filter instanceof Immobilized.UseAlt);
          if (filter == null || altSelect) {
            KeyBuffer.getBuffer().add(p);
          }
        }
        else {
          KeyBuffer.getBuffer().remove(p);
        }
      }
      return null;
    }
  }

  protected void repaintSelectionRect() {
    /*
     * Repaint strategy: There is no reason to repaint the interior of
     * the selection rectangle, as we didn't paint over it in the first
     * place. Instead, we repaint only the four (slender) rectangles
     * which the stroke of the selection rectangle filled. We have to
     * call a repaint on both the old selection rectangle and the new
     * in order to prevent drawing artifacts. The KeyBuffer handles
     * repainting pieces which have been (de)selected, so we don't
     * worry about those.
     *
     * Area drawn:
     *                selection.x
     *                     |
     *                  ___________________
     *   selection.y __ |__|__|_____|__|__| __
     *                  |__|__|_____|__|__|   |
     *                  |  |  |     |  |  |   |
     *                  |  |  |     |  |  |   | selection.height
     *                  |__|__|_____|__|__|   |
     * ~thickness/2 --{ |__|__|_____|__|__| __|
     * ~thickness/2 --{ |__|__|_____|__|__|
     *
     *                     |___________|
     *                    selection.width
     */
    final int ht = thickness / 2 + thickness % 2;
    final int ht2 = 2*ht;

    // left
    map.getView().repaint(selection.x - ht,
                          selection.y - ht,
                          ht2,
                          selection.height + ht2);
    // right
    map.getView().repaint(selection.x + selection.width - ht,
                          selection.y - ht,
                          ht2,
                          selection.height + ht2);
    // top
    map.getView().repaint(selection.x - ht,
                          selection.y - ht,
                          selection.width + ht2,
                          ht2);
    // bottom
    map.getView().repaint(selection.x - ht,
                          selection.y + selection.width - ht,
                          selection.width + ht2,
                          ht2);
  }

  /**
   * Sets the new location of the selection rectangle.
   */
  public void mouseDragged(MouseEvent e) {
    if (selection != null) {
      repaintSelectionRect();

      selection.x = Math.min(e.getX(), anchor.x);
      selection.y = Math.min(e.getY(), anchor.y);
      selection.width = Math.abs(e.getX() - anchor.x);
      selection.height = Math.abs(e.getY() - anchor.y);

      repaintSelectionRect();
    }
  }

  public void mouseMoved(MouseEvent e) {
  }

  public void draw(Graphics g, Map map) {
    if (selection != null) {
      final Graphics2D g2d = (Graphics2D) g;
      final Stroke str = g2d.getStroke();
      g2d.setStroke(new BasicStroke(thickness));
      g2d.setColor(color);
      g2d.drawRect(selection.x, selection.y, selection.width, selection.height);
      g2d.setStroke(str);
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }
}
