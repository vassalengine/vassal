/*
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

import javax.swing.JComponent;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.Decorator;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Immobilized;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.NonRectangular;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.tools.swing.SwingUtils;

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
  protected GamePiece bandSelectPiece = null;

  private enum BandSelectType {
    NONE,
    NORMAL,
    SPECIAL;
  }

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListenerFirst(this);
    map.getView().addMouseMotionListener(this);
    map.addDrawComponent(this);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  @Override
  public void build(Element e) {
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (e.isConsumed()) {
      return;
    }

    final KeyBuffer kbuf = KeyBuffer.getBuffer();

    GamePiece p = map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK);
    // Don't clear the buffer until we find the clicked-on piece
    // Because selecting a piece affects its visibility
    EventFilter filter = null;
    BandSelectType bandSelect = BandSelectType.NONE;
    if (p != null) {
      filter = (EventFilter) p.getProperty(Properties.SELECT_EVENT_FILTER);      
      if (!e.isPopupTrigger() && Boolean.TRUE.equals(p.getProperty(Properties.NON_MOVABLE))) {
        bandSelect = BandSelectType.SPECIAL; //BR// Don't "eat" band-selects if unit found is non-movable
      }
    }
    boolean ignoreEvent = filter != null && filter.rejectEvent(e);
    if (p != null && !ignoreEvent) {
      boolean movingStacksPickupUnits = (Boolean) GameModule.getGameModule().getPrefs().getValue(Map.MOVING_STACKS_PICKUP_UNITS);
      if (!kbuf.contains(p)) {
        if (!e.isShiftDown() && !e.isControlDown()) {
          kbuf.clear();
        }
        // RFE 1629255 - If the top piece of an unexpanded stack is left-clicked
        // while not selected, then select all of the pieces in the stack
        // RFE 1659481 - Control clicking only deselects
        if (!e.isControlDown()) {
          if (movingStacksPickupUnits ||
              p.getParent() == null ||
              p.getParent().isExpanded() ||
              SwingUtils.isRightMouseButton(e) ||
              Boolean.TRUE.equals(p.getProperty(Properties.SELECTED)))
          {
            kbuf.add(p);
          }
          else {
            Stack s = p.getParent();
            s.asList().forEach(gamePiece -> KeyBuffer.getBuffer().add(gamePiece));
          }
        }
        // End RFE 1629255
      }
      else {
        // RFE 1659481 Ctrl-click deselects clicked units
        if (e.isControlDown() && Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
          Stack s = p.getParent();
          if (s == null) {
            kbuf.remove(p);
          }
          else if (!s.isExpanded()) {
            s.asList().forEach(gamePiece -> KeyBuffer.getBuffer().remove(gamePiece));
          }
        }
        // End RFE 1659481
      }

      final GamePiece to_front = p.getParent() != null ? p.getParent() : p;
      map.getPieceCollection().moveToFront(to_front);
    }
    else {
      bandSelect = BandSelectType.NORMAL; //BR// Allowed to band-select  
    }
    
    if (bandSelect != BandSelectType.NONE) {
      bandSelectPiece = null;
      if (!e.isShiftDown() && !e.isControlDown()) { // No deselect if shift key down
        kbuf.clear();
        
        //BR// This section allows band-select to be attempted from non-moving pieces w/o preventing click-to-select from working 
        if ((bandSelect == BandSelectType.SPECIAL) && (p != null) && !ignoreEvent) {
          kbuf.add(p);
          bandSelectPiece = p; 
        }
      }
      anchor = map.mapToComponent(e.getPoint());
      selection = new Rectangle(anchor.x, anchor.y, 0, 0);
      if (map.getHighlighter() instanceof ColoredBorder) {
        ColoredBorder b = (ColoredBorder) map.getHighlighter();
        color = b.getColor();
        thickness = b.getThickness();
      }
    }
  }

  @Override
  public void mouseReleased(MouseEvent evt) {
    if (selection == null) {
      return;
    }

    PieceVisitorDispatcher d = createDragSelector(
      !evt.isControlDown(), evt.isAltDown(), map.componentToMap(selection)
    );
    
    //BR// If it was a legit band-select drag (not just a click), our special case only applies if piece is allowed to be band-selected    
    if (bandSelectPiece != null) {
      final EventFilter bandFilter = (EventFilter) bandSelectPiece.getProperty(Properties.BAND_SELECT_EVENT_FILTER);
      final boolean pieceAllowedToBeBandSelected = (bandFilter != null) && !(evt.isAltDown() && (bandFilter instanceof Immobilized.UseAlt));

      if (pieceAllowedToBeBandSelected) {
        final Point finish = map.mapToComponent(evt.getPoint());
        
        //BR// Open to suggestions about a better way to distinguish "click" from "lasso" (not that Vassal doesn't already suck immensely at click-vs-drag threshhold). FWIW, this "works".
        final boolean isLasso = finish.distance(anchor) >= 10;
        if (isLasso) { 
          bandSelectPiece = null;
        }
      }
    }
    
    // RFE 1659481 Don't clear the entire selection buffer if either shift
    // or control is down - we select/deselect lassoed counters instead
    if ((bandSelectPiece == null) && !evt.isShiftDown() && !evt.isControlDown()) {
      KeyBuffer.getBuffer().clear();
    }
    map.apply(d);
    repaintSelectionRect();
    selection = null;
  }

  /**
   * This PieceVisitorDispatcher determines what to do with pieces on the
   * map when the player finished dragging a rectangle to select pieces
   *
   * @return
   */
  protected PieceVisitorDispatcher createDragSelector(
    boolean selecting,
    boolean altDown,
    Rectangle mapsel)
  {
    return new PieceVisitorDispatcher(
      new KBDeckVisitor(selecting, altDown, mapsel)
    );
  }

  public class KBDeckVisitor implements DeckVisitor {
    boolean selecting = false;
    boolean altDown = false;
    Rectangle mapsel;

    public KBDeckVisitor(boolean b, boolean c, Rectangle ms) {
      selecting = b;
      altDown = c;
      mapsel = ms;
    }

    @Override
    public Object visitDeck(Deck d) {
      return null;
    }

    @Override
    public Object visitStack(Stack s) {
      if (s.topPiece() != null) {
        final KeyBuffer kbuf = KeyBuffer.getBuffer();
        if (s instanceof Deck) {
          s.asList().forEach(gamePiece -> kbuf.remove(gamePiece)); // Clear any deck *members* out of the KeyBuffer.
          return null;
        }
        if (s.isExpanded()) {
          Point[] pos = new Point[s.getPieceCount()];
          map.getStackMetrics().getContents(s, pos, null, null, s.getPosition().x, s.getPosition().y);
          for (int i = 0; i < pos.length; ++i) {
            if (mapsel.contains(pos[i])) {
              if (selecting) {
                kbuf.add(s.getPieceAt(i));
              }
              else {
                kbuf.remove(s.getPieceAt(i));
              }
            }
          }
        }
        else if (mapsel.contains(s.getPosition())) {
          s.asList().forEach(selecting ?
            gamePiece -> kbuf.add(gamePiece) :
            gamePiece -> kbuf.remove(gamePiece)
          );
        }
      }
      return null;
    }

    // Handle non-stacked units, including Does Not Stack units
    // Does Not Stack units deselect normally once selected
    @Override
    public Object visitDefault(GamePiece p) {
      Stack s = p.getParent();
      if (s != null && s instanceof Deck) {
        // Clear any deck *members* out of the KeyBuffer.
        // (yes, members of decks can be does-not-stack)
        KeyBuffer.getBuffer().remove(p);
        return null;
      }
      if (mapsel.contains(p.getPosition()) && !Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))) {
        if (selecting) {
          final EventFilter filter = (EventFilter) p.getProperty(Properties.SELECT_EVENT_FILTER);
          final EventFilter bandFilter = (EventFilter) p.getProperty(Properties.BAND_SELECT_EVENT_FILTER);
          final boolean altSelect = (altDown && filter instanceof Immobilized.UseAlt);
          final boolean altBand = (altDown && bandFilter instanceof Immobilized.UseAlt);
          if ((filter == null || altSelect) && ((bandFilter == null) || altBand)) { //BR// Band-select filtering support
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

    final JComponent view = map.getView();

    // left
    view.repaint(selection.x - ht,
                 selection.y - ht,
                 ht2,
                 selection.height + ht2);
    // right
    view.repaint(selection.x + selection.width - ht,
                 selection.y - ht,
                 ht2,
                 selection.height + ht2);
    // top
    view.repaint(selection.x - ht,
                 selection.y - ht,
                 selection.width + ht2,
                 ht2);
    // bottom
    view.repaint(selection.x - ht,
                 selection.y + selection.width - ht,
                 selection.width + ht2,
                 ht2);
  }

  /**
   * Sets the new location of the selection rectangle.
   */
  @Override
  public void mouseDragged(MouseEvent e) {
    if (selection == null) {
      return;
    }

    repaintSelectionRect();

    final int ex = e.getX();
    final int ey = e.getY();

    selection.x = Math.min(ex, anchor.x);
    selection.y = Math.min(ey, anchor.y);
    selection.width = Math.abs(ex - anchor.x);
    selection.height = Math.abs(ey - anchor.y);

    repaintSelectionRect();
  }

  @Override
  public void mouseMoved(MouseEvent e) {
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (selection == null) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    final Stroke str = g2d.getStroke();
    g2d.setStroke(new BasicStroke((float)(thickness * os_scale)));
    g2d.setColor(color);
    g2d.drawRect(
      (int)(selection.x * os_scale),
      (int)(selection.y * os_scale),
      (int)(selection.width * os_scale),
      (int)(selection.height * os_scale)
    );
    g2d.setStroke(str);
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }
}
