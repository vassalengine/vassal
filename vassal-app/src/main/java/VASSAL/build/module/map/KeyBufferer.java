/*
 *
 * Copyright (c) 2000-2020 by Rodney Kinney, Joel Uckelman, Brian Reynolds
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
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Immobilized;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.tools.swing.SwingUtils;

/**
 * Selects and unselects pieces on the map, using the mouse.
 * <br><br>
 * The KeyBufferer listens for mouse clicks and drags on its parent {@link Map}, determines whether the user is
 * performing a "lasso"/"band-select" or just clicking, draws the selection rectangle where appropriate, and
 * at the end of each relevant mouse action updates the {@link KeyBuffer} to maintain the list of which pieces are
 * currently "Selected" in the UI for the parent map.
 * <br><br>
 * Its rather misleading name derives from the fact that the selected units are kept in a {@link KeyBuffer},
 * with the idea being that if the player now presses a key, any resulting key command would be applied to all
 * the selected pieces, even though the actual tracking of keys getting pressed and key commands getting applied all
 * happens elsewhere, i.e. in the KeyBuffer class). A more appropriate name might have been e.g. "MouseSelection".
 * <br><br>
 * If the user clicks on a {@link GamePiece}, that piece is added to the {@link KeyBuffer}. {@link #draw(Graphics, Map)}
 * is responsible for drawing the mouse selection rectangle, and {@link #mouseDragged(MouseEvent)} is responsible
 * for triggering repaint events as the selection rectangle is moved.
 * <br><br>
 * If you are looking for the drag-and-drop handler for dragging pieces on or between maps,
 * instead see {@link PieceMover}.
 * <br><br>
 * @see Map#addLocalMouseListener
 */
public class KeyBufferer extends MouseAdapter implements Buildable, MouseMotionListener, Drawable {
  protected Map map;                            // Parent map
  protected Rectangle selection;                // Current lasso/band-select bounds.
  protected Point anchor;                       // Anchor point for band-select
  protected Color color = Color.black;          // Color for drawing band-select
  protected int thickness = 3;                  // Thickness for drawing band select
  protected GamePiece bandSelectPiece = null;   // If a band-select started within the boundaries of a piece, this is the piece

  /**
   * Band select modes
   */
  private enum BandSelectType {
    NONE,    // Band select NOT allowed on this mouse action because we started on a movable piece (so must be clicking it)
    NORMAL,  // Band select allowed on this mouse action
    SPECIAL  // We started on a non-movable piece, so we'll heuristically determine whether this is a click or a band-select
  }

  /**
   * Adds us to our parent map - we register as a mouse listener and a drawable component
   * @param b Parent map
   */
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

  /**
   * Process mouse-pressed events. Depending on what's present where the button became pressed (i.e. movable piece?
   * immovable piece? nothing?) we will determine whether this mouse event will be eligible to be a band-select
   * action.
   *
   * @param e Mouse Event
   */
  @Override
  public void mousePressed(MouseEvent e) {
    if (e.isConsumed()) {
      return;
    }

    // Get the list of currently selected pieces
    final KeyBuffer kbuf = KeyBuffer.getBuffer();

    // Find out if there's a GamePiece at the point where we're starting this
    // mouse event (And we don't want to clear the buffer until we find the
    // clicked-on piece because selecting a piece affects its visibility)
    GamePiece p = map.findPiece(e.getPoint(), PieceFinder.DECK_OR_PIECE_IN_STACK);

    EventFilter filter = null;
    BandSelectType bandSelect = BandSelectType.NONE; // Start by assuming we're clicking not band-selecting
    final boolean isDeck = (p != null) && (p instanceof Deck); // Make a note for later if we started on a Deck
    if (p != null) {
      // Unlike Stacks and regular pieces, Decks aren't selected/unselected so
      // apart from making the note, we don't treat them as a "piece" here
      if (isDeck) {
        p = null;
      }
      else {
        // Construct a filter that we will use to decide if the initial piece
        // is selectable by this action, according to any Does Not Stack
        // (VASSAL.counters.Immobilized) trait - some pieces aren't selectable
        // and others require e.g. the Alt key to be held down.
        filter = (EventFilter) p.getProperty(Properties.SELECT_EVENT_FILTER);

        // Don't "eat" band-selects if the piece we start the mouse event on
        // is non-movable
        if (SwingUtils.isMainMouseButtonDown(e) && Boolean.TRUE.equals(p.getProperty(Properties.NON_MOVABLE))) {
          bandSelect = BandSelectType.SPECIAL;
        }
      }
    }

    // Apply any filter to see if the initial piece rejects being selected
    final boolean ignoreEvent = filter != null && filter.rejectEvent(e);

    if (p != null && !ignoreEvent) {
      // We get here if initial piece clicked is eligible to be selected. In
      // the case of a movable piece that means we are definitely clicking
      // and/or dragging, not band-selecting. If it's a non-movable piece, we
      // do some preliminaries here, but are still open to the idea that we
      // might do a band-select.
      final boolean movingStacksPickupUnits = (Boolean) GameModule.getGameModule().getPrefs().getValue(Map.MOVING_STACKS_PICKUP_UNITS);

      if (!kbuf.contains(p)) {
        // If we get here, we've clicked on a selectable piece that's not yet
        // selected.

        // If neither SHIFT (to add units to current selection) nor
        // CTRL/Command (to toggle units to/from current selection)
        // is pressed, then we clear the selection entirely, in preparation to
        // add only this piece.
        if (!e.isShiftDown() && !SwingUtils.isSelectionToggle(e)) {
          kbuf.clear();
        }

        if (movingStacksPickupUnits ||
            p.getParent() == null ||
            p.getParent().isExpanded() ||
            SwingUtils.isSelectionToggle(e) ||
            SwingUtils.isContextMouseButtonDown(e) ||
            Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
          // If we're shift+clicking (add), or ctrl/command+clicking (toggle),
          // or if the stack has been "expanded" allowing individual members to
          // be clicked, or the piece isn't in a stack, add only the individual
          // unit to the selection.
          kbuf.add(p);
        }
        else {
          // If the top piece of an unexpanded stack is left-clicked
          // while not selected, then select all of the pieces in the stack
          final Stack s = p.getParent();
          s.asList().forEach(gamePiece -> KeyBuffer.getBuffer().add(gamePiece));
        }
      }
      else {
        // If we get here, we've clicked on a selectable piece that is already
        // selected

        // RFE 1659481 Ctrl/Command-click ("toggle") deselects clicked units
        // (if they are already selected)
        if (SwingUtils.isSelectionToggle(e) && Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
          final Stack s = p.getParent();
          if (s == null) {
            // Lone piece: simply remove
            kbuf.remove(p);
          }
          else if (!s.isExpanded()) {
            // Unexpanded stack: remove the whole stack
            s.asList().forEach(gamePiece -> KeyBuffer.getBuffer().remove(gamePiece));
          }
          else {
            // Expanded stack: remove just the clicked piece
            kbuf.remove(p);
          }
        }
        // End RFE 1659481
      }

      // Whatever piece or stack was clicked, move it to the front/top of its
      // current visual layer
      final GamePiece to_front = p.getParent() != null ? p.getParent() : p;
      map.getPieceCollection().moveToFront(to_front);
    }
    else {
      // If we got here, we did NOT start the mouse stroke on a selectable
      // piece. Thus we're probably band-selecting.
      bandSelect = BandSelectType.NORMAL; //BR// Allowed to band-select
    }

    // If we get here and are provisionally allowed to band-select, check if
    // we should get one going.
    if (bandSelect != BandSelectType.NONE) {
      bandSelectPiece = null;
      if (!e.isShiftDown() && !SwingUtils.isSelectionToggle(e)) { // No deselect if shift key down
        // If we're starting a new band-select, we clear the present selection
        // if we're not doing a Shift+Lasso to add units to present selection
        // or a Ctrl/Command+Lasso to toggle.
        kbuf.clear();

        //BR// This section allows band-select to be attempted from non-moving
        // pieces w/o preventing click-to-select from working
        if (bandSelect == BandSelectType.SPECIAL && p != null && !ignoreEvent) {
          kbuf.add(p);
          bandSelectPiece = p; // We mark the relevant piece so that we can resolve its status at the end of the mouse stroke
        }
      }
      if (!isDeck) { //BR// If started on a deck, don't also do a band select (because we're busy dragging a piece off the top of the deck)

        // This initializes the band-select "lasso" visuals
        anchor = map.mapToComponent(e.getPoint()); // Our anchor point

        // Sets the initial selection box (and the fact that selection is
        // non-null is how we know a band-select is ongoing)
        selection = new Rectangle(anchor.x, anchor.y, 0, 0);

        if (map.getHighlighter() instanceof ColoredBorder) {
          final ColoredBorder b = (ColoredBorder) map.getHighlighter();
          color = b.getColor();
          thickness = b.getThickness();
        }
      }
    }
  }

  /**
   * When the mouse button is released while doing a band-select, we handle updating the selection based on
   * the final size of the selection box.
   *
   * @param e Mouse event at point of release
   */
  @Override
  public void mouseReleased(MouseEvent e) {
    // If selection is null, no band-select is happening, so nothing to do here -- we already did everything on the initial click
    if (selection == null) {
      return;
    }

    // Creates a dispatcher that will be called for each piece on the map, and which will filter them
    // based on being in or out of the "selection" rectangle, and which modifier keys are pressed. The
    // dispatcher's visitDefault, visitStack, and visitDeck methods, below, will be used to resolve the
    // selection status of each piece on the map -- but not until map.apply(d) is called toward the end
    // of this method.
    final PieceVisitorDispatcher d = createDragSelector(
      !SwingUtils.isSelectionToggle(e), e.isAltDown(), map.componentToMap(selection)
    );

    // If it was a legit band-select drag (not just a click), our special case
    // only applies if piece is allowed to be band-selected
    if (bandSelectPiece != null) {
      final EventFilter bandFilter = (EventFilter) bandSelectPiece.getProperty(Properties.BAND_SELECT_EVENT_FILTER);
      final boolean pieceAllowedToBeBandSelected = bandFilter != null && !e.isAltDown() && bandFilter instanceof Immobilized.UseAlt;

      if (pieceAllowedToBeBandSelected) {
        final Point finish = map.mapToComponent(e.getPoint());
        // Open to suggestions about a better way to distinguish "click" from
        // "lasso" (not that Vassal doesn't already suck immensely at
        // click-vs-drag threshold). FWIW, this "works".
        final boolean isLasso = finish.distance(anchor) >= 10;
        if (isLasso) {
          bandSelectPiece = null;
        }
      }
    }

    // On a normal band select we will start by clearing the current selection. Exceptions are:
    // 1. We started the band-select on a special immovable (but selectable) piece that we will now retain.
    // 2. SHIFT key is down (we're adding lassoed pieces to current selection)
    // 3. CTRL / Command key is down (we're toggling lassoed pieces to/from the current selection)
    if (bandSelectPiece == null && !e.isShiftDown() && !SwingUtils.isSelectionToggle(e)) {
      KeyBuffer.getBuffer().clear();
    }

    // This applies the PieceVisitorDispatcher above to every piece on the map. See the
    // visitDefault, visitStack, and visitDeck methods below, which resolve the status of
    // each piece, stack, and deck found within the selection bounds.
    map.apply(d);

    // Initiate a repaint on our selection rectangle (when it resolves, it will remove the rectangle from the UI)
    repaintSelectionRect();

    // Clears our selection rectangle - this also tells us a band-select is no longer in progress.
    selection = null;
  }

  /**
   * This PieceVisitorDispatcher determines what to do with pieces on the
   * map when the player finished dragging a rectangle to select pieces.
   *
   * @return the "DeckVisitor" used to process each piece.
   */
  protected PieceVisitorDispatcher createDragSelector(
    boolean selecting,
    boolean altDown,
    Rectangle mapsel) {

    return new PieceVisitorDispatcher(
      new KBDeckVisitor(selecting, altDown, mapsel)
    );
  }

  /**
   * This "Visitor" dispatcher receives a method call for EVERY piece on the map (one by one), and must filter the
   * pieces based on their presence in the appropriate selection bounds and take the correct action vis-a-vis that
   * piece's selection status.
   */
  public class KBDeckVisitor implements DeckVisitor {
    boolean selecting;
    boolean altDown;
    Rectangle mapsel;

    public KBDeckVisitor(boolean b, boolean c, Rectangle ms) {
      selecting = b;
      altDown = c;
      mapsel = ms;
    }

    /**
     * Processes decks from the map. Decks themselves are never band-selected
     * @param d a deck
     * @return always null (tells map's accept to keep processing all pieces)
     */
    @Override
    public Object visitDeck(Deck d) {
      return null;
    }

    /**
     * Processes stacks from the map
     * @param s a stack
     * @return always null (tells map's accept to keep processing all pieces)
     */
    @Override
    public Object visitStack(Stack s) {
      if (s.topPiece() != null) {  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
        final KeyBuffer kbuf = KeyBuffer.getBuffer();
        if (s instanceof Deck) {
          s.asList().forEach(kbuf::remove); // Clear any deck *members* out of the KeyBuffer.
          return null;
        }

        // If stack is expanded, we check each individual piece against the selection boundaries
        if (s.isExpanded()) {
          final Point[] pos = new Point[s.getPieceCount()];
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
        // Else (unexpanded stack) we deal with the whole stack the same way
        else if (mapsel.contains(s.getPosition())) {
          s.asList().forEach(selecting ? kbuf::add : kbuf::remove);
        }
      }
      return null;
    }

    /**
     * Handles non-stacked units, including Does Not Stack units.
     * Does Not Stack units deselect normally once selected.
     * @param p a piece
     * @return always null (tells map's accept to keep processing all pieces)
     */
    @Override
    public Object visitDefault(GamePiece p) {
      final Stack s = p.getParent();
      if (s instanceof Deck) {
        // Clear any deck *members* out of the KeyBuffer. (yes, members of decks can be does-not-stack)
        // This prevents us from accidentally "invisibly selecting" the entire contents of a Deck, which
        // in days of yore led to Much Sadness.
        KeyBuffer.getBuffer().remove(p);
        return null;
      }

      // Does-not-stack pieces have their own special band-select filters. We also ignore pieces we can't see.
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

  /**
   * Initiates a repaint on the selection rectangle
   */
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
    final int ht2 = 2 * ht;

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
   * If the mouse has dragged WHILE a band-select is in progress, sets the new location of the selection rectangle.
   */
  @Override
  public void mouseDragged(MouseEvent e) {
    // If no band-select is marked ongoing, OR if somehow the main mouse button is no longer pressed, nothing to do here.
    if (selection == null || !SwingUtils.isMainMouseButtonDown(e)) {
      return;
    }

    // Initiate a repaint on the former selection box size (to make sure it is removed from the screen)
    repaintSelectionRect();

    final int ex = e.getX();
    final int ey = e.getY();

    // Adjust the size of the selection box based on the anchor point and our new location.
    selection.x = Math.min(ex, anchor.x);
    selection.y = Math.min(ey, anchor.y);
    selection.width = Math.abs(ex - anchor.x);
    selection.height = Math.abs(ey - anchor.y);

    // Now initiate a repaint on the new selection box size, which will cause it to be displayed.
    repaintSelectionRect();
  }

  @Override
  public void mouseMoved(MouseEvent e) {
  }

  /**
   * Draws our current selection box
   * @param g graphics object
   * @param map map
   */
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

  /**
   * Selection box is always drawn "above counters'
   * @return true
   */
  @Override
  public boolean drawAboveCounters() {
    return true;
  }
}
