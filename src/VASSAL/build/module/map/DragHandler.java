/*
 * $Id$
 *
 * Copyright (c) 2000-2011 by Rodney Kinney, Jim Urbas, Pieter Geerkens
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

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.datatransfer.StringSelection;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DragSourceMotionListener;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;

import org.apache.commons.lang.SystemUtils;

import VASSAL.build.module.Map;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Highlighter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.tools.image.ImageUtils;

/** Common functionality for DragHandler for cases with and without
 * drag image support. <p>
 * NOTE: DragSource.isDragImageSupported() returns false for j2sdk1.4.2_02 on
 * Windows 2000
 *
 * @author Pieter Geerkens
 */
public abstract class DragHandler
  implements DragGestureListener,       DragSourceListener,
             DragSourceMotionListener,  DropTargetListener
{
  static private DragHandler theDragHandler =
      DragSource.isDragImageSupported() ?
      (SystemUtils.IS_OS_MAC_OSX ?
        new DragHandlerImageMacOSX() : new DragHandlerImage()) :
        new DragHandlerNoImage();

  /** returns the singleton DragHandler instance */
  static public DragHandler getTheDragHandler() {
    return theDragHandler;
  }

  static public void setTheDragHandler(DragHandler myHandler) {
    theDragHandler = myHandler;
  }

  final static int CURSOR_ALPHA = 127; // psuedo cursor is 50% transparent
  final static int EXTRA_BORDER = 4; // psuedo cursor is includes a 4 pixel border


  protected JLabel dragCursor; // An image label. Lives on current DropTarget's
    // LayeredPane.
//      private BufferedImage dragImage; // An image label. Lives on current DropTarget's LayeredPane.
  private Point drawOffset = new Point(); // translates event coords to local
                                          // drawing coords
  private Rectangle boundingBox; // image bounds
  private int originalPieceOffsetX; // How far drag STARTED from gamepiece's
                                    // center
  private int originalPieceOffsetY; // I.e. on original map
  protected double dragPieceOffCenterZoom = 1.0; // zoom at start of drag
  private int currentPieceOffsetX; // How far cursor is CURRENTLY off-center,
                                   // a function of
                                   // dragPieceOffCenter{X,Y,Zoom}
  private int currentPieceOffsetY; // I.e. on current map (which may have
                                   // different zoom
  protected double dragCursorZoom = 1.0; // Current cursor scale (zoom)
  Component dragWin; // the component that initiated the drag operation
  Component dropWin; // the drop target the mouse is currently over
  JLayeredPane drawWin; // the component that owns our psuedo-cursor
  // Seems there can be only one DropTargetListener a drop target. After we
  // process a drop target
  // event, we manually pass the event on to this listener.
  java.util.Map<Component,DropTargetListener> dropTargetListeners =
    new HashMap<Component,DropTargetListener>();

  abstract protected int getOffsetMult();

  /**
   * Creates a new DropTarget and hooks us into the beginning of a
   * DropTargetListener chain. DropTarget events are not multicast;
   * there can be only one "true" listener.
   */
  static public DropTarget makeDropTarget(Component theComponent, int dndContants, DropTargetListener dropTargetListener) {
    if (dropTargetListener != null) {
      DragHandler.getTheDragHandler()
                 .dropTargetListeners.put(theComponent, dropTargetListener);
    }
    return new DropTarget(theComponent, dndContants,
                          DragHandler.getTheDragHandler());
  }

  static public void removeDropTarget(Component theComponent) {
    DragHandler.getTheDragHandler().dropTargetListeners.remove(theComponent);
  }

  protected DropTargetListener getListener(DropTargetEvent e) {
    final Component component = e.getDropTargetContext().getComponent();
    return dropTargetListeners.get(component);
  }

  /** Moves the drag cursor on the current draw window */
  protected void moveDragCursor(int dragX, int dragY) {
    if (drawWin != null) {
      dragCursor.setLocation(dragX - drawOffset.x, dragY - drawOffset.y);
    }
  }

  /** Removes the drag cursor from the current draw window */
  protected void removeDragCursor() {
    if (drawWin != null) {
      if (dragCursor != null) {
        dragCursor.setVisible(false);
        drawWin.remove(dragCursor);
      }
      drawWin = null;
    }
  }

  /** calculates the offset between cursor dragCursor positions */
  private void calcDrawOffset() {
    if (drawWin != null) {
      // drawOffset is the offset between the mouse location during a drag
      // and the upper-left corner of the cursor
      // accounts for difference between event point (screen coords)
      // and Layered Pane position, boundingBox and off-center drag
      drawOffset.x = -boundingBox.x - currentPieceOffsetX + EXTRA_BORDER;
      drawOffset.y = -boundingBox.y - currentPieceOffsetY + EXTRA_BORDER;
      SwingUtilities.convertPointToScreen(drawOffset, drawWin);
    }
  }

  /**
   * creates or moves cursor object to given JLayeredPane. Usually called by setDrawWinToOwnerOf()
   */
  private void setDrawWin(JLayeredPane newDrawWin) {
    if (newDrawWin != drawWin) {
      // remove cursor from old window
      if (dragCursor.getParent() != null) {
        dragCursor.getParent().remove(dragCursor);
      }
      if (drawWin != null) {
        drawWin.repaint(dragCursor.getBounds());
      }
      drawWin = newDrawWin;
      calcDrawOffset();
      dragCursor.setVisible(false);
      drawWin.add(dragCursor, JLayeredPane.DRAG_LAYER);
    }
  }

  /**
   * creates or moves cursor object to given window. Called when drag operation begins in a window or the cursor is
   * dragged over a new drop-target window
   */
  public void setDrawWinToOwnerOf(Component newDropWin) {
    if (newDropWin != null) {
      final JRootPane rootWin = SwingUtilities.getRootPane(newDropWin);
      if (rootWin != null) {
        setDrawWin(rootWin.getLayeredPane());
      }
    }
  }

  /** Common functionality abstracted from makeDragImage and makeDragCursor
   *
   * @param zoom
   * @param doOffset
   * @param target
   * @param setSize
   * @return
   */
  BufferedImage makeDragImageCursorCommon(double zoom, boolean doOffset,
    Component target, boolean setSize)
  {
    // FIXME: Should be an ImageOp.
    dragCursorZoom = zoom;

    final List<Point> relativePositions = buildBoundingBox(zoom, doOffset);

    final int w = boundingBox.width + EXTRA_BORDER * 2;
    final int h = boundingBox.height + EXTRA_BORDER * 2;

    BufferedImage image = ImageUtils.createCompatibleTranslucentImage(w, h);

    drawDragImage(image, target, relativePositions, zoom);

    if (setSize) dragCursor.setSize(w, h);
    image = featherDragImage(image, w, h, EXTRA_BORDER);

    return image;
  }

  /**
   * Creates the image to use when dragging based on the zoom factor
   * passed in.
   *
   * @param zoom DragBuffer.getBuffer
   * @return dragImage
   */
  private BufferedImage makeDragImage(double zoom) {
    return makeDragImageCursorCommon(zoom, false, null, false);
  }

  /**
   * Installs the cursor image into our dragCursor JLabel.
   * Sets current zoom. Should be called at beginning of drag
   * and whenever zoom changes. INPUT: DragBuffer.getBuffer OUTPUT:
   * dragCursorZoom cursorOffCenterX cursorOffCenterY boundingBox
   * @param zoom DragBuffer.getBuffer
   *
   */
  protected void makeDragCursor(double zoom) {
    // create the cursor if necessary
    if (dragCursor == null) {
      dragCursor = new JLabel();
      dragCursor.setVisible(false);
    }
    dragCursor.setIcon(new ImageIcon(
        makeDragImageCursorCommon(zoom, true, dragCursor, true)));
  }

  private List<Point> buildBoundingBox(double zoom, boolean doOffset) {
    final ArrayList<Point> relativePositions = new ArrayList<Point>();
    final PieceIterator dragContents = DragBuffer.getBuffer().getIterator();
    final GamePiece firstPiece = dragContents.nextPiece();
    GamePiece lastPiece = firstPiece;

    currentPieceOffsetX =
      (int) (originalPieceOffsetX / dragPieceOffCenterZoom * zoom + 0.5);
    currentPieceOffsetY =
      (int) (originalPieceOffsetY / dragPieceOffCenterZoom * zoom + 0.5);

    boundingBox = firstPiece.getShape().getBounds();
    boundingBox.width *= zoom;
    boundingBox.height *= zoom;
    boundingBox.x *= zoom;
    boundingBox.y *= zoom;
    if (doOffset) {
      calcDrawOffset();
    }

    relativePositions.add(new Point(0,0));
    int stackCount = 0;
    while (dragContents.hasMoreElements()) {
      final GamePiece nextPiece = dragContents.nextPiece();
      final Rectangle r = nextPiece.getShape().getBounds();
      r.width *= zoom;
      r.height *= zoom;
      r.x *= zoom;
      r.y *= zoom;

      final Point p = new Point(
        (int) Math.round(
          zoom * (nextPiece.getPosition().x - firstPiece.getPosition().x)),
        (int) Math.round(
          zoom * (nextPiece.getPosition().y - firstPiece.getPosition().y)));
      r.translate(p.x, p.y);

      if (nextPiece.getPosition().equals(lastPiece.getPosition())) {
        stackCount++;
        final StackMetrics sm = getStackMetrics(nextPiece);
        r.translate(sm.unexSepX*stackCount,-sm.unexSepY*stackCount);
      }

      boundingBox.add(r);
      relativePositions.add(p);
      lastPiece = nextPiece;
    }
    return relativePositions;
  }

  private void drawDragImage(BufferedImage image, Component target,
                             List<Point> relativePositions, double zoom) {
    final Graphics2D g = image.createGraphics();

    int index = 0;
    Point lastPos = null;
    int stackCount = 0;
    for (PieceIterator dragContents = DragBuffer.getBuffer().getIterator();
         dragContents.hasMoreElements(); ) {

      final GamePiece piece = dragContents.nextPiece();
      final Point pos = relativePositions.get(index++);
      final Map map = piece.getMap();

      if (piece instanceof Stack){
        stackCount = 0;
        piece.draw(g, EXTRA_BORDER - boundingBox.x + pos.x,
                      EXTRA_BORDER - boundingBox.y + pos.y,
                      map == null ? target : map.getView(), zoom);
      }
      else {
        final Point offset = new Point(0,0);
        if (pos.equals(lastPos)) {
          stackCount++;
          final StackMetrics sm = getStackMetrics(piece);
          offset.x = sm.unexSepX * stackCount;
          offset.y = sm.unexSepY * stackCount;
        }
        else {
          stackCount = 0;
        }

        final int x = EXTRA_BORDER - boundingBox.x + pos.x + offset.x;
        final int y = EXTRA_BORDER - boundingBox.y + pos.y - offset.y;
        piece.draw(g, x, y, map == null ? target : map.getView(), zoom);

        final Highlighter highlighter = map == null ?
          BasicPiece.getHighlighter() : map.getHighlighter();
        highlighter.draw(piece, g, x, y, null, zoom);
      }

      lastPos = pos;
    }

    g.dispose();
  }

  private StackMetrics getStackMetrics(GamePiece piece) {
    StackMetrics sm = null;
    final Map map = piece.getMap();
    if (map != null) {
      sm = map.getStackMetrics();
    }
    if (sm == null) {
      sm = new StackMetrics();
    }
    return sm;
  }

  private BufferedImage featherDragImage(BufferedImage src,
                                         int w, int h, int b) {
// FIXME: This should be redone so that we draw the feathering onto the
// destination first, and then pass the Graphics2D on to draw the pieces
// directly over it. Presently this doesn't work because some of the
// pieces screw up the Graphics2D when passed it... The advantage to doing
// it this way is that we create only one BufferedImage instead of two.
    final BufferedImage dst =
      ImageUtils.createCompatibleTranslucentImage(w, h);

    final Graphics2D g = dst.createGraphics();
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                       RenderingHints.VALUE_ANTIALIAS_ON);

    // paint the rectangle occupied by the piece at specified alpha
    g.setColor(new Color(0xff, 0xff, 0xff, CURSOR_ALPHA));
    g.fillRect(0, 0, w, h);

    // feather outwards
    for (int f = 0; f < b; ++f) {
      final int alpha = CURSOR_ALPHA * (f + 1) / b;
      g.setColor(new Color(0xff, 0xff, 0xff, alpha));
      g.drawRect(f, f, w-2*f, h-2*f);
    }

    // paint in the source image
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_IN));
    g.drawImage(src, 0, 0, null);
    g.dispose();

    return dst;
  }

  ///////////////////////////////////////////////////////////////////////////
  // DRAG GESTURE LISTENER INTERFACE
  //
  // EVENT uses SCALED, DRAG-SOURCE coordinate system.
  // PIECE uses SCALED, OWNER (arbitrary) coordinate system
  //
  ///////////////////////////////////////////////////////////////////////////
  /** Fires after user begins moving the mouse several pixels over a map. */
  public void dragGestureRecognized(DragGestureEvent dge) {
    try {
      beginDragging(dge);
    }
    // FIXME: Fix by replacing AWT Drag 'n Drop with Swing DnD.
    // Catch and ignore spurious DragGestures
    catch (InvalidDnDOperationException e) {
    }
  }

  protected Point dragGestureRecognizedPrep(DragGestureEvent dge) {
    // Ensure the user has dragged on a counter before starting the drag.
    final DragBuffer db = DragBuffer.getBuffer();
    if (db.isEmpty()) return null;

    // Remove any Immovable pieces from the DragBuffer that were
    // selected in a selection rectangle, unless they are being
    // dragged from a piece palette (i.e., getMap() == null).
    final List<GamePiece> pieces = new ArrayList<GamePiece>();
    for (PieceIterator i = db.getIterator();
         i.hasMoreElements(); pieces.add(i.nextPiece()));
    for (GamePiece piece : pieces) {
      if (piece.getMap() != null &&
          Boolean.TRUE.equals(piece.getProperty(Properties.NON_MOVABLE))) {
        db.remove(piece);
      }
    }

    // Bail out if this leaves no pieces to drag.
    if (db.isEmpty()) return null;

    final GamePiece piece = db.getIterator().nextPiece();

    final Map map = dge.getComponent() instanceof Map.View ?
                    ((Map.View) dge.getComponent()).getMap() : null;

    final Point mousePosition = (map == null)
                      ? dge.getDragOrigin()
                      : map.componentCoordinates(dge.getDragOrigin());
    Point piecePosition = (map == null)
                  ?  piece.getPosition()
                  : map.componentCoordinates(piece.getPosition());
    // If DragBuffer holds a piece with invalid coordinates (for example, a
    // card drawn from a deck), drag from center of piece
    if (piecePosition.x <= 0 || piecePosition.y <= 0) {
      piecePosition = mousePosition;
    }
    // Account for offset of piece within stack
    // We do this even for un-expanded stacks, since the offset can
    // still be significant if the stack is large
    dragPieceOffCenterZoom = map == null ? 1.0 : map.getZoom();
    if (piece.getParent() != null && map != null) {
      final Point offset = piece.getParent()
                                .getStackMetrics()
                                .relativePosition(piece.getParent(), piece);
      piecePosition.translate(
        (int) Math.round(offset.x * dragPieceOffCenterZoom),
        (int) Math.round(offset.y * dragPieceOffCenterZoom));
    }

    // dragging from UL results in positive offsets
    originalPieceOffsetX = piecePosition.x - mousePosition.x;
    originalPieceOffsetY = piecePosition.y - mousePosition.y;
    dragWin = dge.getComponent();
    drawWin = null;
    dropWin = null;
    return mousePosition;
  }

  protected void beginDragging(DragGestureEvent dge) {
    // this call is needed to instantiate the boundingBox object
    final BufferedImage bImage = makeDragImage(dragPieceOffCenterZoom);

    final Point dragPointOffset = new Point(
      getOffsetMult() * (boundingBox.x + currentPieceOffsetX - EXTRA_BORDER),
      getOffsetMult() * (boundingBox.y + currentPieceOffsetY - EXTRA_BORDER)
    );

    dge.startDrag(
      Cursor.getPredefinedCursor(Cursor.HAND_CURSOR),
      bImage,
      dragPointOffset,
      new StringSelection(""),
      this
    );

    dge.getDragSource().addDragSourceMotionListener(this);
  }

  ///////////////////////////////////////////////////////////////////////////
  // DRAG SOURCE LISTENER INTERFACE
  //
  ///////////////////////////////////////////////////////////////////////////
  public void dragDropEnd(DragSourceDropEvent e) {
    final DragSource ds = e.getDragSourceContext().getDragSource();
    ds.removeDragSourceMotionListener(this);
  }

  public void dragEnter(DragSourceDragEvent e) {}

  public void dragExit(DragSourceEvent e) {}

  public void dragOver(DragSourceDragEvent e) {}

  public void dropActionChanged(DragSourceDragEvent e) {}

  ///////////////////////////////////////////////////////////////////////////
  // DRAG SOURCE MOTION LISTENER INTERFACE
  //
  // EVENT uses UNSCALED, SCREEN coordinate system
  //
  ///////////////////////////////////////////////////////////////////////////
  // Used to check for real mouse movement.
  // Warning: dragMouseMoved fires 8 times for each point on development
  // system (Win2k)
  protected Point lastDragLocation = new Point();

  /** Moves cursor after mouse */
  abstract public void dragMouseMoved(DragSourceDragEvent e);

  ///////////////////////////////////////////////////////////////////////////
  // DROP TARGET INTERFACE
  //
  // EVENT uses UNSCALED, DROP-TARGET coordinate system
  ///////////////////////////////////////////////////////////////////////////
  /** switches current drawWin when mouse enters a new DropTarget */
  public void dragEnter(DropTargetDragEvent e) {
    final DropTargetListener forward = getListener(e);
    if (forward != null) forward.dragEnter(e);
  }

  /**
   * Last event of the drop operation. We adjust the drop point for
   * off-center drag, remove the cursor, and pass the event along
   * listener chain.
   */
  public void drop(DropTargetDropEvent e) {
    // EVENT uses UNSCALED, DROP-TARGET coordinate system
    e.getLocation().translate(currentPieceOffsetX, currentPieceOffsetY);
    final DropTargetListener forward = getListener(e);
    if (forward != null) forward.drop(e);
  }

  /** ineffectual. Passes event along listener chain */
  public void dragExit(DropTargetEvent e) {
    final DropTargetListener forward = getListener(e);
    if (forward != null) forward.dragExit(e);
  }

  /** ineffectual. Passes event along listener chain */
  public void dragOver(DropTargetDragEvent e) {
    final DropTargetListener forward = getListener(e);
    if (forward != null) forward.dragOver(e);
  }

  /** ineffectual. Passes event along listener chain */
  public void dropActionChanged(DropTargetDragEvent e) {
    final DropTargetListener forward = getListener(e);
    if (forward != null) forward.dropActionChanged(e);
  }
}
