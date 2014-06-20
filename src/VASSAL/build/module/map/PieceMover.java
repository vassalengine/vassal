/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney, Jim Urbas
 * Refactoring of DragHandler Copyright 2011 Pieter Geerkens
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

import java.awt.Component;
import java.awt.Image;
import java.awt.Point;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.Decorator;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.PieceSorter;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.imageop.Op;

/**
 * This is a MouseListener that moves pieces onto a Map window
 */
public class PieceMover extends AbstractBuildable
                        implements MouseListener,
                                   GameComponent,
                                   Comparator<GamePiece> {
  /** The Preferences key for autoreporting moves. */
  public static final String AUTO_REPORT = "autoReport"; //$NON-NLS-1$
  public static final String NAME = "name";

  public static final String HOTKEY = "hotkey";

  protected Map map;
  protected Point dragBegin;
  protected GamePiece dragging;
  protected LaunchButton markUnmovedButton;
  protected String markUnmovedText;
  protected String markUnmovedIcon;
  public static final String ICON_NAME = "icon"; //$NON-NLS-1$
  protected String iconName;

  // Selects drag target from mouse click on the Map
  protected PieceFinder dragTargetSelector;

  // Selects piece to merge with at the drop destination
  protected PieceFinder dropTargetSelector;

  // Processes drag target  after having been selected
  protected PieceVisitorDispatcher selectionProcessor;

  protected Comparator<GamePiece> pieceSorter = new PieceSorter();

  public void addTo(Buildable b) {
    dragTargetSelector = createDragTargetSelector();
    dropTargetSelector = createDropTargetSelector();
    selectionProcessor = createSelectionProcessor();
    map = (Map) b;
    map.addLocalMouseListener(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    map.setDragGestureListener(DragHandler.getTheDragHandler());
    map.setPieceMover(this);
    setAttribute(Map.MARK_UNMOVED_TEXT,
                 map.getAttributeValueString(Map.MARK_UNMOVED_TEXT));
    setAttribute(Map.MARK_UNMOVED_ICON,
                 map.getAttributeValueString(Map.MARK_UNMOVED_ICON));
  }

  protected MovementReporter createMovementReporter(Command c) {
    return new MovementReporter(c);
  }

  /**
   * When the user completes a drag-drop operation, the pieces being
   * dragged will either be combined with an existing piece on the map
   * or else placed on the map without stack. This method returns a
   * {@link PieceFinder} instance that determines which {@link GamePiece}
   * (if any) to combine the being-dragged pieces with.
   *
   * @return
   */
  protected PieceFinder createDropTargetSelector() {
    return new PieceFinder.Movable() {
      public Object visitDeck(Deck d) {
        final Point pos = d.getPosition();
        final Point p = new Point(pt.x - pos.x, pt.y - pos.y);
        if (d.getShape().contains(p)) {
          return d;
        }
        else {
          return null;
        }
      }

      public Object visitDefault(GamePiece piece) {
        GamePiece selected = null;
        if (this.map.getStackMetrics().isStackingEnabled() &&
            this.map.getPieceCollection().canMerge(dragging, piece)) {
          if (this.map.isLocationRestricted(pt)) {
            final Point snap = this.map.snapTo(pt);
            if (piece.getPosition().equals(snap)) {
              selected = piece;
            }
          }
          else {
            selected = (GamePiece) super.visitDefault(piece);
          }
        }

        if (selected != null &&
            DragBuffer.getBuffer().contains(selected) &&
            selected.getParent() != null &&
            selected.getParent().topPiece() == selected) {
          selected = null;
        }
        return selected;
      }

      public Object visitStack(Stack s) {
        GamePiece selected = null;
        if (this.map.getStackMetrics().isStackingEnabled() &&
            this.map.getPieceCollection().canMerge(dragging, s) &&
            !DragBuffer.getBuffer().contains(s) &&
            s.topPiece() != null) {
          if (this.map.isLocationRestricted(pt) && !s.isExpanded()) {
            if (s.getPosition().equals(this.map.snapTo(pt))) {
              selected = s;
            }
          }
          else {
            selected = (GamePiece) super.visitStack(s);
          }
        }
        return selected;
      }
    };
  }

  /**
   * When the user clicks on the map, a piece from the map is selected by
   * the dragTargetSelector. What happens to that piece is determined by
   * the {@link PieceVisitorDispatcher} instance returned by this method.
   * The default implementation does the following: If a Deck, add the top
   * piece to the drag buffer If a stack, add it to the drag buffer.
   * Otherwise, add the piece and any other multi-selected pieces to the
   * drag buffer.
   *
   * @see #createDragTargetSelector
   * @return
   */
  protected PieceVisitorDispatcher createSelectionProcessor() {
    return new DeckVisitorDispatcher(new DeckVisitor() {
      public Object visitDeck(Deck d) {
        DragBuffer.getBuffer().clear();
        for (PieceIterator it = d.drawCards(); it.hasMoreElements();) {
          DragBuffer.getBuffer().add(it.nextPiece());
        }
        return null;
      }

      public Object visitStack(Stack s) {
        DragBuffer.getBuffer().clear();
        // RFE 1629255 - Only add selected pieces within the stack to the DragBuffer
        // Add whole stack if all pieces are selected - better drag cursor
        int selectedCount = 0;
        for (int i = 0; i < s.getPieceCount(); i++) {
          if (Boolean.TRUE.equals(s.getPieceAt(i)
                                   .getProperty(Properties.SELECTED))) {
            selectedCount++;
          }
        }

        if (((Boolean) GameModule.getGameModule().getPrefs().getValue(Map.MOVING_STACKS_PICKUP_UNITS)).booleanValue() || s.getPieceCount() == 1 || s.getPieceCount() == selectedCount) {
          DragBuffer.getBuffer().add(s);
        }
        else {
          for (int i = 0; i < s.getPieceCount(); i++) {
            final GamePiece p = s.getPieceAt(i);
            if (Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
              DragBuffer.getBuffer().add(p);
            }
          }
        }
        // End RFE 1629255
        if (KeyBuffer.getBuffer().containsChild(s)) {
          // If clicking on a stack with a selected piece, put all selected
          // pieces in other stacks into the drag buffer
          KeyBuffer.getBuffer().sort(PieceMover.this);
          for (Iterator<GamePiece> i =
                KeyBuffer.getBuffer().getPiecesIterator(); i.hasNext();) {
            final GamePiece piece = i.next();
            if (piece.getParent() != s) {
              DragBuffer.getBuffer().add(piece);
            }
          }
        }
        return null;
      }

      public Object visitDefault(GamePiece selected) {
        DragBuffer.getBuffer().clear();
        if (KeyBuffer.getBuffer().contains(selected)) {
          // If clicking on a selected piece, put all selected pieces into the
          // drag buffer
          KeyBuffer.getBuffer().sort(PieceMover.this);
          for (Iterator<GamePiece> i =
                KeyBuffer.getBuffer().getPiecesIterator(); i.hasNext();) {
            DragBuffer.getBuffer().add(i.next());
          }
        }
        else {
          DragBuffer.getBuffer().clear();
          DragBuffer.getBuffer().add(selected);
        }
        return null;
      }
    });
  }

  /**
   * Returns the {@link PieceFinder} instance that will select a
   * {@link GamePiece} for processing when the user clicks on the map.
   * The default implementation is to return the first piece whose shape
   * contains the point clicked on.
   *
   * @return
   */
  protected PieceFinder createDragTargetSelector() {
    return new PieceFinder.Movable() {
      public Object visitDeck(Deck d) {
        final Point pos = d.getPosition();
        final Point p = new Point(pt.x - pos.x, pt.y - pos.y);
        if (d.boundingBox().contains(p) && d.getPieceCount() > 0) {
          return d;
        }
        else {
          return null;
        }
      }
    };
  }

  public void setup(boolean gameStarting) {
    if (gameStarting) {
      initButton();
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  private Image loadIcon(String name) {
    if (name == null || name.length() == 0) return null;
    return Op.load(name).getImage();
  }

  protected void initButton() {
    final String value = getMarkOption();
    if (GlobalOptions.PROMPT.equals(value)) {
      BooleanConfigurer config = new BooleanConfigurer(
        Map.MARK_MOVED, "Mark Moved Pieces", Boolean.TRUE);
      GameModule.getGameModule().getPrefs().addOption(config);
    }

    if (!GlobalOptions.NEVER.equals(value)) {
      if (markUnmovedButton == null) {
        final ActionListener al = new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            final GamePiece[] p = map.getAllPieces();
            final Command c = new NullCommand();
            for (int i = 0; i < p.length; ++i) {
              c.append(markMoved(p[i], false));
            }
            GameModule.getGameModule().sendAndLog(c);
            map.repaint();
          }
        };

        markUnmovedButton =
          new LaunchButton("", NAME, HOTKEY, Map.MARK_UNMOVED_ICON, al);

        Image img = null;
        if (iconName != null && iconName.length() > 0) {
          img = loadIcon(iconName);
          if (img != null) {
            markUnmovedButton.setAttribute(Map.MARK_UNMOVED_ICON, iconName);
          }
        }

        if (img == null) {
          img = loadIcon(markUnmovedIcon);
          if (img != null) {
            markUnmovedButton.setAttribute(Map.MARK_UNMOVED_ICON, markUnmovedIcon);
          }
        }

        markUnmovedButton.setAlignmentY(0.0F);
        markUnmovedButton.setText(markUnmovedText);
        markUnmovedButton.setToolTipText(
          map.getAttributeValueString(Map.MARK_UNMOVED_TOOLTIP));
        map.getToolBar().add(markUnmovedButton);
      }
    }
    else if (markUnmovedButton != null) {
      map.getToolBar().remove(markUnmovedButton);
      markUnmovedButton = null;
    }
  }

  private String getMarkOption() {
    String value = map.getAttributeValueString(Map.MARK_MOVED);
    if (value == null) {
      value = GlobalOptions.getInstance()
                           .getAttributeValueString(GlobalOptions.MARK_MOVED);
    }
    return value;
  }

  public String[] getAttributeNames() {
    return new String[]{ICON_NAME};
  }

  public String getAttributeValueString(String key) {
    return ICON_NAME.equals(key) ? iconName : null;
  }

  public void setAttribute(String key, Object value) {
    if (ICON_NAME.equals(key)) {
      iconName = (String) value;
    }
    else if (Map.MARK_UNMOVED_TEXT.equals(key)) {
      if (markUnmovedButton != null) {
        markUnmovedButton.setAttribute(NAME, value);
      }
      markUnmovedText = (String) value;
    }
    else if (Map.MARK_UNMOVED_ICON.equals(key)) {
      if (markUnmovedButton != null) {
        markUnmovedButton.setAttribute(Map.MARK_UNMOVED_ICON, value);
      }
      markUnmovedIcon = (String) value;
    }
  }

  protected boolean isMultipleSelectionEvent(MouseEvent e) {
    return e.isShiftDown();
  }

  /** Invoked just before a piece is moved */
  protected Command movedPiece(GamePiece p, Point loc) {
    setOldLocation(p);
    Command c = null;
    if (!loc.equals(p.getPosition())) {
      c = markMoved(p, true);
    }
    if (p.getParent() != null) {
      final Command removedCommand = p.getParent().pieceRemoved(p);
      c = c == null ? removedCommand : c.append(removedCommand);
    }
    return c;
  }

  protected void setOldLocation(GamePiece p) {
    if (p instanceof Stack) {
      for (int i = 0; i < ((Stack) p).getPieceCount(); i++) {
        Decorator.setOldProperties(((Stack) p).getPieceAt(i));
      }
    }
    else Decorator.setOldProperties(p);
  }

  public Command markMoved(GamePiece p, boolean hasMoved) {
    if (GlobalOptions.NEVER.equals(getMarkOption())) {
      hasMoved = false;
    }

    Command c = new NullCommand();
    if (!hasMoved || shouldMarkMoved()) {
      if (p instanceof Stack) {
        for (Iterator<GamePiece> i = ((Stack) p).getPiecesIterator();
             i.hasNext();) {
          c.append(markMoved(i.next(), hasMoved));
        }
      }
      else if (p.getProperty(Properties.MOVED) != null) {
        if (p.getId() != null) {
          final ChangeTracker comm = new ChangeTracker(p);
          p.setProperty(Properties.MOVED,
                        hasMoved ? Boolean.TRUE : Boolean.FALSE);
          c = comm.getChangeCommand();
        }
      }
    }
    return c;
  }

  protected boolean shouldMarkMoved() {
    final String option = getMarkOption();
    if (GlobalOptions.ALWAYS.equals(option)) {
      return true;
    }
    else if (GlobalOptions.NEVER.equals(option)) {
      return false;
    }
    else {
      return Boolean.TRUE.equals(
        GameModule.getGameModule().getPrefs().getValue(Map.MARK_MOVED));
    }
  }

  /**
   * Moves pieces in DragBuffer to point p by generating a Command for
   * each element in DragBuffer
   *
   * @param map
   *          Map
   * @param p
   *          Point mouse released
   */
  public Command movePieces(Map map, Point p) {
    final List<GamePiece> allDraggedPieces = new ArrayList<GamePiece>();
    final PieceIterator it = DragBuffer.getBuffer().getIterator();
    if (!it.hasMoreElements()) return null;

    Point offset = null;
    Command comm = new NullCommand();
    final BoundsTracker tracker = new BoundsTracker();
    // Map of Point->List<GamePiece> of pieces to merge with at a given
    // location. There is potentially one piece for each Game Piece Layer.
    final HashMap<Point,List<GamePiece>> mergeTargets =
      new HashMap<Point,List<GamePiece>>();
    while (it.hasMoreElements()) {
      dragging = it.nextPiece();
      tracker.addPiece(dragging);
      /*
       * Take a copy of the pieces in dragging.
       * If it is a stack, it is cleared by the merging process.
       */
      final ArrayList<GamePiece> draggedPieces = new ArrayList<GamePiece>(0);
      if (dragging instanceof Stack) {
        int size = ((Stack) dragging).getPieceCount();
        for (int i = 0; i < size; i++) {
           draggedPieces.add(((Stack) dragging).getPieceAt(i));
        }
      }
      else {
         draggedPieces.add(dragging);
      }

      if (offset != null) {
        p = new Point(dragging.getPosition().x + offset.x,
                      dragging.getPosition().y + offset.y);
      }

      List<GamePiece> mergeCandidates = mergeTargets.get(p);
      GamePiece mergeWith = null;
      // Find an already-moved piece that we can merge with at the destination
      // point
      if (mergeCandidates != null) {
        for (int i = 0, n = mergeCandidates.size(); i < n; ++i) {
          final GamePiece candidate = mergeCandidates.get(i);
          if (map.getPieceCollection().canMerge(candidate, dragging)) {
            mergeWith = candidate;
            mergeCandidates.set(i, dragging);
            break;
          }
        }
      }

      // Now look for an already-existing piece at the destination point
      if (mergeWith == null) {
        mergeWith = map.findAnyPiece(p, dropTargetSelector);
        if (mergeWith == null && !Boolean.TRUE.equals(
            dragging.getProperty(Properties.IGNORE_GRID))) {
          p = map.snapTo(p);
        }

        if (offset == null) {
          offset = new Point(p.x - dragging.getPosition().x,
                             p.y - dragging.getPosition().y);
        }

        if (mergeWith != null && map.getStackMetrics().isStackingEnabled()) {
          mergeCandidates = new ArrayList<GamePiece>();
          mergeCandidates.add(dragging);
          mergeCandidates.add(mergeWith);
          mergeTargets.put(p, mergeCandidates);
        }
      }

      if (mergeWith == null) {
        comm = comm.append(movedPiece(dragging, p));
        comm = comm.append(map.placeAt(dragging, p));
        if (!(dragging instanceof Stack) &&
            !Boolean.TRUE.equals(dragging.getProperty(Properties.NO_STACK))) {
          final Stack parent = map.getStackMetrics().createStack(dragging);
          if (parent != null) {
            comm = comm.append(map.placeAt(parent, p));
          }
        }
      }
      else {
        // Do not add pieces to the Deck that are Obscured to us, or that
        // the Deck does not want to contain. Removing them from the
        // draggedPieces list will cause them to be left behind where the
        // drag started. NB. Pieces that have been dragged from a face-down
        // Deck will be be Obscued to us, but will be Obscured by the dummy
        // user Deck.NO_USER
        if (mergeWith instanceof Deck) {
          final ArrayList<GamePiece> newList = new ArrayList<GamePiece>(0);
          for (GamePiece piece : draggedPieces) {
            if (((Deck) mergeWith).mayContain(piece)) {
              final boolean isObscuredToMe = Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME));
              if (!isObscuredToMe || (isObscuredToMe && Deck.NO_USER.equals(piece.getProperty(Properties.OBSCURED_BY)))) {
                newList.add(piece);
              }
            }
          }

          if (newList.size() != draggedPieces.size()) {
            draggedPieces.clear();
            for (GamePiece piece : newList) {
              draggedPieces.add(piece);
            }
          }
        }

        // Add the remaining dragged counters to the target.
        // If mergeWith is a single piece (not a Stack), then we are merging
        // into an expanded Stack and the merge order must be reversed to
        // maintain the order of the merging pieces.
        if (mergeWith instanceof Stack) {
          for (int i = 0; i < draggedPieces.size(); ++i) {
            comm = comm.append(movedPiece(draggedPieces.get(i), mergeWith.getPosition()));
            comm = comm.append(map.getStackMetrics().merge(mergeWith, draggedPieces.get(i)));
          }
        }
        else {
          for (int i = draggedPieces.size()-1; i >= 0; --i) {
            comm = comm.append(movedPiece(draggedPieces.get(i), mergeWith.getPosition()));
            comm = comm.append(map.getStackMetrics().merge(mergeWith, draggedPieces.get(i)));
          }
        }
      }

      for (GamePiece piece : draggedPieces) {
        KeyBuffer.getBuffer().add(piece);
      }

      // Record each individual piece moved
      for (GamePiece piece : draggedPieces) {
        allDraggedPieces.add(piece);
      }

      tracker.addPiece(dragging);
    }

    if (GlobalOptions.getInstance().autoReportEnabled()) {
      final Command report = createMovementReporter(comm).getReportCommand().append(new MovementReporter.HiddenMovementReporter(comm).getReportCommand());
      report.execute();
      comm = comm.append(report);
    }

    // Apply key after move to each moved piece
    if (map.getMoveKey() != null) {
      applyKeyAfterMove(allDraggedPieces, comm, map.getMoveKey());
    }

    tracker.repaint();
    return comm;
  }

  protected void applyKeyAfterMove(List<GamePiece> pieces,
                                   Command comm, KeyStroke key) {
    for (GamePiece piece : pieces) {
      if (piece.getProperty(Properties.SNAPSHOT) == null) {
        piece.setProperty(Properties.SNAPSHOT,
                          PieceCloner.getInstance().clonePiece(piece));
      }
      comm.append(piece.keyEvent(key));
    }
  }

  /**
   * This listener is used for faking drag-and-drop on Java 1.1 systems
   *
   * @param e
   */
  public void mousePressed(MouseEvent e) {
    if (canHandleEvent(e)) {
      selectMovablePieces(e);
    }
  }

  /** Place the clicked-on piece into the {@link DragBuffer} */
  protected void selectMovablePieces(MouseEvent e) {
    final GamePiece p = map.findPiece(e.getPoint(), dragTargetSelector);
    dragBegin = e.getPoint();
    if (p != null) {
      final EventFilter filter =
        (EventFilter) p.getProperty(Properties.MOVE_EVENT_FILTER);
      if (filter == null || !filter.rejectEvent(e)) {
        selectionProcessor.accept(p);
      }
      else {
        DragBuffer.getBuffer().clear();
      }
    }
    else {
      DragBuffer.getBuffer().clear();
    }
    // show/hide selection boxes
    map.repaint();
  }

  /** @deprecated Use {@link #selectMovablePieces(MouseEvent)}. */
  @Deprecated
  protected void selectMovablePieces(Point point) {
    final GamePiece p = map.findPiece(point, dragTargetSelector);
    dragBegin = point;
    selectionProcessor.accept(p);
    // show/hide selection boxes
    map.repaint();
  }

  protected boolean canHandleEvent(MouseEvent e) {
    return !e.isShiftDown() &&
           !e.isControlDown() &&
           !e.isMetaDown() &&
            e.getClickCount() < 2 &&
           !e.isConsumed();
  }

  /**
   * Return true if this point is "close enough" to the point at which
   * the user pressed the mouse to be considered a mouse click (such
   * that no moves are done)
   */
  public boolean isClick(Point pt) {
    boolean isClick = false;
    if (dragBegin != null) {
      final Board b = map.findBoard(pt);
      boolean useGrid = b != null && b.getGrid() != null;
      if (useGrid) {
        final PieceIterator it = DragBuffer.getBuffer().getIterator();
      final GamePiece dragging = it.hasMoreElements() ? it.nextPiece() : null;
        useGrid =
          dragging != null &&
          !Boolean.TRUE.equals(dragging.getProperty(Properties.IGNORE_GRID)) &&
          (dragging.getParent() == null || !dragging.getParent().isExpanded());
      }
      if (useGrid) {
        if (map.equals(DragBuffer.getBuffer().getFromMap())) {
          if (map.snapTo(pt).equals(map.snapTo(dragBegin))) {
            isClick = true;
          }
        }
      }
      else {
        if (Math.abs(pt.x - dragBegin.x) <= 5 &&
            Math.abs(pt.y - dragBegin.y) <= 5) {
          isClick = true;
        }
      }
    }
    return isClick;
  }

  public void mouseReleased(MouseEvent e) {
    if (canHandleEvent(e)) {
      if (!isClick(e.getPoint())) {
        performDrop(e.getPoint());
      }
    }
    dragBegin = null;
    map.getView().setCursor(null);
  }

  protected void performDrop(Point p) {
    final Command move = movePieces(map, p);
    GameModule.getGameModule().sendAndLog(move);
    if (move != null) {
      DragBuffer.getBuffer().clear();
    }
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  /**
   * Implement Comparator to sort the contents of the drag buffer before
   * completing the drag. This sorts the contents to be in the same order
   * as the pieces were in their original parent stack.
   */
  public int compare(GamePiece p1, GamePiece p2) {
    return pieceSorter.compare(p1, p2);
  }

  // We force the loading of these classes because otherwise they would
  // be loaded when the user initiates the first drag, which makes the
  // start of the drag choppy.
  static {
    try {
      Class.forName(MovementReporter.class.getName(),
                    true, MovementReporter.class.getClassLoader());
      Class.forName(KeyBuffer.class.getName(),
                    true, KeyBuffer.class.getClassLoader());
    }
    catch (ClassNotFoundException e) {
      throw new IllegalStateException(e); // impossible
    }
  }

  /** @deprecated Use {@link DragHandler} instead. */
  @Deprecated
  static public class AbstractDragHandler extends VASSAL.build.module.map.DragHandler {
    private VASSAL.build.module.map.DragHandler theRealDragHandler =
      VASSAL.build.module.map.DragHandler.getTheDragHandler();
    static private final AbstractDragHandler shim = new AbstractDragHandler();

    static public AbstractDragHandler getTheDragHandler() {
      return shim;
    }

    static public void setTheDragHandler(AbstractDragHandler myHandler) {
      VASSAL.build.module.map.DragHandler.setTheDragHandler(shim);
      shim.theRealDragHandler = myHandler;
    }

    static public DropTarget makeDropTarget(Component theComponent, int dndContents, DropTargetListener dropTargetListener) {
      return VASSAL.build.module.map.DragHandler.makeDropTarget(
        theComponent, dndContents, dropTargetListener
      );
    }

    static public void removeDropTarget(Component theComponent) {
      VASSAL.build.module.map.DragHandler.removeDropTarget(theComponent);
    }

    protected int getOffsetMult() {
      return theRealDragHandler.getOffsetMult();
    }

    public void setDrawWinToOwnerOf(Component newDropWin) {
      theRealDragHandler.setDrawWinToOwnerOf(newDropWin);
    }

    public void dragGestureRecognized(DragGestureEvent dge) {
      theRealDragHandler.dragGestureRecognized(dge);
    }

    public void dragDropEnd(DragSourceDropEvent e) {
      theRealDragHandler.dragDropEnd(e);
    }

    public void dragEnter(DragSourceDragEvent e) {
      theRealDragHandler.dragEnter(e);
    }

    public void dragExit(DragSourceEvent e) {
      theRealDragHandler.dragExit(e);
    }

    public void dragOver(DragSourceDragEvent e) {
      theRealDragHandler.dragOver(e);
    }

    public void dropActionChanged(DragSourceDragEvent e) {
      theRealDragHandler.dropActionChanged(e);
    }

    public void dragMouseMoved(DragSourceDragEvent e) {
      theRealDragHandler.dragMouseMoved(e);
    }

    public void dragEnter(DropTargetDragEvent e) {
      theRealDragHandler.dragEnter(e);
    }

    public void drop(DropTargetDropEvent e) {
      theRealDragHandler.drop(e);
    }

    public void dragExit(DropTargetEvent e) {
      theRealDragHandler.dragExit(e);
    }

    public void dragOver(DropTargetDragEvent e) {
      theRealDragHandler.dragOver(e);
    }

    public void dropActionChanged(DropTargetDragEvent e) {
      theRealDragHandler.dropActionChanged(e);
    }
  }

  /** @deprecated Use {@link DragHandlerNoImage} instead. */
  @Deprecated
  static public class DragHandlerNoImage extends AbstractDragHandler {
    public DragHandlerNoImage() {
      AbstractDragHandler.setTheDragHandler(new DragHandlerNonNative());
    }
  }

  /** @deprecated Use {@link DragHandlerNative} instead. */
  @Deprecated
  static public class DragHandler extends AbstractDragHandler {
    public DragHandler() {
      AbstractDragHandler.setTheDragHandler(new DragHandlerNative());
    }
  }

  /** @deprecated Use {@link DragHandlerImageMacOSX} instead. */
  @Deprecated
  static public class DragHandlerMacOSX extends AbstractDragHandler {
    public DragHandlerMacOSX() {
      AbstractDragHandler.setTheDragHandler(new DragHandlerNativeMacOSX());
    }
  }
}
