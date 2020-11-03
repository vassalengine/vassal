/*
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

import VASSAL.i18n.Resources;
import VASSAL.tools.ProblemDialog;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Image;
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
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JRootPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import org.apache.commons.lang3.SystemUtils;

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
import VASSAL.counters.BasicPiece;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.Decorator;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Highlighter;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.PieceSorter;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.PropertyExporter;
import VASSAL.counters.Stack;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.swing.SwingUtils;

/**
 * This is a MouseListener that moves pieces onto or within a Map window. Handles dragging and dropping of
 * both individual pieces, stacks, and groups of pieces/stacks. It is a subcomponent of Map.
 */
public class PieceMover extends AbstractBuildable
                        implements MouseListener,
                                   GameComponent,
                                   Comparator<GamePiece> {
  /** The Preferences key for auto-reporting moves. */
  public static final String AUTO_REPORT = "autoReport"; //$NON-NLS-1$
  public static final String NAME = "name"; //NON-NLS

  public static final String HOTKEY = "hotkey"; //NON-NLS

  protected Map map;                           // Map we're the PieceMover for.
  protected Point dragBegin;
  protected GamePiece dragging;
  protected LaunchButton markUnmovedButton;
  protected String markUnmovedText;
  protected String markUnmovedIcon;
  public static final String ICON_NAME = "icon"; //$NON-NLS-1$
  protected String iconName;

  protected PieceFinder dragTargetSelector; // Selects drag target from mouse click on the Map
  protected PieceFinder dropTargetSelector; // Selects piece to merge with at the drop destination
  protected PieceVisitorDispatcher selectionProcessor; // Processes drag target after having been selected
  protected Comparator<GamePiece> pieceSorter = new PieceSorter();

  /**
   * Adds this component to its parent map. Add ourselves as a mouse listener, drag gesture listener, etc.
   * @param b Map to add to
   */
  @Override
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

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (iconName != null) s.add(iconName);
    if (markUnmovedIcon != null) s.add(markUnmovedIcon);
  }

  /**
   * When the user completes a drag-drop operation, the pieces being dragged will either be combined
   * with an existing piece on the map or else placed on the map without stack.
   *
   * @return a {@link PieceFinder} instance that determines which {@link GamePiece} (if any) to
   *     combine the being-dragged pieces with.
   */
  protected PieceFinder createDropTargetSelector() {
    return new PieceFinder.Movable() {
      /**
       * When a deck exists on the map, and we need to find out if our piece was dragged to the deck
       * @param d Potential target {@link Deck}
       * @return true if our target location is inside the footprint of the Deck.
       */
      @Override
      public Object visitDeck(Deck d) {
        final Point pos = d.getPosition();
        final Point p = new Point(pt.x - pos.x, pt.y - pos.y);
        return d.getShape().contains(p) ? d : null;
      }

      /**
       * When an unstacked piece exists on the map, we see if this is a piece we could
       * form a stack with -- if it is at our precise location (or the location we would
       * be getting snapped to).
       * @param piece Potential target piece on the map.
       * @return The piece to stack with, if we should, otherwise null.
       */
      @Override
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

        // We don't drag a piece "to itself".
        if (selected != null &&
            DragBuffer.getBuffer().contains(selected) &&
            selected.getParent() != null &&
            selected.getParent().topPiece() == selected) {  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
          selected = null;
        }
        return selected;
      }

      /**
       * When a stack already exists on the map, we see if this our piece could be added
       * to it.
       * @param s Stack to check if we're appropriately configured to merge with
       * @return The piece to stack with, if we should, otherwise null.
       */
      @Override
      public Object visitStack(Stack s) {
        GamePiece selected = null;
        if (this.map.getStackMetrics().isStackingEnabled() &&
            this.map.getPieceCollection().canMerge(dragging, s) &&
            !DragBuffer.getBuffer().contains(s) &&
            !DragBuffer.getBuffer().containsAllMembers(s) &&  //BR// Don't merge back into a stack we are in the act of emptying
            s.topPiece() != null) {  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
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
   * @return Dispatcher
   */
  protected PieceVisitorDispatcher createSelectionProcessor() {
    return new DeckVisitorDispatcher(new DeckVisitor() {
      /**
       * We've picked a Deck - Clear the drag buffer and add Deck's top piece to the drag buffer.
       * @param d Deck we clicked on
       * @return null
       */
      @Override
      public Object visitDeck(Deck d) {
        final DragBuffer dbuf = DragBuffer.getBuffer();
        dbuf.clear();
        for (final PieceIterator it = d.drawCards(); it.hasMoreElements();) {
          final GamePiece p = it.nextPiece();
          p.setProperty(Properties.OBSCURED_BY, p.getProperty(Properties.OBSCURED_BY_PRE_DRAW)); // Bug 13433 restore correct OBSCURED_BY
          dbuf.add(p);
        }
        return null;
      }

      /**
       * We've picked a Stack. Clear the drag buffer and add the stack's pieces to it.
       * @param s Stack we clicked on
       * @return null
       */
      @Override
      public Object visitStack(Stack s) {
        final DragBuffer dbuf = DragBuffer.getBuffer();
        dbuf.clear();
        // RFE 1629255 - Only add selected pieces within the stack to the DragBuffer,
        // except when global pref "MOVING_STACKS_PICKUP_UNITS" is set
        final boolean selectAllUnitsInStackRegardlessOfSelection =
            (Boolean) GameModule.getGameModule().getPrefs().getValue(Map.MOVING_STACKS_PICKUP_UNITS);

        s.asList().forEach(
          selectAllUnitsInStackRegardlessOfSelection ?
            dbuf::add :
          (p) -> {
            if (Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
              dbuf.add(p);
            }
          }
        );
        // End RFE 1629255

        final KeyBuffer kbuf = KeyBuffer.getBuffer();
        if (kbuf.containsChild(s)) {
          // If clicking on a stack with a selected piece, put all selected
          // pieces in other stacks into the drag buffer
          kbuf.sort(PieceMover.this);
          for (final GamePiece piece : kbuf.asList()) {
            if (piece.getParent() != s) {
              dbuf.add(piece);
            }
          }
        }
        return null;
      }

      /**
       * We've clicked a regular (non-stacked) piece. Clear drag buffer and the piece.
       * @param selected piece clicked on
       * @return null
       */
      @Override
      public Object visitDefault(GamePiece selected) {
        final DragBuffer dbuf = DragBuffer.getBuffer();
        dbuf.clear();

        final KeyBuffer kbuf = KeyBuffer.getBuffer();
        if (kbuf.contains(selected)) {
          // If clicking on a selected piece, put all selected pieces into the
          // drag buffer
          kbuf.sort(PieceMover.this);
          for (final GamePiece piece : kbuf.asList()) {
            dbuf.add(piece);
          }
        }
        else {
          dbuf.add(selected);
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
   * @return Piece Finder
   */
  protected PieceFinder createDragTargetSelector() {
    return new PieceFinder.Movable() {
      @Override
      public Object visitDeck(Deck d) {
        final Point pos = d.getPosition();
        final Point p = new Point(pt.x - pos.x, pt.y - pos.y);
        return d.boundingBox().contains(p) && d.getPieceCount() > 0 ? d : null;
      }
    };
  }

  /**
   * Detects when a game is starting, for purposes of managing the mark-unmoved button.
   * @param gameStarting if true, a game is starting.  If false, then a game is ending
   */
  @Override
  public void setup(boolean gameStarting) {
    if (gameStarting) {
      initButton();
    }
  }

  /**
   * PieceMover has nothing to save/restore in a save file.
   * @return null
   */
  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * @param name Name of icon file
   * @return Image for button icon
   */
  private Image loadIcon(String name) {
    if (name == null || name.length() == 0) return null;
    return Op.load(name).getImage();
  }

  /**
   * PieceMover manages the "Mark All Pieces Unmoved" button for the map.
   */
  protected void initButton() {
    final String value = getMarkOption();
    if (GlobalOptions.PROMPT.equals(value)) {
      final BooleanConfigurer config = new BooleanConfigurer(
        Map.MARK_MOVED, Resources.getString("Editor.PieceMover.mark_moved_pieces"), Boolean.TRUE);
      GameModule.getGameModule().getPrefs().addOption(config);
    }

    if (!GlobalOptions.NEVER.equals(value)) {
      if (markUnmovedButton == null) {
        final ActionListener al = e -> {
          final GamePiece[] p = map.getAllPieces();
          final Command c = new NullCommand();
          for (final GamePiece gamePiece : p) {
            c.append(markMoved(gamePiece, false));
          }
          GameModule.getGameModule().sendAndLog(c);
          map.repaint();
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

  /**
   * @return Our setting w/ regard to marking pieces moved.
   */
  private String getMarkOption() {
    String value = map.getAttributeValueString(Map.MARK_MOVED);
    if (value == null) {
      value = GlobalOptions.getInstance()
                           .getAttributeValueString(GlobalOptions.MARK_MOVED);
    }
    return value;
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{ICON_NAME};
  }

  @Override
  public String getAttributeValueString(String key) {
    return ICON_NAME.equals(key) ? iconName : null;
  }

  @Override
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
    Command c = new NullCommand();
    c = c.append(setOldLocations(p));
    if (!loc.equals(p.getPosition())) {
      c = c.append(markMoved(p, true));
    }
    if (p.getParent() != null) {
      final Command removedCommand = p.getParent().pieceRemoved(p);
      c = c.append(removedCommand);
    }
    return c;
  }

  /**
   * @deprecated  {@link #setOldLocations(GamePiece)} to return generated Commands
   * @param p Piece
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected void setOldLocation(GamePiece p) {
    ProblemDialog.showDeprecated("2020-08-06");
    setOldLocations(p);
  }

  protected Command setOldLocations(GamePiece p) {
    Command comm = new NullCommand();
    if (p instanceof Stack) {
      for (final GamePiece gamePiece : ((Stack) p).asList()) {
        comm = comm.append(Decorator.putOldProperties(gamePiece));
      }
    }
    else {
      comm = comm.append(Decorator.putOldProperties(p));
    }
    return comm;

  }

  public Command markMoved(GamePiece p, boolean hasMoved) {
    if (GlobalOptions.NEVER.equals(getMarkOption())) {
      hasMoved = false;
    }

    Command c = new NullCommand();
    if (!hasMoved || shouldMarkMoved()) {
      if (p instanceof Stack) {
        for (final GamePiece gamePiece : ((Stack) p).asList()) {
          c = c.append(markMoved(gamePiece, hasMoved));
        }
      }
      else if (p.getProperty(Properties.MOVED) != null) {
        if (p.getId() != null) {
          final ChangeTracker comm = new ChangeTracker(p);
          p.setProperty(Properties.MOVED,
                        hasMoved ? Boolean.TRUE : Boolean.FALSE);
          c = c.append(comm.getChangeCommand());
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
    final PieceIterator it = DragBuffer.getBuffer().getIterator();
    if (!it.hasMoreElements()) return null;

    final List<GamePiece> allDraggedPieces = new ArrayList<>();
    Point offset = null;
    Command comm = new NullCommand();
    final BoundsTracker tracker = new BoundsTracker();
    // Map of Point->List<GamePiece> of pieces to merge with at a given
    // location. There is potentially one piece for each Game Piece Layer.
    final HashMap<Point, List<GamePiece>> mergeTargets = new HashMap<>();
    while (it.hasMoreElements()) {
      dragging = it.nextPiece();
      tracker.addPiece(dragging);
      /*
       * Take a copy of the pieces in dragging.
       * If it is a stack, it is cleared by the merging process.
       */
      final ArrayList<GamePiece> draggedPieces = new ArrayList<>(0);
      if (dragging instanceof Stack) {
        draggedPieces.addAll(((Stack) dragging).asList());
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
        final int n = mergeCandidates.size();
        for (int i = 0; i < n; ++i) {
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
          mergeCandidates = new ArrayList<>();
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

            //BR// We've made a new stack, so put it on the list of merge targets, in case more pieces land here too
            mergeCandidates = new ArrayList<>();
            mergeCandidates.add(dragging);
            mergeCandidates.add(parent);
            mergeTargets.put(p, mergeCandidates);
          }
        }
      }
      else {
        // Do not add pieces to the Deck that are Obscured to us, or that
        // the Deck does not want to contain. Removing them from the
        // draggedPieces list will cause them to be left behind where the
        // drag started. NB. Pieces that have been dragged from a face-down
        // Deck will be be Obscured to us, but will be Obscured by the dummy
        // user Deck.NO_USER
        if (mergeWith instanceof Deck) {
          final ArrayList<GamePiece> newList = new ArrayList<>(0);
          for (final GamePiece piece : draggedPieces) {
            if (((Deck) mergeWith).mayContain(piece)) {
              final boolean isObscuredToMe = Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME));
              if (!isObscuredToMe || Deck.NO_USER.equals(piece.getProperty(Properties.OBSCURED_BY))) {
                newList.add(piece);
              }
            }
          }

          if (newList.size() != draggedPieces.size()) {
            draggedPieces.clear();
            draggedPieces.addAll(newList);
          }
        }

        // Add the remaining dragged counters to the target.
        // If mergeWith is a single piece (not a Stack), then we are merging
        // into an expanded Stack and the merge order must be reversed to
        // maintain the order of the merging pieces.
        if (mergeWith instanceof Stack) {
          for (final GamePiece draggedPiece : draggedPieces) {
            comm = comm.append(movedPiece(draggedPiece, mergeWith.getPosition()));
            comm = comm.append(map.getStackMetrics().merge(mergeWith, draggedPiece));
          }
        }
        else {
          for (int i = draggedPieces.size() - 1; i >= 0; --i) {
            comm = comm.append(movedPiece(draggedPieces.get(i), mergeWith.getPosition()));
            comm = comm.append(map.getStackMetrics().merge(mergeWith, draggedPieces.get(i)));
          }
        }
      }

      for (final GamePiece piece : draggedPieces) {
        KeyBuffer.getBuffer().add(piece);
      }

      // Record each individual piece moved
      allDraggedPieces.addAll(draggedPieces);

      tracker.addPiece(dragging);
    }

    if (GlobalOptions.getInstance().autoReportEnabled()) {
      final Command report = createMovementReporter(comm).getReportCommand().append(new MovementReporter.HiddenMovementReporter(comm).getReportCommand());
      report.execute();
      comm = comm.append(report);
    }

    // Apply key after move to each moved piece
    if (map.getMoveKey() != null) {
      comm = comm.append(applyKeyAfterMove(allDraggedPieces, map.getMoveKey()));
    }

    tracker.repaint();
    return comm;
  }

  /**
   * @deprecated Use {@link #applyKeyAfterMove(List, KeyStroke)} to return Commands
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected void applyKeyAfterMove(List<GamePiece> pieces, Command comm, KeyStroke key) {
    ProblemDialog.showDeprecated("2020-08-06");
    comm.append(applyKeyAfterMove(pieces, key));
  }

  protected Command applyKeyAfterMove(List<GamePiece> pieces, KeyStroke key) {
    Command comm = new NullCommand();
    for (final GamePiece piece : pieces) {
      if (piece.getProperty(Properties.SNAPSHOT) == null) {
        piece.setProperty(Properties.SNAPSHOT, ((PropertyExporter) piece).getProperties());
      }
      comm = comm.append(piece.keyEvent(key));
    }
    return comm;
  }

  /**
   * This listener is used for faking drag-and-drop on Java 1.1 systems
   *
   * @param e Event
   */
  @Override
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
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected void selectMovablePieces(Point point) {
    ProblemDialog.showDeprecated("2020-08-06");
    final GamePiece p = map.findPiece(point, dragTargetSelector);
    dragBegin = point;
    selectionProcessor.accept(p);
    // show/hide selection boxes
    map.repaint();
  }

  protected boolean canHandleEvent(MouseEvent e) {
    return !e.isConsumed() &&
           !e.isShiftDown() &&
           !SwingUtils.isSelectionToggle(e) &&
           e.getClickCount() < 2 &&
           (e.getButton() == MouseEvent.NOBUTTON ||
            SwingUtils.isMainMouseButtonDown(e));
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

  @Override
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

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  /**
   * Implement Comparator to sort the contents of the drag buffer before
   * completing the drag. This sorts the contents to be in the same order
   * as the pieces were in their original parent stack.
   */
  @Override
  public int compare(GamePiece p1, GamePiece p2) {
    return pieceSorter.compare(p1, p2);
  }

  // We force the loading of these classes because otherwise they would
  // be loaded when the user initiates the first drag, which makes the
  // start of the drag choppy.
  static {
    try {
      Class.forName(MovementReporter.class.getName());
      Class.forName(KeyBuffer.class.getName());
    }
    catch (ClassNotFoundException e) {
      throw new IllegalStateException(e); // impossible
    }
  }

  /** Common functionality for DragHandler for cases with and without
   * drag image support. <p>
   * NOTE: DragSource.isDragImageSupported() returns false for j2sdk1.4.2_02 on
   * Windows 2000
   *
   * @author Pieter Geerkens
   */
  public abstract static class AbstractDragHandler
    implements DragGestureListener,       DragSourceListener,
               DragSourceMotionListener,  DropTargetListener   {
    private static AbstractDragHandler theDragHandler =
        DragSource.isDragImageSupported() ? (SystemUtils.IS_OS_MAC_OSX ?
        new DragHandlerMacOSX() : new DragHandler()) :
        new DragHandlerNoImage();

    /** returns the singleton DragHandler instance */
    public static AbstractDragHandler getTheDragHandler() {
      return theDragHandler;
    }

    public static void setTheDragHandler(AbstractDragHandler myHandler) {
      theDragHandler = myHandler;
    }

    static final int CURSOR_ALPHA = 127; // pseudo cursor is 50% transparent
    static final int EXTRA_BORDER = 4; // pseudo cursor is includes a 4 pixel border

    protected JLabel dragCursor; // An image label. Lives on current DropTarget's
    // LayeredPane.
    //      private BufferedImage dragImage; // An image label. Lives on current DropTarget's LayeredPane.
    private final Point drawOffset = new Point(); // translates event coords to local
                                            // drawing coords
    private Rectangle boundingBox; // image bounds
    private int originalPieceOffsetX; // How far drag STARTED from gamepieces
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
    JLayeredPane drawWin; // the component that owns our pseudo-cursor
    // Seems there can be only one DropTargetListener a drop target. After we
    // process a drop target
    // event, we manually pass the event on to this listener.
    java.util.Map<Component, DropTargetListener> dropTargetListeners =
      new HashMap<>();

    protected abstract int getOffsetMult();

    protected abstract double getDeviceScale(DragGestureEvent dge);

    /**
     * Creates a new DropTarget and hooks us into the beginning of a
     * DropTargetListener chain. DropTarget events are not multicast;
     * there can be only one "true" listener.
     */
    public static DropTarget makeDropTarget(Component theComponent, int dndContants, DropTargetListener dropTargetListener) {
      if (dropTargetListener != null) {
        DragHandler.getTheDragHandler()
                   .dropTargetListeners.put(theComponent, dropTargetListener);
      }
      return new DropTarget(theComponent, dndContants,
                            DragHandler.getTheDragHandler());
    }

    public static void removeDropTarget(Component theComponent) {
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
     * @param zoom Zoom Level
     * @param doOffset Drag Offset
     * @param target Target Component
     * @param setSize Set Size
     * @return Drag Image
     */
    BufferedImage makeDragImageCursorCommon(double zoom, boolean doOffset,
      Component target, boolean setSize) {

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
      final ArrayList<Point> relativePositions = new ArrayList<>();
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

      relativePositions.add(new Point(0, 0));
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
          r.translate(sm.unexSepX * stackCount, -sm.unexSepY * stackCount);
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
      for (final PieceIterator dragContents = DragBuffer.getBuffer().getIterator();
           dragContents.hasMoreElements(); ) {

        final GamePiece piece = dragContents.nextPiece();
        final Point pos = relativePositions.get(index++);
        final Map map = piece.getMap();

        if (piece instanceof Stack) {
          stackCount = 0;
          piece.draw(g, EXTRA_BORDER - boundingBox.x + pos.x,
                        EXTRA_BORDER - boundingBox.y + pos.y,
                        map == null ? target : map.getView(), zoom);
        }
        else {
          final Point offset = new Point(0, 0);
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

          String owner = "";
          if (piece.getParent() instanceof Deck) {
            owner = (String)piece.getProperty(Properties.OBSCURED_BY);
            piece.setProperty(Properties.OBSCURED_BY, ((Deck) piece.getParent()).isFaceDown() ? Deck.NO_USER : null);
          }
          piece.draw(g, x, y, map == null ? target : map.getView(), zoom);
          if (piece.getParent() instanceof Deck) {
            piece.setProperty(Properties.OBSCURED_BY, owner);
          }

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
        g.drawRect(f, f, w - 2 * f, h - 2 * f);
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
    @Override
    public void dragGestureRecognized(DragGestureEvent dge) {
      try {
        beginDragging(dge);
      }
      // FIXME: Fix by replacing AWT Drag 'n Drop with Swing DnD.
      // Catch and ignore spurious DragGestures
      catch (InvalidDnDOperationException ignored) {
      }
    }

    protected Point dragGestureRecognizedPrep(DragGestureEvent dge) {
      // Ensure the user has dragged on a counter before starting the drag.
      final DragBuffer db = DragBuffer.getBuffer();
      if (db.isEmpty()) return null;

      // Remove any Immovable pieces from the DragBuffer that were
      // selected in a selection rectangle, unless they are being
      // dragged from a piece palette (i.e., getMap() == null).
      final List<GamePiece> pieces = new ArrayList<>();
      for (final PieceIterator i = db.getIterator();
           i.hasMoreElements(); pieces.add(i.nextPiece()));
      for (final GamePiece piece : pieces) {
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

      final Point mousePosition = dge.getDragOrigin(); //BR// Bug13137 - now that we're not pre-adulterating dge's event, it already arrives in component coordinates
      Point piecePosition = (map == null)
                    ?  piece.getPosition()
                    : map.mapToComponent(piece.getPosition());
      // If DragBuffer holds a piece with invalid coordinates (for example, a
      // card drawn from a deck), drag from center of piece
      if (piecePosition.x <= 0 || piecePosition.y <= 0) {
        piecePosition = mousePosition;
      }
      // Account for offset of piece within stack
      // We do this even for un-expanded stacks, since the offset can
      // still be significant if the stack is large
      dragPieceOffCenterZoom = (map == null ? 1.0 : map.getZoom()) * getDeviceScale(dge);

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
    @Override
    public void dragDropEnd(DragSourceDropEvent e) {
      final DragSource ds = e.getDragSourceContext().getDragSource();
      ds.removeDragSourceMotionListener(this);
    }

    @Override
    public void dragEnter(DragSourceDragEvent e) {}

    @Override
    public void dragExit(DragSourceEvent e) {}

    @Override
    public void dragOver(DragSourceDragEvent e) {}

    @Override
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
    @Override
    public abstract void dragMouseMoved(DragSourceDragEvent e);

    ///////////////////////////////////////////////////////////////////////////
    // DROP TARGET INTERFACE
    //
    // EVENT uses UNSCALED, DROP-TARGET coordinate system
    ///////////////////////////////////////////////////////////////////////////
    /** switches current drawWin when mouse enters a new DropTarget */
    @Override
    public void dragEnter(DropTargetDragEvent e) {
      final DropTargetListener forward = getListener(e);
      if (forward != null) {
        forward.dragEnter(e);
      }
    }

    /**
     * Last event of the drop operation. We adjust the drop point for
     * off-center drag, remove the cursor, and pass the event along
     * listener chain.
     */
    @Override
    public void drop(DropTargetDropEvent e) {
      // EVENT uses UNSCALED, DROP-TARGET coordinate system
      e.getLocation().translate(currentPieceOffsetX, currentPieceOffsetY);
      final DropTargetListener forward = getListener(e);
      if (forward != null) {
        forward.drop(e);
      }
    }

    /** ineffectual. Passes event along listener chain */
    @Override
    public void dragExit(DropTargetEvent e) {
      final DropTargetListener forward = getListener(e);
      if (forward != null) forward.dragExit(e);
    }

    /** ineffectual. Passes event along listener chain */
    @Override
    public void dragOver(DropTargetDragEvent e) {
      final DropTargetListener forward = getListener(e);
      if (forward != null) forward.dragOver(e);
    }

    /** ineffectual. Passes event along listener chain */
    @Override
    public void dropActionChanged(DropTargetDragEvent e) {
      final DropTargetListener forward = getListener(e);
      if (forward != null) forward.dropActionChanged(e);
    }
  }

  /**
   * Implements a pseudo-cursor that follows the mouse cursor when user
   * drags gamepieces. Supports map zoom by resizing cursor when it enters
   * a drop target of type Map.View.
   *
   * @author Jim Urbas
   * @version 0.4.2
   *
   */
  public static class DragHandlerNoImage extends AbstractDragHandler {
    @Override
    public void dragGestureRecognized(DragGestureEvent dge) {
      final Point mousePosition = dragGestureRecognizedPrep(dge);
      if (mousePosition == null) return;

      makeDragCursor(dragPieceOffCenterZoom);
      setDrawWinToOwnerOf(dragWin);
      SwingUtilities.convertPointToScreen(mousePosition, drawWin);
      moveDragCursor(mousePosition.x, mousePosition.y);

      super.dragGestureRecognized(dge);
    }

    @Override
    protected int getOffsetMult() {
      return 1;
    }

    @Override
    protected double getDeviceScale(DragGestureEvent dge) {
      return 1.0;
    }

    @Override
    public void dragDropEnd(DragSourceDropEvent e) {
      removeDragCursor();
      super.dragDropEnd(e);
    }

    @Override
    public void dragMouseMoved(DragSourceDragEvent e) {
      if (!e.getLocation().equals(lastDragLocation)) {
        lastDragLocation = e.getLocation();
        moveDragCursor(e.getX(), e.getY());
        if (dragCursor != null && !dragCursor.isVisible()) {
          dragCursor.setVisible(true);
        }
      }
    }

    @Override
    public void dragEnter(DropTargetDragEvent e) {
      final Component newDropWin = e.getDropTargetContext().getComponent();
      if (newDropWin != dropWin) {
        final double newZoom = newDropWin instanceof Map.View
          ? ((Map.View) newDropWin).getMap().getZoom() : 1.0;
        if (Math.abs(newZoom - dragCursorZoom) > 0.01) {
          makeDragCursor(newZoom);
        }
        setDrawWinToOwnerOf(e.getDropTargetContext().getComponent());
        dropWin = newDropWin;
      }
      super.dragEnter(e);
    }

    @Override
    public void drop(DropTargetDropEvent e) {
      removeDragCursor();
      super.drop(e);
    }
  }

  /**
   * Implementation of AbstractDragHandler when DragImage is supported by JRE
   *
   * @author Pieter Geerkens
   */
  public static class DragHandler extends AbstractDragHandler {
    @Override
    public void dragGestureRecognized(DragGestureEvent dge) {
      if (dragGestureRecognizedPrep(dge) == null) return;
      super.dragGestureRecognized(dge);
    }

    @Override
    protected int getOffsetMult() {
      return -1;
    }

    @Override
    protected double getDeviceScale(DragGestureEvent dge) {
      // Get the OS scaling; note that this is _probably_ running only on
      // Windows.
      final Graphics2D g2d = (Graphics2D) dge.getComponent().getGraphics();
      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
      g2d.dispose();
      return os_scale;
    }

    @Override
    public void dragMouseMoved(DragSourceDragEvent e) {}
  }

  public static class DragHandlerMacOSX extends DragHandler {
    @Override
    protected int getOffsetMult() {
      return 1;
    }

    @Override
    protected double getDeviceScale(DragGestureEvent dge) {
      // Retina Macs account for the device scaling for the drag icon, so
      // we don't have to.
      return 1.0;
    }
  }
}
