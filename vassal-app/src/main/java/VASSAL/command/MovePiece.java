package VASSAL.command;

import java.awt.Point;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.CompoundPieceCollection;
import VASSAL.build.module.map.HighlightLastMoved;
import VASSAL.build.module.map.PieceCollection;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

/*
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * Command that moves a piece to a new location and position within a stack.
 * While this can be accomplished with a {@link ChangePiece} command, this
 * command is safer in terms of recovering from changes to the game state that may have occurred
 * since the command was created.  For instance, A {@link ChangePiece} command that adds
 * a piece to a {@link VASSAL.counters.Stack} will cause the piece to disappear if the
 * stack has been deleted.  This Command will recover more gracefully.
 */
public class MovePiece extends Command {
  private final String id;
  private final String newMapId;
  private final String oldMapId;
  private final Point newPosition;
  private final Point oldPosition;
  private final String newUnderneathId;
  private final String oldUnderneathId;
  private final String playerId;

  /**
   *
   * @param id The id of the piece being moved
   * @param newMapId The id of the map being moved to
   * @param newPosition the new position
   * @param newUnderneathId The id of the piece which will be immediately beneath this piece in any containing Stack.  May be null
   * @param oldMapId The id of the map being moved from
   * @param oldPosition the old position
   * @param oldUnderneathId The id of the piece which was immediately beneath this piece in its original containing Stack.
   * @param playerId the id of the player making this move
   */
  public MovePiece(String id, String newMapId, Point newPosition, String newUnderneathId, String oldMapId, Point oldPosition, String oldUnderneathId, String playerId) {
    this.id = id;
    this.newMapId = newMapId;
    this.oldMapId = oldMapId;
    this.newPosition = newPosition;
    this.oldPosition = oldPosition;
    this.newUnderneathId = newUnderneathId;
    this.oldUnderneathId = oldUnderneathId;
    this.playerId = playerId;
  }

  public String getId() {
    return id;
  }

  public String getNewMapId() {
    return newMapId;
  }

  public String getOldMapId() {
    return oldMapId;
  }

  public Point getNewPosition() {
    return newPosition;
  }

  public Point getOldPosition() {
    return oldPosition;
  }

  public String getNewUnderneathId() {
    return newUnderneathId;
  }

  public String getOldUnderneathId() {
    return oldUnderneathId;
  }

  public String getPlayerId() {
    return playerId;
  }

  private void stackOrPlacePiece(GamePiece piece, Map newMap, Point newPosition) {
    final boolean stackable = newMap.getStackMetrics().isStackingEnabled()
      && !(piece instanceof Stack)
      && !Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK));

    if (stackable && piece.getParent() == null) {
      final Stack s = new Stack();
      s.add(piece);
      GameModule.getGameModule().getGameState().addPiece(s);
      newMap.placeAt(s, newPosition);
      return;
    }

    // A piece leaving a Stack goes into an empty Stack waiting at the destination rather than
    // being left unstacked, e.g. its own Stack when undoing a move out of a location where it was alone
    final Stack empty = stackable ? findEmptyStack(newMap, piece, newPosition) : null;
    if (empty != null) {
      newMap.getStackMetrics().merge(empty, piece);
    }
    else {
      newMap.placeAt(piece, newPosition);
    }
  }

  /**
   * Find an empty Stack at a position that a piece can join. The Stack must already be bound to the
   * piece's layer. If there are several, the one with the lowest id is chosen so that every client
   * picks the same one.
   */
  private Stack findEmptyStack(Map map, GamePiece piece, Point pos) {
    final PieceCollection pieces = map.getPieceCollection();
    final int layer = pieces instanceof CompoundPieceCollection ?
      ((CompoundPieceCollection) pieces).getLayerForPiece(piece) : Stack.LAYER_NOT_SET;
    if (layer == Stack.LAYER_NOT_SET) {
      return null;
    }

    Stack found = null;
    for (final GamePiece p : map.getAllPieces()) {
      if (p instanceof Stack && !(p instanceof Deck)) {
        final Stack s = (Stack) p;
        if (s.isEmpty()
            && s.getId() != null
            && s.getLayer() == layer
            && s.getPosition().equals(pos)
            && (found == null || s.getId().compareTo(found.getId()) < 0)) {
          found = s;
        }
      }
    }
    return found;
  }

  @Override
  protected void executeCommand() {
    final GamePiece piece = GameModule.getGameModule().getGameState().getPieceForId(id);
    if (piece != null) {
      final BoundsTracker bounds = new BoundsTracker();
      bounds.addPiece(piece);
      final Map newMap = Map.getMapById(newMapId);
      if (newMap != null) {
        final PieceVisitorDispatcher mergeFinder = createMergeFinder(newMap, piece, newPosition);
        if (newUnderneathId != null) {
          final GamePiece under = GameModule.getGameModule().getGameState().getPieceForId(newUnderneathId);
          if (under != null
              && under.getPosition().equals(newPosition)
              && under.getMap() == newMap) { //BR// lest someone have simultaneously moved or deleted the piece.
            newMap.getStackMetrics().merge(under, piece);
          }
          else {
            if (newMap.apply(mergeFinder) == null) {
              stackOrPlacePiece(piece, newMap, newPosition);
            }
          }
        }
        else {
          if (newMap.apply(mergeFinder) == null) {
            stackOrPlacePiece(piece, newMap, newPosition);
          }
          if (piece.getParent() != null) {
            piece.getParent().insert(piece, 0);
          }
        }
      }
      else {
        final Map oldMap = Map.getMapById(oldMapId);
        if (oldMap != null) {
          oldMap.removePiece(piece);
        }
      }
      bounds.addPiece(piece);

      // Highlight the stack the piece was moved to
      HighlightLastMoved.setLastMoved(piece);

      bounds.repaint();
      if (piece.getMap() != null
          && GlobalOptions.getInstance().centerOnOpponentsMove()
          && !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
        piece.getMap().ensureVisible(piece.getMap().selectionBoundsOf(piece));
      }
    }
  }

  @Override
  protected Command myUndoCommand() {
    return new MovePiece(id, oldMapId, oldPosition, oldUnderneathId, newMapId, newPosition, newUnderneathId, playerId);
  }

  /**
   * Creates a new {@link PieceVisitorDispatcher} that will create a {@link Command} object
   * to merge the target piece with any applicable pieces at the target location
   * @param map
   * @param p
   * @param pt
   * @return
   */
  protected PieceVisitorDispatcher createMergeFinder(final Map map, final GamePiece p, final Point pt) {
    return new DeckVisitorDispatcher(new DeckVisitor() {
      @Override
      public Object visitDeck(Deck d) {
        if (d.getPosition().equals(pt)) {
          return map.getStackMetrics().merge(d, p);
        }
        else {
          return null;
        }
      }

      @Override
      public Object visitStack(Stack s) {
        if (s.getPosition().equals(pt)
            && map.getStackMetrics().isStackingEnabled()
            && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
            && s.topPiece(playerId) != null  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
            && map.getPieceCollection().canMerge(p, s)) {
          return map.getStackMetrics().merge(s, p);
        }
        else {
          return null;
        }
      }

      @Override
      public Object visitDefault(GamePiece piece) {
        if (piece.getPosition().equals(pt)
            && map.getStackMetrics().isStackingEnabled()
            && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK))
            && map.getPieceCollection().canMerge(p, piece)) {
          final String hiddenBy = (String) piece.getProperty(Properties.HIDDEN_BY);
          if (hiddenBy == null
              || hiddenBy.equals(playerId)) {
            return map.getStackMetrics().merge(piece, p);
          }
          else {
            return null;
          }
        }
        else {
          return null;
        }
      }
    });
  }

  @Override
  public String getDetails() {
    return "id=" + id + ",map=" + newMapId + ",position=" + newPosition + ",under=" + newUnderneathId; //NON-NLS
  }
}
