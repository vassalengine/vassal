package VASSAL.command;

import java.awt.Point;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.HighlightLastMoved;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

/*
 * $Id$
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
  private String id;
  private String newMapId;
  private String oldMapId;
  private Point newPosition;
  private Point oldPosition;
  private String newUnderneathId;
  private String oldUnderneathId;
  private String playerId;

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

  protected void executeCommand() {
    GamePiece piece = GameModule.getGameModule().getGameState().getPieceForId(id);
    if (piece != null) {
      BoundsTracker bounds = new BoundsTracker();
      bounds.addPiece(piece);
      Map newMap = Map.getMapById(newMapId);
      if (newMap != null) {
        PieceVisitorDispatcher mergeFinder = createMergeFinder(newMap, piece, newPosition);
        if (newUnderneathId != null) {
          GamePiece under = GameModule.getGameModule().getGameState().getPieceForId(newUnderneathId);
          if (under != null
              && under.getPosition().equals(newPosition)) {
            newMap.getStackMetrics().merge(under, piece);
          }
          else {
            if (newMap.apply(mergeFinder) == null) {
              newMap.placeAt(piece, newPosition);
            }
          }
        }
        else {
          if (newMap.apply(mergeFinder) == null) {
            newMap.placeAt(piece, newPosition);
          }
          if (piece.getParent() != null) {
            piece.getParent().insert(piece, 0);
          }
        }
      }
      else {
        Map oldMap = Map.getMapById(oldMapId);
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
    PieceVisitorDispatcher dispatch = new DeckVisitorDispatcher(new DeckVisitor() {
      public Object visitDeck(Deck d) {
        if (d.getPosition().equals(pt)) {
          return map.getStackMetrics().merge(d, p);
        }
        else {
          return null;
        }
      }

      public Object visitStack(Stack s) {
        if (s.getPosition().equals(pt)
            && map.getStackMetrics().isStackingEnabled()
            && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
            && s.topPiece(playerId) != null
            && map.getPieceCollection().canMerge(p,s)) {
          return map.getStackMetrics().merge(s, p);
        }
        else {
          return null;
        }
      }

      public Object visitDefault(GamePiece piece) {
        if (piece.getPosition().equals(pt)
            && map.getStackMetrics().isStackingEnabled()
            && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK))
            && map.getPieceCollection().canMerge(p,piece)) {
          String hiddenBy = (String) piece.getProperty(Properties.HIDDEN_BY);
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
    return dispatch;
  }
  public String getDetails() {
    return "id="+id+",map="+newMapId+",position="+newPosition+",under="+newUnderneathId;
  }

}
