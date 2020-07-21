/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.MovePiece;
import VASSAL.command.NullCommand;
import VASSAL.counters.GamePiece;
import VASSAL.counters.MovementMarkable;
import VASSAL.counters.PieceAccess;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.tools.FormattedString;

/**
 * Builds an auto-report message for a collection of Move Commands
 */
public class MovementReporter {
  protected FormattedString format = new FormattedString();

  protected List<MoveSummary> movesToReport = new ArrayList<>();
  protected List<MoveSummary> movesToMark = new ArrayList<>();

  public MovementReporter(Command moveCommand) {
    extractMoveCommands(moveCommand);
  }

  protected void extractMoveCommands(Command c) {
    MoveSummary summary = null;
    if (c instanceof AddPiece) {
      AddPiece addPiece = ((AddPiece) c);
      if (shouldReport(addPiece)) {
        summary = createMoveSummary(addPiece);
      }
    }
    else if (c instanceof MovePiece) {
      MovePiece movePiece = (MovePiece) c;
      if (shouldReport(movePiece)) {
        summary = createMoveSummary(movePiece);
      }
    }
    if (summary != null) {
      // Do we already have an instance that represents movement
      // between the same two map positions on the same two maps?
      int index = movesToReport.indexOf(summary);
      if (index >= 0
          && c instanceof MovePiece
          && shouldReport((MovePiece) c)) {
        MoveSummary existing = movesToReport.get(index);
        existing.append((MovePiece) c);
      }
      else {
        movesToReport.add(summary);
      }
      if (shouldMarkMoved(summary)) {
        movesToMark.add(summary);
      }
    }
    Command[] sub = c.getSubCommands();
    for (Command command : sub) {
      extractMoveCommands(command);
    }
  }

  protected MoveSummary createMoveSummary(AddPiece c) {
    return new MoveSummary(c);
  }

  protected MoveSummary createMoveSummary(MovePiece c) {
    return new MoveSummary(c);
  }


  /**
   * Mark all pieces with the {@link MovementMarkable} trait
   * @return the equivalent Command
   */
  public Command markMovedPieces() {
    Command c = null;
    if (!movesToMark.isEmpty()) {
      c = new NullCommand();
      for (MoveSummary ms : movesToMark) {
        for (GamePiece p : ms.pieces) {
          c.append(markMoved(p));
        }
      }
    }
    return c;
  }

  public Command markMoved(GamePiece p) {
    Command c = null;
    if (p instanceof Stack) {
      c = new NullCommand();
      for (GamePiece gp : ((Stack)p).asList()) {
        c = c.append(markMoved(gp));
      }
    }
    else if (p.getProperty(Properties.MOVED) != null) {
      if (p.getId() != null) {
        ChangeTracker comm = new ChangeTracker(p);
        p.setProperty(Properties.MOVED, Boolean.TRUE);
        c = comm.getChangeCommand();
      }
    }
    return c;
  }


  protected boolean shouldMarkMoved(MoveSummary summary) {
    String option = Map.getMapById(summary.getNewMapId()).getAttributeValueString(Map.MARK_MOVED);
    if (option == null) {
      option = GlobalOptions.getInstance().getAttributeValueString(GlobalOptions.MARK_MOVED);
    }
    if (GlobalOptions.ALWAYS.equals(option)) {
      return true;
    }
    else if (GlobalOptions.NEVER.equals(option)) {
      return false;
    }
    else {
      return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(Map.MARK_MOVED));
    }
  }

  protected boolean shouldReport(AddPiece addPiece) {
    GamePiece target = addPiece.getTarget();
    if (target != null
        && !(target instanceof Stack)
        && !Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_ME))
        && !Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
      return true;
    }
    else {
      return false;
    }
  }

  protected boolean shouldReport(MovePiece movePiece) {
    GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(movePiece.getId());
    if (target == null) {
      return false;
    }
    if (target instanceof Stack) {
      GamePiece top = ((Stack) target).topPiece(null);
      return top != null;
    }
    else {
      return !Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_ME))
          && !Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_OTHERS));
    }
  }

  public Command getReportCommand() {
    PieceAccess.GlobalAccess.hideAll();

    Command c = new NullCommand();
    for (MoveSummary ms : movesToReport) {
      Map fromMap = Map.getMapById(ms.getOldMapId());
      Map toMap = Map.getMapById(ms.getNewMapId());
      format.clearProperties();
      if (fromMap == null) {
        format.setFormat(toMap.getCreateFormat());
      }
      else if (fromMap != toMap) {
        format.setFormat(toMap.getMoveToFormat());
      }
      else {
        format.setFormat(toMap.getMoveWithinFormat());
      }
      if (format.getFormat().length() == 0) {
        break;
      }
      format.setProperty(Map.PIECE_NAME, ms.getPieceName());
      format.setProperty(Map.LOCATION, getLocation(toMap, ms.getNewPosition()));
      if (fromMap != null) {
        format.setProperty(Map.OLD_MAP, fromMap.getLocalizedConfigureName());
        format.setProperty(Map.OLD_LOCATION, getLocation(fromMap, ms.getOldPosition()));
      }
      format.setProperty(Map.MAP_NAME, toMap.getLocalizedConfigureName());

      String moveText = format.getLocalizedText();

      if (moveText.length() > 0) {
        c = c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + moveText));
      }
    }
    PieceAccess.GlobalAccess.revertAll();
    return c;
  }

  protected String getLocation(Map map, Point p) {
    return map.localizedLocationName(p);
  }

  /**
   * A version of the MovementReporter for reporting the movement of
   * Invisible pieces. Replace the locations with '?'
   */
  public static class HiddenMovementReporter extends MovementReporter {

    public HiddenMovementReporter(Command moveCommand) {
      super(moveCommand);
    }

    @Override
    protected MoveSummary createMoveSummary(AddPiece c) {
      return new HiddenMoveSummary(c);
    }

    @Override
    protected MoveSummary createMoveSummary(MovePiece c) {
      return new HiddenMoveSummary(c);
    }

    @Override
    protected boolean shouldReport(AddPiece addPiece) {
      GamePiece target = addPiece.getTarget();
      if (target != null
          && !(target instanceof Stack)
          && (Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_ME))
              || Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_OTHERS)))) {
        return true;
      }
      else {
        return false;
      }
    }

    @Override
    protected boolean shouldReport(MovePiece movePiece) {
      GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(movePiece.getId());
      if (target == null) {
        return false;
      }
      if (target instanceof Stack) {
        final Predicate<GamePiece> gamePiecePredicate =
          piece -> Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
            || Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_OTHERS));
        return ((Stack) target).asList().stream().anyMatch(gamePiecePredicate);
      }
      else {
        return Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_ME))
            || Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_OTHERS));
      }
    }

    @Override
    protected String getLocation(Map map, Point p) {
      return "?";
    }

  }

  public static class MoveSummary {
    protected String oldMapId, newMapId;
    protected Point oldPosition, newPosition;
    protected List<GamePiece> pieces = new ArrayList<>();

    public MoveSummary(AddPiece c) {
      GamePiece target = c.getTarget();
      newMapId = target.getMap().getIdentifier();
      newPosition = target.getPosition();
      pieces.add(target);
    }

    public MoveSummary(MovePiece c) {
      GamePiece target = GameModule.getGameModule()
                                   .getGameState().getPieceForId(c.getId());
      oldMapId = c.getOldMapId();
      newMapId = c.getNewMapId();
      oldPosition = c.getOldPosition();
      newPosition = c.getNewPosition();
      if (target != null) {
        pieces.add(target);
      }
    }

    public String getNewMapId() {
      return newMapId;
    }

    public Point getNewPosition() {
      return newPosition;
    }

    public String getOldMapId() {
      return oldMapId;
    }

    public Point getOldPosition() {
      return oldPosition;
    }

    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof MoveSummary)) return false;

      final MoveSummary moveSummary = (MoveSummary) o;

      if (!newPosition.equals(moveSummary.newPosition)) return false;
      if (!newMapId.equals(moveSummary.newMapId)) return false;
      if (oldMapId != null ? !oldMapId.equals(moveSummary.oldMapId) : moveSummary.oldMapId != null) return false;
      if (oldMapId != null) {
        // If there is no old map, then ignore the old position for equals() purposes.
        if (oldPosition != null ? !oldPosition.equals(moveSummary.oldPosition) : moveSummary.oldPosition != null) return false;
      }

      return true;
    }

    public int hashCode() {
      int result;
      result = (oldMapId != null ? oldMapId.hashCode() : 0);
      result = 29 * result + newMapId.hashCode();
      result = 29 * result + newPosition.hashCode();
      if (oldMapId != null) {
        result = 29 * result + (oldPosition != null ? oldPosition.hashCode() : 0);
      }
      return result;
    }

    public void append(MovePiece movePiece) {
      GamePiece target = GameModule.getGameModule()
                                   .getGameState()
                                   .getPieceForId(movePiece.getId());
      if (target != null && !pieces.contains(target)) {
        pieces.add(target);
      }
    }

    public String getPieceName() {
      final StringBuilder names = new StringBuilder();
      for (Iterator<GamePiece> i = pieces.iterator(); i.hasNext(); ) {
        names.append(i.next().getLocalizedName());
        if (i.hasNext()) {
          names.append(", ");
        }
      }
      return names.toString();
    }
  }

  public static class HiddenMoveSummary extends MoveSummary {

    public HiddenMoveSummary(AddPiece c) {
      super(c);
    }

    public HiddenMoveSummary(MovePiece c) {
      super(c);
    }

    @Override
    public String getPieceName() {
      final StringBuilder names = new StringBuilder();
      boolean first = true;
      for (GamePiece piece : pieces) {
        if (piece instanceof Stack) {
          for (GamePiece p : ((Stack) piece).asList()) {
            if (isInvisible(p)) {
              if (!first) {
                names.append(", ");
              }
              names.append("?");
              first = false;
            }
          }
        }
        else {
          if (isInvisible(piece)) {
            if (!first) {
              names.append(", ");
            }
            names.append("?");
            first = false;
          }
        }
      }
      return names.toString();
    }

    protected boolean isInvisible(GamePiece piece) {
      return Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
        || Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_OTHERS));
    }

  }
}
