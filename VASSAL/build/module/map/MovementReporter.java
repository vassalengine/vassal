/*
 * $Id$
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
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

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
  private FormattedString format = new FormattedString();

  private List movesToReport = new ArrayList();
  private List movesToMark = new ArrayList();

  public MovementReporter(Command moveCommand) {
    extractMoveCommands(moveCommand);
  }

  private void extractMoveCommands(Command c) {
    MoveSummary summary = null;
    if (c instanceof AddPiece) {
      AddPiece addPiece = ((AddPiece) c);
      if (shouldReport(addPiece)) {
        summary = new MoveSummary(addPiece);
      }
    }
    else if (c instanceof MovePiece) {
      MovePiece movePiece = (MovePiece) c;
      if (shouldReport(movePiece)) {
        summary = new MoveSummary(movePiece);
      }
    }
    if (summary != null) {
      // Do we already have an instance that represents movement
      // between the same two map positions on the same two maps?
      int index = movesToReport.indexOf(summary);
      if (index >= 0
          && c instanceof MovePiece
          && shouldReport((MovePiece) c)) {
        MoveSummary existing = (MoveSummary) movesToReport.get(index);
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
    for (int i = 0; i < sub.length; i++) {
      extractMoveCommands(sub[i]);
    }
  }

  /**
   * Mark all pieces with the {@link MovementMarkable} trait
   * @return the equivalent Command
   */
  public Command markMovedPieces() {
    Command c = null;
    if (!movesToMark.isEmpty()) {
      c = new NullCommand();
      for (Iterator it = movesToMark.iterator(); it.hasNext();) {
        MoveSummary moveSummary = (MoveSummary) it.next();
        for (Iterator iterator = moveSummary.pieces.iterator(); iterator.hasNext();) {
          GamePiece p = (GamePiece) iterator.next();
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
      for (Enumeration e = ((Stack) p).getPieces(); e.hasMoreElements();) {
        c.append(markMoved((GamePiece) e.nextElement()));
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
    for (Iterator it = movesToReport.iterator(); it.hasNext();) {
      MoveSummary ms = (MoveSummary) it.next();
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
      format.setProperty(Map.LOCATION, toMap.locationName(ms.getNewPosition()));
      if (fromMap != null) {
        format.setProperty(Map.OLD_MAP, fromMap.getConfigureName());
        format.setProperty(Map.OLD_LOCATION, fromMap.locationName(ms.getOldPosition()));
      }
      format.setProperty(Map.MAP_NAME, toMap.getConfigureName());

      String moveText = format.getText();

      if (moveText.length() > 0) {
        c = c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + moveText));
      }
    }
    PieceAccess.GlobalAccess.revertAll();
    return c;
  }

  public static class MoveSummary {
    private String oldMapId, newMapId;
    private Point oldPosition, newPosition;
    private List pieces = new ArrayList();

    public MoveSummary(AddPiece c) {
      GamePiece target = c.getTarget();
      newMapId = target.getMap().getIdentifier();
      newPosition = target.getPosition();
      pieces.add(target);
    }

    public MoveSummary(MovePiece c) {
      GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(c.getId());
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
      GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(movePiece.getId());
      if (target != null
        && !pieces.contains(target)) {
        pieces.add(target);
      }
    }

    public String getPieceName() {
      StringBuffer names = new StringBuffer();
      for (Iterator it = pieces.iterator(); it.hasNext();) {
        GamePiece gamePiece = (GamePiece) it.next();
        names.append(gamePiece.getName());
        if (it.hasNext()) {
          names.append(", ");
        }
      }
      return names.toString();
    }
  }
}
