/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
package VASSAL.counters;

import java.util.Enumeration;

import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.tools.FormattedString;

/**
 * Applies a given keyboard command to all counters on a map
 */
public class GlobalCommand {
  private KeyStroke keyStroke;
  private boolean reportSingle;
  private FormattedString reportFormat = new FormattedString();

  public void setKeyStroke(KeyStroke keyStroke) {
    this.keyStroke = keyStroke;
  }

  public void setReportFormat(String format) {
    this.reportFormat.setFormat(format);
  }

  public KeyStroke getKeyStroke() {
    return keyStroke;
  }

  public String getReportFormat() {
    return reportFormat.getFormat();
  }

  public boolean isReportSingle() {
    return reportSingle;
  }

  public void setReportSingle(boolean reportSingle) {
    this.reportSingle = reportSingle;
  }

  public Command apply(Map m, PieceFilter filter) {
    return apply(new Map[]{m},filter);
  }
  /**
   * Apply the key command to all pieces that pass the given filter on all the given maps
   * 
   * @param m
   * @param filter
   * @return a the corresponding {@link Command}
   */
  public Command apply(Map[] m, PieceFilter filter) {
    String reportText = reportFormat.getText();
    Command c;
    if (reportText.length() > 0) {
      c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "*" + reportText);
      c.execute();
    }
    else {
      c = new NullCommand();
    }
    for (int mapI = 0; mapI < m.length; ++mapI) {
      String mapFormat = m[mapI].getChangeFormat();
      if (reportSingle) {
        m[mapI].setAttribute(Map.CHANGE_FORMAT, "");
      }
      Visitor visitor = new Visitor(c, filter, keyStroke);
      PieceVisitorDispatcher dispatcher = new PieceVisitorDispatcher(visitor);
      GamePiece[] p = m[mapI].getPieces();
      for (int i = 0; i < p.length; ++i) {
        dispatcher.accept(p[i]);
      }
      visitor.getTracker().repaint();
      if (reportSingle) {
        m[mapI].setAttribute(Map.CHANGE_FORMAT, mapFormat);
      }
      c = visitor.getCommand();
    }
    return c;
  }

  /*
   * We don't treat {@link Deck}s any differently than {@link Stack}s, so no
   * need to implement {@link DeckVisitor 
   */
  private static class Visitor implements PieceVisitor {
    private Command command;
    private BoundsTracker tracker;
    private PieceFilter filter;
    private KeyStroke stroke;

    public Visitor(Command command, PieceFilter filter, KeyStroke stroke) {
      this.command = command;
      tracker = new BoundsTracker();
      this.filter = filter;
      this.stroke = stroke;
    }

    public Object visitStack(Stack s) {
      for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
        apply((GamePiece) e.nextElement());
      }
      return null;
    }

    public Object visitDefault(GamePiece p) {
      apply(p);
      return null;
    }

    private void apply(GamePiece p) {
      if (filter == null || filter.accept(p)) {
        tracker.addPiece(p);
        p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
        command.append(p.keyEvent(stroke));
        tracker.addPiece(p);
      }
    }

    public Command getCommand() {
      return command;
    }

    public BoundsTracker getTracker() {
      return tracker;
    }

  }
}
