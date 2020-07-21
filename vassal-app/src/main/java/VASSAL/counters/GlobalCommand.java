/*
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

import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;

/**
 * Applies a given keyboard command to all counters on a map
 */
public class GlobalCommand {
  protected KeyStroke keyStroke;
  protected boolean reportSingle;
  protected int selectFromDeck = -1;
  protected FormattedString reportFormat = new FormattedString();
  protected Loopable owner;
  protected PropertySource source;

  public GlobalCommand(Loopable l) {
    this (l, null);
  }

  public GlobalCommand(Loopable l, PropertySource p) {
    owner = l;
    source = p;
  }

  public void setPropertySource(PropertySource ps) {
    source = ps;
  }

  public void setKeyStroke(KeyStroke keyStroke) {
    this.keyStroke = keyStroke;
  }

  public void setKeyStroke(NamedKeyStroke keyStroke) {
    this.keyStroke = keyStroke.getKeyStroke();
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
    Command c = new NullCommand();
    try {
      if (reportSingle) {
        Map.setChangeReportingEnabled(false);
      }
      RecursionLimiter.startExecution(owner);
      String reportText = reportFormat.getLocalizedText(source);
      if (reportText.length() > 0) {
        c = new Chatter.DisplayText(
          GameModule.getGameModule().getChatter(), "*" + reportText);
        c.execute();
      }
      for (Map map : m) {
        Visitor visitor = new Visitor(c, filter, keyStroke);
        DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);
        GamePiece[] p = map.getPieces();
        for (GamePiece gamePiece : p) {
          dispatcher.accept(gamePiece);
        }
        visitor.getTracker().repaint();
        c = visitor.getCommand();
      }
    }
    catch (RecursionLimitException e) {
      RecursionLimiter.infiniteLoop(e);
    }
    finally {
      RecursionLimiter.endExecution();
      if (reportSingle) {
        Map.setChangeReportingEnabled(true);
      }
    }

    return c;
  }

  protected class Visitor implements DeckVisitor {
    private Command command;
    private BoundsTracker tracker;
    private PieceFilter filter;
    private KeyStroke stroke;
    private int selectedCount;

    public Visitor(Command command, PieceFilter filter, KeyStroke stroke) {
      this.command = command;
      tracker = new BoundsTracker();
      this.filter = filter;
      this.stroke = stroke;
    }

    @Override
    public Object visitDeck(Deck d) {
      Object target = null;
      if (getSelectFromDeck() != 0) {
        
        // selectFromDeck = -1 means process all cards in Deck
        // selectFromDeck > 0 means select that many cards from the Deck
        
        // Ask for all cards to be drawn.        
        d.setDragCount(d.getPieceCount());
        
        // Keep drawing until required select count met or all cards in Deck have been processed
        selectedCount = 0;
        for (PieceIterator it = d.drawCards(); it.hasMoreElements() && (getSelectFromDeck() < 0 || getSelectFromDeck() > selectedCount);) {
          apply(it.nextPiece());
        }
      }
      return target;
    }

    @Override
    public Object visitStack(Stack s) {
      s.asList().forEach(this::apply);
      return null;
    }

    @Override
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
        selectedCount++;
      }
    }

    public Command getCommand() {
      return command;
    }

    public BoundsTracker getTracker() {
      return tracker;
    }

  }

  public int getSelectFromDeck() {
    return selectFromDeck;
  }

  /**
   * Set the number of pieces to select from a deck that the command will apply to.  A value <0 means to apply to all pieces in the deck
   * @param selectFromDeck
   */
  public void setSelectFromDeck(int selectFromDeck) {
    this.selectFromDeck = selectFromDeck;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((keyStroke == null) ? 0 : keyStroke.hashCode());
    result = prime * result
      + ((reportFormat == null) ? 0 : reportFormat.hashCode());
    result = prime * result + (reportSingle ? 1231 : 1237);
    result = prime * result + selectFromDeck;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    GlobalCommand other = (GlobalCommand) obj;
    if (keyStroke == null) {
      if (other.keyStroke != null)
        return false;
    }
    else if (!keyStroke.equals(other.keyStroke))
      return false;
    if (reportFormat == null) {
      if (other.reportFormat != null)
        return false;
    }
    else if (!reportFormat.equals(other.reportFormat))
      return false;
    if (reportSingle != other.reportSingle)
      return false;
    if (selectFromDeck != other.selectFromDeck)
      return false;
    return true;
  }

}
