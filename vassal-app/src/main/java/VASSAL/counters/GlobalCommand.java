/*
 *
 * Copyright (c) 2005-2020 by Rodney Kinney, Brian Reynolds
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
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;

import java.awt.Point;
import java.util.List;

/**
 * The heart of all the different forms of Global Key Command, GlobalCommand handles sending a key command to
 * multiple pieces, potentially on multiple maps, as long as they match its filtering condition.
 *
 * The various forms of Global Key Command that use GlobalCommand are:
 * {@link VASSAL.build.module.GlobalKeyCommand} - Global Key Commands from a Module window
 * {@link VASSAL.build.module.StartupGlobalKeyCommand} - Startup Global Key Commands from a Module
 * {@link VASSAL.build.module.map.MassKeyCommand} - Global Key Commands from a specific Map window
 * {@link VASSAL.build.module.map.DeckGlobalKeyCommand} - Global Key Commands from a Deck
 * {@link CounterGlobalKeyCommand} - Global Key Command from a Game Piece
 */
public class GlobalCommand {
  protected KeyStroke keyStroke;        // Key Command we will issue
  protected boolean reportSingle;       // If true, we temporarily disable Report traits in any receiving pieces
  protected int selectFromDeck = -1;    // selectFromDeck = -1 means process all cards in Deck; > 0 means select that many cards from the Deck
  protected FormattedString reportFormat = new FormattedString(); // Report to display before sending the command
  protected Loopable owner;             // For preventing infinite loops
  protected PropertySource source;      // For resolving properties (i.e. for our report message)
  protected GlobalCommandTarget target; // This holds all of the "Fast Match" information

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

  public void setTarget(GlobalCommandTarget target) {
    this.target = target;
  }

  public GlobalCommandTarget getTarget() {
    return target;
  }

  public Command apply(Map m, PieceFilter filter) {
    return apply(new Map[]{m}, filter);
  }

  /**
   * Check the Property Fast Match for a given gamePiece
   * @param gamePiece the game piece
   * @return true if piece matches
   */
  private boolean passesPropertyFastMatch(GamePiece gamePiece) {
    if (!target.fastMatchProperty || target.targetProperty.isEmpty()) return true;
    String value = (String) gamePiece.getProperty(target.targetProperty);
    return value.equals(target.targetValue);
  }

  /**
   * Apply the key command to all pieces that pass the given filter & our Fast Match {@link GlobalCommandTarget} parameters on all the given maps
   *
   * @param m Array of Maps
   * @param filter Filter to apply
   * @return a the corresponding {@link Command}
   */
  public Command apply(Map[] m, PieceFilter filter) {
    Command c = new NullCommand();

    try {
      if (reportSingle) {
        Map.setChangeReportingEnabled(false); // Disable individual reports, if specified
      }

      RecursionLimiter.startExecution(owner); // Trap infinite loops of Global Key Commands

      // Send our report, if one is specified
      String reportText = reportFormat.getLocalizedText(source);
      if (reportText.length() > 0) {
        c = new Chatter.DisplayText(
          GameModule.getGameModule().getChatter(), "*" + reportText); //NON-NLS
        c.execute();
      }

      // If we're basic comparison on an existing piece, preload our comparison property for better performance.
      GamePiece curPiece = target.getCurPiece();
      String compareString = "";
      if (target.fastMatchLocation && target.targetType.isCurrent() && (curPiece != null)) {
        switch (target.targetType) {
        case CURZONE:
          compareString = (String) curPiece.getProperty(BasicPiece.CURRENT_ZONE);
          break;
        case CURLOC:
          compareString = (String) curPiece.getProperty(BasicPiece.LOCATION_NAME);
          break;
        }
      }

      // This dispatcher will eventually handle applying the Beanshell filter and actually issuing the command to any pieces that match
      Visitor visitor = new Visitor(c, filter, keyStroke);
      DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);

      // If we're using "current stack or deck" then we simply iterate quickly through the members of the stack or deck that the current piece is in
      if (target.fastMatchLocation && target.targetType == GlobalCommandTarget.Target.CURSTACK) {
        if (curPiece != null) {
          Stack stack = curPiece.getParent();
          List<GamePiece> pieces = stack.asList();
          for (GamePiece gamePiece : pieces) {
            // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
            if (!passesPropertyFastMatch(gamePiece)) {
              continue;
            }

            // Anything else we send to dispatcher to apply BeanShell filter and issue the command if the piece matches
            dispatcher.accept(gamePiece);
          }
        }
      }
      // If we're using "specific deck", then we find that deck and iterate through it, checking fast property matches only
      else if (target.fastMatchLocation && target.targetType == GlobalCommandTarget.Target.DECK) {
        DrawPile d = DrawPile.findDrawPile(target.targetDeck);
        if (d != null) {
          List<GamePiece> pieces = d.getDeck().asList();
          for (GamePiece gamePiece : pieces) {
            // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
            // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
            if (!passesPropertyFastMatch(gamePiece)) {
              continue;
            }

            // Anything else we send to dispatcher to apply BeanShell filter and issue the command if the piece matches
            dispatcher.accept(gamePiece);
          }
        }
      }
      else {
        // For most Global Key Commands we need to run through the larger lists of maps & pieces. Hopefully the Fast Matches
        // here will filter some of that out.
        for (Map map : m) {
          // First check that this is a map we're even interested in
          if (target.fastMatchLocation) {
            // "Current Map" only cares about the map the issuing piece is on
            if (target.targetType == GlobalCommandTarget.Target.CURMAP) {
              if ((curPiece != null) && !map.equals(curPiece.getMap())) {
                continue;
              }
            }
            // If a Fast Match Map is specified, only check that one.
            else if (!target.targetType.isCurrent() && !target.targetMap.isEmpty() && !target.targetMap.equals(map.getConfigureName())) {
              continue;
            }
          }

          // Now we go through all the pieces on this map
          GamePiece[] p = map.getPieces();
          for (GamePiece gamePiece : p) {
            // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
            if (!passesPropertyFastMatch(gamePiece)) {
              continue;
            }

            // These basic location filters are faster than equivalent filters in the Beanshell expression
            // If this is a stack, the top piece in the stack's current properties will be the same as any other piece in the stack.
            if ((target.targetType == GlobalCommandTarget.Target.ZONE) || (target.targetType == GlobalCommandTarget.Target.LOCATION) ||
              (target.targetType == GlobalCommandTarget.Target.CURZONE) || (target.targetType == GlobalCommandTarget.Target.CURLOC)) {
              GamePiece pp;
              if (gamePiece instanceof Stack) {
                Stack s;
                s = (Stack) gamePiece;
                pp = s.topPiece();
                if (pp == null) {
                  continue;
                }
              }
              else {
                pp = gamePiece;
              }

              // Fast matches for Zone / Location
              switch (target.targetType) {
              case ZONE:
                if (!target.targetZone.equals(pp.getProperty(BasicPiece.CURRENT_ZONE))) {
                  continue;
                }
                break;
              case LOCATION:
                if (!target.targetLocation.equals(pp.getProperty(BasicPiece.LOCATION_NAME))) {
                  continue;
                }
                break;
              case CURZONE:
                if (!compareString.equals(pp.getProperty(BasicPiece.CURRENT_ZONE))) {
                  continue;
                }
                break;
              case CURLOC:
                if (!compareString.equals(pp.getProperty(BasicPiece.LOCATION_NAME))) {
                  continue;
                }
                break;
              }
            }

            // Fast Match of "exact XY position"
            if (target.targetType == GlobalCommandTarget.Target.XY) {
              if (!target.targetBoard.equals(gamePiece.getProperty(BasicPiece.CURRENT_BOARD))) {
                continue;
              }
              Point pt = new Point(gamePiece.getPosition());
              if ((target.targetX != pt.getX()) || (target.targetY != pt.getY())) {
                continue;
              }
            }

            // Passed all the "Fast Match" tests -- the dispatcher will apply the BeanShell filter and if that passes will issue the command to the piece
            dispatcher.accept(gamePiece);
          }
        }
      }
      visitor.getTracker().repaint();
      c = visitor.getCommand();
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
    private final Command command;
    private final BoundsTracker tracker;
    private final PieceFilter filter;
    private final KeyStroke stroke;
    private int selectedCount;

    public Visitor(Command command, PieceFilter filter, KeyStroke stroke) {
      this.command = command;
      tracker = new BoundsTracker();
      this.filter = filter;
      this.stroke = stroke;
    }

    @Override
    public Object visitDeck(Deck d) {
      if (getSelectFromDeck() != 0) {
        
        // selectFromDeck = -1 means process all cards in Deck
        // selectFromDeck > 0 means select that many cards from the Deck
        
        // Ask for all cards to be drawn.        
        d.setDragCount(d.getPieceCount());
        
        // Keep drawing until required select count met or all cards in Deck have been processed
        selectedCount = 0;
        for (PieceIterator it = d.drawCards(); it.hasMoreElements() && (getSelectFromDeck() < 0 || getSelectFromDeck() > selectedCount);) {
          apply(it.nextPiece(), true);
        }
      }
      return null;
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
      apply(p, false);
    }

    private void apply(GamePiece p, boolean visitingDeck) {
      if (filter == null || filter.accept(p)) {
        if (visitingDeck) {
          p.setProperty(Properties.OBSCURED_BY, p.getProperty(Properties.OBSCURED_BY_PRE_DRAW));  // Bug 13433 restore correct OBSCURED_BY after checking filter
        }
        tracker.addPiece(p);
        p.setProperty(Properties.SNAPSHOT, ((PropertyExporter) p).getProperties());
        command.append(p.keyEvent(stroke));
        tracker.addPiece(p);
        selectedCount++;
      }
      else {
        if (visitingDeck) {
          p.setProperty(Properties.OBSCURED_BY, p.getProperty(Properties.OBSCURED_BY_PRE_DRAW));  // Bug 13433 restore correct OBSCURED_BY
        }
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
   * @param selectFromDeck Number of pieces to select
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

    // Match any specific targeting information, depending on the targeting type. targetType must always match.
    if (target.fastMatchLocation != other.target.fastMatchLocation) {
      return false;
    }
    if (target.targetType != other.target.targetType) {
      return false;
    }
    if (!target.targetType.isCurrent() && !target.targetMap.equals(other.target.targetMap)) {
      return false;
    }
    if ((target.targetType == GlobalCommandTarget.Target.ZONE) && !target.targetZone.equals(other.target.targetZone)) {
      return false;
    }
    if ((target.targetType == GlobalCommandTarget.Target.LOCATION) && !target.targetLocation.equals(other.target.targetLocation)) {
      return false;
    }
    if ((target.targetType == GlobalCommandTarget.Target.XY) && (!target.targetBoard.equals(other.target.targetBoard) || ((target.targetX != other.target.targetX) || (target.targetY != other.target.targetY)))) {
      return false;
    }

    if (target.fastMatchProperty != other.target.fastMatchProperty) {
      return false;
    }

    if (target.fastMatchProperty) {
      if (!target.targetProperty.equals(other.target.targetProperty)) {
        return false;
      }
      if (!target.targetValue.equals(other.target.targetValue)) {
        return false;
      }
    }

    return selectFromDeck == other.selectFromDeck;
  }
}
