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
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;

import java.awt.Point;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

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
 *
 * Other important classes:
 * {@link GlobalCommandTarget}           - "Fast Match" parameters
 * {@link GlobalCommandTargetConfigurer} - configurer for "Fast Match" parameters
 */
public class GlobalCommand {
  protected KeyStroke keyStroke;        // Key Command we will issue
  protected boolean reportSingle;       // If true, we temporarily disable Report traits in any receiving pieces
  protected int selectFromDeck = -1;    // selectFromDeck = -1 means process all cards in Deck; > 0 means select that many cards from the Deck
  protected FormattedString reportFormat = new FormattedString(); // Report to display before sending the command
  protected Loopable owner;             // For preventing infinite loops
  protected PropertySource source;      // Context for resolving properties (i.e. for our report message)
  protected GlobalCommandTarget target; // This holds all of the "Fast Match" information

  private String fastProperty = "";     // Used during property Fast Match to hold *evaluated* expressions
  private String fastValue = "";        // Used during property Fast Match to hold *evaluated* expressions
  private boolean fastIsNumber = false; // Used during property Fast Match to remember if value is numeric
  private double fastNumber = 0;        // Used during property Fast Match to hold evaluated numerical value
  private Pattern fastPattern;          // Fast Match regex pattern

  private static final Pattern fastCheckNumber = Pattern.compile("(\\+-)?\\d+(\\.\\d+)?");  //match a number with optional +/- and decimal.

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

  /**
   * Check the Property Fast Match for a given gamePiece
   * @param gamePiece the game piece
   * @return true if piece matches
   */
  private boolean passesPropertyFastMatch(GamePiece gamePiece) {
    if (!target.fastMatchProperty || fastProperty.isEmpty()) return true;

    final Object prop = gamePiece.getProperty(fastProperty);
    final String value = (prop == null) ? null : prop.toString();

    // Intentionally favors the default "Equals" as first to process
    switch (target.targetCompare) {
    case EQUALS:
      return fastValue.equals(value);
    case NOT_EQUALS:
      return !fastValue.equals(value);
    }

    // The non-equals-y ones have to deal with null
    if (prop == null || value == null) {
      return false;
    }

    switch (target.targetCompare) {
    case MATCH:
      return fastPattern.matcher(value).matches();
    case NOT_MATCH:
      return !fastPattern.matcher(value).matches();
    }

    // Lexical comparisons for strings
    if (!fastIsNumber || !isNumeric(value)) {
      switch (target.targetCompare) {
      case GREATER_EQUALS:
        return value.compareTo(fastValue) >= 0;
      case GREATER:
        return value.compareTo(fastValue) > 0;
      case LESS_EQUALS:
        return value.compareTo(fastValue) <= 0;
      case LESS:
        return value.compareTo(fastValue) < 0;
      }
    }

    // Numerical comparisons for numbers
    final double num = Double.parseDouble(value);

    switch (target.targetCompare) {
    case GREATER_EQUALS:
      return num >= fastNumber;
    case GREATER:
      return num > fastNumber;
    case LESS_EQUALS:
      return num <= fastNumber;
    case LESS:
      return num < fastNumber;
    }

    return false; // Never gets here, but checkStyle doesn't understand that.
  }

  /**
   * Need a super-fast (i.e. not dependent on exception-throwing) plan for detecting valid numbers
   * @param s string to check
   * @return true if a value number
   */
  private boolean isNumeric(String s) {
    return fastCheckNumber.matcher(s).matches(); //match a number with optional +/- and decimal.
  }

  /**
   * Apply the key command to all pieces that pass the given filter & our Fast Match {@link GlobalCommandTarget} parameters on all the given maps
   *
   * @param maps Array of Maps
   * @param filter Filter to apply (created e.g. with {@link PropertyExpression#getFilter}
   * @param fastMatch Fast matching parameters, or null. {@link GlobalCommandTarget} and {@link VASSAL.configure.GlobalCommandTargetConfigurer}
   * @return the corresponding {@link Command} that would reproduce all the things this GKC just did, on another client.
   */
  public Command apply(Map[] maps, PieceFilter filter, GlobalCommandTarget fastMatch) {
    Command command = new NullCommand(); // We will chronicle our exploits in this command, so that others may repeat them later.
    setTarget((fastMatch != null) ? fastMatch : new GlobalCommandTarget()); // Set our Fast Match parameters

    try {
      if (reportSingle) {
        Map.setChangeReportingEnabled(false); // Disable individual reports, if specified
      }

      RecursionLimiter.startExecution(owner); // Trap infinite loops of Global Key Commands

      // Send our report, if one is specified
      final String reportText = reportFormat.getLocalizedText(source);
      if (reportText.length() > 0) {
        command = new Chatter.DisplayText(
          GameModule.getGameModule().getChatter(), "*" + reportText); //NON-NLS
        command.execute();
      }

      // If there actually isn't any key command to execute, we're finished here, having issued the report-if-any.
      if ((keyStroke == null) || ((keyStroke.getKeyCode() == 0) && (keyStroke.getModifiers() == 0))) {
        return command;
      }

      // These will hold the *evaluated results* of our various Fast Match expressions
      String fastMap = "";
      String fastBoard = "";
      String fastZone = "";
      String fastLocation = "";
      String fastDeck = "";
      String fastX = "";
      String fastY = "";

      // Context piece, if we are doing current-piece-relative fast-matching (may be null otherwise)
      final GamePiece curPiece = target.getCurPiece();

      // Evaluate all location-based expressions we will be using - these are evaluated w/r/t the SOURCE of the command, not target pieces.
      if (target.fastMatchLocation) {
        switch (target.targetType) {
        case CURZONE:
          fastZone = (curPiece != null) ? (String) curPiece.getProperty(BasicPiece.CURRENT_ZONE) : "";
          break;
        case CURLOC:
          fastLocation = (curPiece != null) ? (String) curPiece.getProperty(BasicPiece.LOCATION_NAME) : "";
          break;
        case ZONE:
          fastZone = target.targetZone.tryEvaluate(source);
          break;
        case DECK:
          fastDeck = target.targetDeck.tryEvaluate(source);
          break;
        case LOCATION:
          fastLocation = target.targetLocation.tryEvaluate(source);
          break;
        case XY:
          fastBoard = target.targetLocation.tryEvaluate(source);
          fastX = target.targetX.tryEvaluate(source);
          fastY = target.targetY.tryEvaluate(source);
          break;
        }

        if (!target.targetType.isCurrent()) {
          fastMap = target.targetMap.tryEvaluate(source);
        }
      }

      // Evaluate any property-based expressions we will be using - these are evaluated w/r/t the SOURCE of the command, not target pieces.
      if (target.fastMatchProperty) {
        fastProperty = target.targetProperty.tryEvaluate(source);
        fastValue    = target.targetValue.tryEvaluate(source);
        if ((target.targetCompare == GlobalCommandTarget.CompareMode.EQUALS) ||
            (target.targetCompare == GlobalCommandTarget.CompareMode.NOT_EQUALS)) {
          fastIsNumber = false;
          fastNumber   = 0;
        }
        else if ((target.targetCompare == GlobalCommandTarget.CompareMode.MATCH) ||
                 (target.targetCompare == GlobalCommandTarget.CompareMode.NOT_MATCH)) {
          fastPattern  = Pattern.compile(fastValue);
          fastIsNumber = false;
          fastNumber   = 0;
        }
        else {
          fastIsNumber = isNumeric(fastValue);
          fastNumber = fastIsNumber ? Double.parseDouble(fastValue) : 0;
        }
      }

      // This dispatcher will eventually handle applying the Beanshell filter and actually issuing the command to any pieces that match
      final Visitor visitor = new Visitor(command, filter, keyStroke);
      final DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);

      // If we're using "current stack or deck" then we simply iterate quickly through the members of the stack or deck that the current piece is in
      if (target.fastMatchLocation && target.targetType == GlobalCommandTarget.Target.CURSTACK) {
        if (curPiece != null) {
          final Stack stack = curPiece.getParent();
          final int useFromDeck = (stack instanceof Deck) ? getSelectFromDeck() : -1;
          if (stack instanceof Deck) {
            visitor.setSelectedCount(0);
          }
          List<GamePiece> pieces = stack.asList();
          if (stack instanceof Deck) {
            pieces = ((Deck) stack).getOrderedPieces();
          }
          if (useFromDeck != 0) {
            for (final GamePiece gamePiece : pieces) {
              // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
              if (!passesPropertyFastMatch(gamePiece)) {
                continue;
              }

              // Anything else we send to dispatcher to apply BeanShell filter and issue the command if the piece matches
              dispatcher.accept(gamePiece);

              if ((useFromDeck > 0) && visitor.getSelectedCount() >= useFromDeck) {
                break;
              }
            }
          }
        }
      }
      // If we're using "specific deck", then we find that deck and iterate through it, checking fast property matches only
      else if (target.fastMatchLocation && target.targetType == GlobalCommandTarget.Target.DECK) {
        final DrawPile d = DrawPile.findDrawPile(fastDeck);
        final int useFromDeck = getSelectFromDeck();
        if ((d != null) && (useFromDeck != 0)) {
          final List<GamePiece> pieces = d.getDeck().getOrderedPieces();

          visitor.setSelectedCount(0);
          for (final GamePiece gamePiece : pieces) {
            // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
            if (!passesPropertyFastMatch(gamePiece)) {
              continue;
            }

            // Anything else we send to dispatcher to apply BeanShell filter and issue the command if the piece matches
            dispatcher.accept(gamePiece);

            if ((useFromDeck > 0) && visitor.getSelectedCount() >= useFromDeck) {
              break;
            }
          }
        }
      }
      else {
        // For most Global Key Commands we need to run through the larger lists of maps & pieces. Ideally the Fast Matches
        // here will filter some of that out to improve performance, but we also want to do the best job possible for old
        // modules that don't take advantage of Fast Match yet.
        for (final Map map : maps) {
          // First check that this is a map we're even interested in
          if (target.fastMatchLocation) {
            // "Current Map" only cares about the map the issuing piece is on
            if (target.targetType == GlobalCommandTarget.Target.CURMAP) {
              if ((curPiece != null) && !map.equals(curPiece.getMap())) {
                continue;
              }
            }
            // If a Fast Match Map is specified, only check that one.
            else if (!target.targetType.isCurrent() && !fastMap.isEmpty() && !fastMap.equals(map.getConfigureName())) {
              continue;
            }
          }

          // Now we go through all the pieces/stacks/decks on this map
          final GamePiece[] everythingOnMap = map.getPieces();

          if (!target.fastMatchLocation) {
            // If NOT doing Location fast-matching we do tighter loops (because perf is important during GKCs)
            if (!target.fastMatchProperty) {
              // This is the no-fast-matching-at-all version, with "minimum extra overhead" since it's already going to be slow.
              for (final GamePiece pieceOrStack : everythingOnMap) {
                dispatcher.accept(pieceOrStack);
              }
            }
            else {
              // This loop is WITH property Fast Match but WITHOUT location Fast Match
              for (final GamePiece pieceOrStack : everythingOnMap) {
                final List<GamePiece> pieceList;

                // We may have an individual piece, or we may have a Stack (or Deck), in which case we need to traverse it.
                if (pieceOrStack instanceof Deck) {
                  final int useFromDeck = getSelectFromDeck();
                  if (useFromDeck != 0) {
                    visitor.setSelectedCount(0);
                    pieceList = ((Deck) pieceOrStack).getOrderedPieces();

                    // This will iterate through actual game pieces
                    for (final GamePiece gamePiece : pieceList) {
                      // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
                      if (!passesPropertyFastMatch(gamePiece)) {
                        continue;
                      }
                      dispatcher.accept(gamePiece);

                      if ((useFromDeck > 0) && visitor.getSelectedCount() >= useFromDeck) {
                        break;
                      }
                    }
                  }
                }
                else {
                  if (pieceOrStack instanceof Stack) {
                    pieceList = ((Stack) pieceOrStack).asList();
                  }
                  else {
                    pieceList = Collections.singletonList(pieceOrStack); // Or if really just a single piece.
                  }

                  // This will iterate through actual game pieces
                  for (final GamePiece gamePiece : pieceList) {
                    // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
                    if (!passesPropertyFastMatch(gamePiece)) {
                      continue;
                    }
                    dispatcher.accept(gamePiece);
                  }
                }
              }
            }
          }
          else {
            // WITH Location Fast Matching we have some extra steps
            for (final GamePiece pieceOrStack : everythingOnMap) {
              List<GamePiece> pieceList;
              final int useFromDeck;

              // We may have an individual piece, or we may have a Stack (or Deck), in which case we need to traverse it.
              if (pieceOrStack instanceof Stack) {
                if (pieceOrStack instanceof Deck) {
                  useFromDeck = getSelectFromDeck();
                  visitor.setSelectedCount(0);
                }
                else {
                  useFromDeck = -1; // Not a deck, so accept all pieces
                }
                pieceList = ((Stack) pieceOrStack).asList();
                if (pieceOrStack instanceof Deck) {
                  pieceList = ((Deck) pieceOrStack).getOrderedPieces();
                }
              }
              else {
                pieceList = Collections.singletonList(pieceOrStack); // Or if really just a single piece.
                useFromDeck = -1; // Not a deck, so no deck parameters
              }

              if (useFromDeck != 0) {
                // This will iterate through actual game pieces
                for (final GamePiece gamePiece : pieceList) {
                  // If a property-based Fast Match is specified, we eliminate non-matchers of that first.
                  if (!passesPropertyFastMatch(gamePiece)) {
                    continue;
                  }

                  // These basic location filters are faster than equivalent filters in the Beanshell expression,
                  // and avoid re-evaluating/re-loading the source property for every target piece.

                  // Fast matches for Zone / Location
                  switch (target.targetType) {
                  case ZONE:
                  case CURZONE:
                    if (!fastZone.equals(gamePiece.getProperty(BasicPiece.CURRENT_ZONE))) {
                      continue;
                    }
                    break;
                  case LOCATION:
                  case CURLOC:
                    if (!fastLocation.equals(gamePiece.getProperty(BasicPiece.LOCATION_NAME))) {
                      continue;
                    }
                    break;
                  }

                  // Fast Match of "exact XY position"
                  if (target.targetType == GlobalCommandTarget.Target.XY) {
                    if (!fastBoard.isEmpty() && !fastBoard.equals(gamePiece.getProperty(BasicPiece.CURRENT_BOARD))) {
                      continue;
                    }
                    final Point pt = new Point(gamePiece.getPosition());
                    if (!fastX.equals(Integer.toString((int) pt.getX())) || !fastY.equals(Integer.toString((int) pt.getY()))) {
                      continue;
                    }
                  }

                  // Passed all the "Fast Match" tests -- the dispatcher will apply the BeanShell filter and if that passes will issue the command to the piece
                  dispatcher.accept(gamePiece);

                  if ((useFromDeck > 0) && visitor.getSelectedCount() >= useFromDeck) {
                    break;
                  }
                }
              }
            }
          }
        }
      }

      // Repaint for anything that has been moved by our shenanigans
      visitor.getTracker().repaint();

      // Now we grab our (possibly massive) command, encompassing every single thing that has happened to every
      // single piece affected by this command. This command can be sent to other clients involved in the same
      // game to replicate all the stuff we just did.
      command = visitor.getCommand();
    }
    catch (RecursionLimitException e) {
      // It is very easy to construct a set of GKC commands that fire each other off infinitely. This catches those.
      RecursionLimiter.infiniteLoop(e);
    }
    finally {
      RecursionLimiter.endExecution();
      if (reportSingle) {
        Map.setChangeReportingEnabled(true); // Restore normal reporting behavior (if we'd disabled all individual reports)
      }
    }

    return command; // Here, eat this tasty command!
  }

  /**
   * (Legacy - applies GKC without Fast Match)
   * @param map a single map
   * @param filter filter
   * @return command
   */
  public Command apply(Map map, PieceFilter filter) {
    return apply(new Map[]{map}, filter);
  }

  /**
   * (Legacy - applies GKC without Fast Match)
   * @param maps list of maps
   * @param filter filter
   * @return command
   */
  public Command apply(Map[] maps, PieceFilter filter) {
    return apply(maps, filter, null);
  }

  /**
   * Apply the key command on ONE SPECIFIC MAP to all pieces that pass the given filter and our Fast Match parameters.
   * @param map a single map
   * @param filter Filter to apply (created e.g. with {@link PropertyExpression#getFilter}
   * @param fastMatch Fast matching parameters, or null. {@link GlobalCommandTarget} and {@link VASSAL.configure.GlobalCommandTargetConfigurer}
   * @return the corresponding {@link Command} that would reproduce all the things this GKC just did, on another client.
   */
  public Command apply(Map map, PieceFilter filter, GlobalCommandTarget fastMatch) {
    return apply(new Map[]{map}, filter, fastMatch);
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

    public void setSelectedCount(int selectedCount) {
      this.selectedCount = selectedCount;
    }

    public int getSelectedCount() {
      return selectedCount;
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
        for (final PieceIterator it = d.drawCards(); it.hasMoreElements() && (getSelectFromDeck() < 0 || getSelectFromDeck() > selectedCount);) {
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
    final GlobalCommand other = (GlobalCommand) obj;
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
    if ((target.targetType == GlobalCommandTarget.Target.XY) && (!target.targetBoard.equals(other.target.targetBoard) || ((!target.targetX.equals(other.target.targetX)) || (!target.targetY.equals(other.target.targetY))))) {
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
