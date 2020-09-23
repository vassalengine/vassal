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

import VASSAL.build.AutoConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;

import java.awt.Point;
import java.util.Arrays;

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
  protected GlobalCommandTarget targetType = GlobalCommandTarget.GAME;
  protected String targetMap = "";
  protected String targetBoard = "";
  protected String targetZone = "";
  protected String targetRegion = "";
  protected String targetGrid = "";
  protected boolean targetExactMatch = false;
  protected String targetProperty = "";
  protected String targetValue = "";
  protected int targetX = 0;
  protected int targetY = 0;

  /**
   * Levels of pre-filtering of piece location for Global Key Commands.
   */
  public enum GlobalCommandTarget {
    GAME("game"),        //NON-NLS
    MAP("map"),          //NON-NLS
    ZONE("zone"),        //NON-NLS
    REGION("region"),    //NON-NLS
    XY("xy");            //NON-NLS

    private final String name;

    GlobalCommandTarget(String name) {
      this.name = name;
    }

    @Override
    public String toString() {
      return name;
    }

    public String toTranslatedString() {
      return "Editor.GlobalKeyCommand.target_" + name;  //NON-NLS
    }

    public static String[] getKeys() {
      return Arrays.stream(values())
        .map(Enum::toString)
        .toArray(String[]::new);
    }

    public static String[] geti18nKeys() {
      return Arrays.stream(values())
        .map(GlobalCommandTarget::toTranslatedString)
        .toArray(String[]::new);
    }
  }


  public static class GlobalCommandTargetConfigurer extends TranslatableStringEnum {
    @Override

    public String[] getValidValues(AutoConfigurable target) {
      return GlobalCommandTarget.getKeys();
    }

    public String[] getI18nKeys(AutoConfigurable target) {
      return GlobalCommandTarget.geti18nKeys();
    }
  }


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

  public void setTargetType (GlobalCommandTarget targetType) {
    this.targetType = targetType;
  }

  public GlobalCommandTarget getTargetType () {
    return targetType;
  }

  public void setTargetX (int targetX) {
    this.targetX = targetX;
  }

  public void setTargetY (int targetY) {
    this.targetY = targetY;
  }

  public int getTargetX () {
    return targetX;
  }

  public int getTargetY () {
    return targetY;
  }

  public void setTargetMap (String targetMap) {
    this.targetMap = targetMap;
  }

  public String getTargetMap () {
    return targetMap;
  }

  public void setTargetBoard (String targetBoard) {
    this.targetBoard = targetBoard;
  }

  public String getTargetBoard () {
    return targetBoard;
  }

  public void setTargetZone (String targetZone) {
    this.targetZone = targetZone;
  }

  public String getTargetZone () {
    return targetZone;
  }

  public void setTargetRegion (String targetRegion) {
    this.targetRegion = targetRegion;
  }

  public String getTargetRegion () {
    return targetRegion;
  }

  public void setTargetProperty (String targetProperty) {
    this.targetProperty = targetProperty;
  }

  public String getTargetProperty () {
    return targetProperty;
  }

  public void setTargetValue (String targetValue) {
    this.targetValue = targetValue;
  }

  public String getTargetValue () {
    return targetValue;
  }

  public boolean isTargetExactMatch() {
    return targetExactMatch;
  }

  public void setTargetExactMatch (Boolean targetExactMatch) {
    this.targetExactMatch = targetExactMatch;
  }



  public Command apply(Map m, PieceFilter filter) {
    return apply(new Map[]{m}, filter);
  }
  /**
   * Apply the key command to all pieces that pass the given filter on all the given maps
   *
   * @param m list of maps
   * @param filter beanshell filter
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
          GameModule.getGameModule().getChatter(), "*" + reportText); //NON-NLS
        c.execute();
      }

      for (Map map : m) {
        if ((targetType != GlobalCommandTarget.GAME) && !targetMap.isEmpty() && !targetMap.equals(map.getConfigureName())) {
          continue;
        }

        Visitor visitor = new Visitor(c, filter, keyStroke);
        DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);
        GamePiece[] p = map.getPieces();
        for (GamePiece gamePiece : p) {

          if (targetExactMatch && !targetProperty.isEmpty()) {
            String value = (String) gamePiece.getProperty(targetProperty);
            if (!value.equals(targetValue)) {
              continue;
            }
          }

          // These basic location filters are hopefully(?) faster than equivalent filters in the Beanshell expression
          // If this is a stack, the top piece in the stack's current properties will be the same as any other piece in the stack.
          if ((targetType == GlobalCommandTarget.ZONE) || (targetType == GlobalCommandTarget.REGION)) {
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
            switch (targetType) {
            case ZONE:
              if (!targetZone.equals((String) pp.getProperty(BasicPiece.CURRENT_ZONE))) {
                continue;
              }
              break;
            case REGION:
              if (!targetRegion.equals((String) pp.getProperty(BasicPiece.LOCATION_NAME))) {
                continue;
              }
              break;
            }
          }

          if (targetType == GlobalCommandTarget.XY) {
            if (!targetBoard.equals((String)gamePiece.getProperty(BasicPiece.CURRENT_BOARD))) {
              continue;
            }
            Point pt = new Point (gamePiece.getPosition());
            if ((targetX != pt.getX()) || (targetY != pt.getY())) {
              continue;
            }
          }

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
   * @param selectFromDeck Set the number of pieces to select from a deck that the command will apply to.  A value <0 means to apply to all pieces in the deck
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
    if (targetType != other.targetType) {
      return false;
    }
    if ((targetType != GlobalCommandTarget.GAME) && !targetMap.equals(other.targetMap)) {
      return false;
    }
    if ((targetType == GlobalCommandTarget.ZONE) && !targetZone.equals(other.targetZone)) {
      return false;
    }
    if ((targetType == GlobalCommandTarget.REGION) && !targetRegion.equals(other.targetRegion)) {
      return false;
    }
    if ((targetType == GlobalCommandTarget.XY) && (!targetBoard.equals(other.targetBoard) || ((targetX != other.targetX) || (targetY != other.targetY)))) {
      return false;
    }

    return true;
  }
}
