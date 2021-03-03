/*
 *
 * Copyright (c) 2020 by Vassal Development Team
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

import VASSAL.build.AutoConfigurable;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.script.expression.Expression;
import VASSAL.search.SearchTarget;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * GlobalCommandTarget configures and stores the "Fast Match" parameters of Global Key Commands, allowing certain
 * simple filters to be "pre-matched" without having to initiate the (relatively slower) BeanShell filters.
 */
public class GlobalCommandTarget implements ConfigurerFactory, SearchTarget {

  protected static final char ENCODE_DELIMITER = '|';

  protected GKCtype gkcType = GKCtype.MAP;  // What flavor of GKC (Module, Map, Deck, Piece)

  protected boolean fastMatchLocation = false; // True if we are doing Fast Match by location (else other values in this block unused)
  protected Target targetType = Target.MAP;    // Type of location Fast Match we are doing
  protected Expression targetMap;              // Specified Map (for MAP, ZONE, LOCATION, XY types)
  protected Expression targetBoard;            // Specified Board (for XY type)
  protected Expression targetZone;             // Specified Zone (for ZONE type)
  protected Expression targetLocation;         // Specified Location (for LOCATION type)
  protected Expression targetDeck;             // Specified Deck (for DECK type)
  protected Expression targetX;                // Specified X (for XY type)
  protected Expression targetY;                // Specified Y (for XY type)

  protected boolean fastMatchProperty = false; // True if we're doing a Fast Match by property value (else next two values ignored)
  protected Expression targetProperty;         // Name/Key of Fast Match property
  protected Expression targetValue;            // Value to match for that property
  protected CompareMode targetCompare;         // Comparison mode

  private GamePiece curPiece; // Reference piece for "current <place>". NOT encoded into the module, only set and used at time of command execution.

  @Override
  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
    return new GlobalCommandTargetConfigurer(key, name, ((MassKeyCommand) c).getTarget());
  }

  /**
   * Specifies the type of GKC being configured (affects which Target options are allowed)
   */
  public enum GKCtype {
    COUNTER, /** {@link CounterGlobalKeyCommand */
    MAP,     /** {@link VASSAL.build.module.map.MassKeyCommand} */
    MODULE,  /** {@link VASSAL.build.module.GlobalKeyCommand} */
    DECK     // {@link VASSAL.build.module.map.DeckGlobalKeyCommand}
  }

  /**
   * Comparison Modes for property match
   */
  public enum CompareMode {
    EQUALS("=="),
    NOT_EQUALS("!="),
    GREATER(">"),
    GREATER_EQUALS(">="),
    LESS("<"),
    LESS_EQUALS("<="),
    MATCH("=~"),
    NOT_MATCH("!~");

    String symbol;

    CompareMode(String symbol) {
      this.symbol = symbol;
    }

    public String getSymbol() {
      return symbol;
    }

    public static CompareMode whichSymbol(String symbol) {
      for (final CompareMode mode : GlobalCommandTarget.CompareMode.values()) {
        if (mode.getSymbol().equals(symbol)) {
          return mode;
        }
      }
      return EQUALS;
    }

    public static String[] getSymbols() {
      return Arrays.stream(values())
        .map(GlobalCommandTarget.CompareMode::getSymbol)
        .toArray(String[]::new);
    }
  }

  /**
   * Specifies the kind of target matching
   */
  public enum Target {
    CURSTACK,  // Current stack or deck (of issuing piece or deck)
    CURMAP,    // Current map           (of issuing piece)
    CURZONE,   // Current zone          (of issuing piece)
    CURLOC,    // Current location      (of issuing piece)
    MAP,       // Specified map
    ZONE,      // Specified zone
    LOCATION,  // Specified location name
    XY,        // Specified X/Y position
    DECK;      // Specified Deck

    /**
     * @return true if our match is relative to an issuing piece or deck
     */
    public boolean isCurrent() {
      return (this == CURSTACK) || (this == CURMAP) || (this == CURZONE) || (this == CURLOC);
    }

    /**
     * @return Localizable key corresponding to target value
     */
    public String toTranslatedString() {
      return "Editor.GlobalKeyCommand.target_" + name().toLowerCase();  //NON-NLS
    }

    /**
     * @return Internal keys for all target types
     */
    public static String[] getKeys() {
      return Arrays.stream(values())
        .map(GlobalCommandTarget.Target::name)
        .toArray(String[]::new);
    }

    /**
     * @return Localization keys for all target types
     */
    public static String[] geti18nKeys() {
      return Arrays.stream(values())
        .map(GlobalCommandTarget.Target::toTranslatedString)
        .toArray(String[]::new);
    }
  }

  public GlobalCommandTarget() {
    this(GKCtype.MAP);
  }

  public GlobalCommandTarget(GKCtype gkc) {
    setGKCtype(gkc);

    // Can't just let this shit be null => ANGRY ENCODER IS ANGRY!!!
    targetMap      = Expression.createExpression("");
    targetBoard    = Expression.createExpression("");
    targetZone     = Expression.createExpression("");
    targetLocation = Expression.createExpression("");
    targetDeck     = Expression.createExpression("");
    targetProperty = Expression.createExpression("");
    targetValue    = Expression.createExpression("");
    targetX        = Expression.createExpression("0");
    targetY        = Expression.createExpression("0");
    targetCompare  = CompareMode.EQUALS;
  }

  public GlobalCommandTarget(String s) {
    decode(s);
  }

  public GlobalCommandTarget(GlobalCommandTarget gc) {
    this(gc.encode());
  }

  /**
   * Encoder for the part of GlobalCommandTarget that gets stored in the module
   * @return Fast Match parameters encoded in string form
   */
  public String encode() {
    final SequenceEncoder se = new SequenceEncoder(ENCODE_DELIMITER);
    se.append(gkcType.name())
      .append(fastMatchLocation)
      .append(targetType.name())
      .append(targetMap.getExpression())
      .append(targetBoard.getExpression())
      .append(targetZone.getExpression())
      .append(targetLocation.getExpression())
      .append(targetX.getExpression())
      .append(targetY.getExpression())
      .append(targetDeck.getExpression())
      .append(fastMatchProperty)
      .append(targetProperty.getExpression())
      .append(targetValue.getExpression())
      .append(targetCompare.name());

    return se.getValue();
  }

  /**
   * Decoder for loading GlobalCommandTarget from the module XML
   * @param code String to decode into our fields.
   */
  public void decode(String code) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ENCODE_DELIMITER);
    final String source = sd.nextToken("");
    gkcType = source.isEmpty() ? GKCtype.MAP : GKCtype.valueOf(source);
    fastMatchLocation = sd.nextBoolean(false);
    final String type = sd.nextToken(Target.MAP.name());
    targetType = type.isEmpty() ? Target.MAP : Target.valueOf(type);
    targetMap = Expression.createExpression(sd.nextToken(""));
    targetBoard = Expression.createExpression(sd.nextToken(""));
    targetZone = Expression.createExpression(sd.nextToken(""));
    targetLocation = Expression.createExpression(sd.nextToken(""));
    targetX = Expression.createExpression(sd.nextToken("0"));
    targetY = Expression.createExpression(sd.nextToken("0"));
    targetDeck = Expression.createExpression(sd.nextToken(""));
    fastMatchProperty = sd.nextBoolean(false);
    targetProperty = Expression.createExpression(sd.nextToken(""));
    targetValue = Expression.createExpression(sd.nextToken(""));
    final String compare = sd.nextToken(CompareMode.EQUALS.name());
    targetCompare = compare.isEmpty() ? CompareMode.EQUALS : CompareMode.valueOf(compare);
  }

  /**
   * Compares two GlobalCommandTargets
   * @param o GlobalCommandTarget to compare to this one
   * @return true if their meaningful parts are equal
   */
  @Override
  public boolean equals(Object o) {
    if (! (o instanceof GlobalCommandTarget)) return false;
    final GlobalCommandTarget t = (GlobalCommandTarget) o;
    return encode().equals(t.encode());
  }

/*
  protected boolean fastMatchLocation = false; // True if we are doing Fast Match by location (else other values in this block unused)
  protected Target targetType = Target.MAP;    // Type of location Fast Match we are doing
  protected Expression targetMap;              // Specified Map (for MAP, ZONE, LOCATION, XY types)
  protected Expression targetBoard;            // Specified Board (for XY type)
  protected Expression targetZone;             // Specified Zone (for ZONE type)
  protected Expression targetLocation;         // Specified Location (for LOCATION type)
  protected Expression targetDeck;             // Specified Deck (for DECK type)
  protected Expression targetX;                // Specified X (for XY type)
  protected Expression targetY;                // Specified Y (for XY type)

  protected boolean fastMatchProperty = false; // True if we're doing a Fast Match by property value (else next two values ignored)
  protected Expression targetProperty;         // Name/Key of Fast Match property
  protected Expression targetValue;            // Value to match for that property
  protected CompareMode targetCompare;         // Comparison mode
*/

  /**
   * @return a list of the item's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> expList = new ArrayList<>();

    if (fastMatchLocation) {
      if ((targetType == Target.MAP) || (targetType == Target.ZONE) || (targetType == Target.LOCATION) || (targetType == Target.XY)) {
        expList.add(targetMap.getExpression());
      }
      if (targetType == Target.XY) {
        expList.add(targetBoard.getExpression());
        expList.add(targetX.getExpression());
        expList.add(targetY.getExpression());
      }
      else if (targetType == Target.ZONE) {
        expList.add(targetZone.getExpression());
      }
      else if (targetType == Target.LOCATION) {
        expList.add(targetLocation.getExpression());
      }
      else if (targetType == Target.DECK) {
        expList.add(targetDeck.getExpression());
      }
    }

    if (fastMatchProperty) {
      expList.add(targetProperty.getExpression());
      expList.add(targetValue.getExpression());
    }

    return expList;
  }

  /**
   * @return a list of any Message Format strings referenced in the item, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return Collections.emptyList();
  }

  /**
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the item, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return Collections.emptyList();
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the item, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Collections.emptyList();
  }

  /**
   * @return a list of any Property Names referenced in the item, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    if (fastMatchProperty) {
      return List.of(targetProperty.getExpression());
    }
    return Collections.emptyList();
  }

  // Welcome to Getters-and-Setters HELL!

  public GKCtype getGKCtype() {
    return gkcType;
  }

  public void setGKCtype(GKCtype gkcType) {
    this.gkcType = gkcType;
  }

  public boolean isFastMatchLocation() {
    return fastMatchLocation;
  }

  public void setFastMatchLocation(boolean fastMatchLocation) {
    this.fastMatchLocation = fastMatchLocation;
  }

  public boolean isFastMatchProperty() {
    return fastMatchProperty;
  }

  public void setFastMatchProperty(boolean fastMatchProperty) {
    this.fastMatchProperty = fastMatchProperty;
  }

  public Target getTargetType() {
    return targetType;
  }

  public void setTargetType(Target targetType) {
    this.targetType = targetType;
  }

  public void setTargetType(String targetType) {
    this.targetType = Target.valueOf(targetType);
  }

  public Expression getTargetMap() {
    return targetMap;
  }

  public void setTargetMap(Expression targetMap) {
    this.targetMap = targetMap;
  }

  public void setTargetMap(String targetMap) {
    this.targetMap = Expression.createExpression(targetMap);
  }

  public Expression getTargetBoard() {
    return targetBoard;
  }

  public void setTargetBoard(Expression targetBoard) {
    this.targetBoard = targetBoard;
  }

  public void setTargetBoard(String targetBoard) {
    this.targetBoard = Expression.createExpression(targetBoard);
  }

  public Expression getTargetZone() {
    return targetZone;
  }

  public void setTargetZone(Expression targetZone) {
    this.targetZone = targetZone;
  }

  public void setTargetZone(String targetZone) {
    this.targetZone = Expression.createExpression(targetZone);
  }

  public Expression getTargetLocation() {
    return targetLocation;
  }

  public void setTargetLocation(Expression targetLocation) {
    this.targetLocation = targetLocation;
  }

  public void setTargetLocation(String targetLocation) {
    this.targetLocation = Expression.createExpression(targetLocation);
  }


  public Expression getTargetDeck() {
    return targetDeck;
  }

  public void setTargetDeck(Expression targetDeck) {
    this.targetDeck = targetDeck;
  }

  public void setTargetDeck(String targetDeck) {
    this.targetDeck = Expression.createExpression(targetDeck);
  }

  public Expression getTargetProperty() {
    return targetProperty;
  }

  public void setTargetProperty(Expression targetProperty) {
    this.targetProperty = targetProperty;
  }

  public void setTargetProperty(String targetProperty) {
    this.targetProperty = Expression.createExpression(targetProperty);
  }

  public Expression getTargetValue() {
    return targetValue;
  }

  public void setTargetValue(Expression targetValue) {
    this.targetValue = targetValue;
  }

  public void setTargetValue(String targetValue) {
    this.targetValue = Expression.createExpression(targetValue);
  }

  public CompareMode getTargetCompare() {
    return targetCompare;
  }

  public void setTargetCompare(CompareMode targetCompare) {
    this.targetCompare = targetCompare;
  }

  public void setTargetCompare(String targetCompare) {
    this.targetCompare = CompareMode.valueOf(targetCompare);
  }

  public Expression getTargetX() {
    return targetX;
  }

  public void setTargetX(Expression targetX) {
    this.targetX = targetX;
  }
  public void setTargetX(String targetX) {
    this.targetX = Expression.createExpression(targetX);
  }
  public void setTargetX(int targetX) {
    this.targetX = Expression.createExpression(Integer.toString(targetX));
  }

  public Expression getTargetY() {
    return targetY;
  }

  public void setTargetY(Expression targetY) {
    this.targetY = targetY;
  }
  public void setTargetY(String targetY) {
    this.targetY = Expression.createExpression(targetY);
  }
  public void setTargetY(int targetY) {
    this.targetY = Expression.createExpression(Integer.toString(targetY));
  }

  public void setCurPiece(GamePiece curPiece) {
    this.curPiece = curPiece;
  }

  public GamePiece getCurPiece() {
    return curPiece;
  }
}
