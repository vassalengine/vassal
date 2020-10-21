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

import java.util.Arrays;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * GlobalCommandTarget configures and stores the "Fast Match" parameters of Global Key Commands, allowing certain
 * simple filters to be "pre-matched" without having to initiate the (relatively slower) BeanShell filters.
 */
public class GlobalCommandTarget implements ConfigurerFactory {

  protected static final char ENCODE_DELIMITER = '|';

  protected GKCtype gkcType = GKCtype.MAP;  // What flavor of GKC (Module, Map, Deck, Piece)

  protected boolean fastMatchLocation = false; // True if we are doing Fast Match by location (else other values in this block unused)
  protected Target targetType = Target.MAP;    // Type of location Fast Match we are doing
  protected String targetMap = "";             // Specified Map (for MAP, ZONE, LOCATION, XY types)
  protected String targetBoard = "";           // Specified Board (for XY type)
  protected String targetZone = "";            // Specified Zone (for ZONE type)
  protected String targetLocation = "";        // Specified Location (for LOCATION type)
  protected String targetDeck = "";            // Specified Deck (for DECK type)
  protected int targetX = 0;                   // Specified X (for XY type)
  protected int targetY = 0;                   // Specified Y (for XY type)

  protected boolean fastMatchProperty = false; // True if we're doing a Fast Match by property value (else next two values ignored)
  protected String targetProperty = "";        // Name/Key of Fast Match property
  protected String targetValue = "";           // Value to match for that property

  private GamePiece curPiece; // Reference piece for "current <place>". NOT encoded into the module, only set and used at time of command execution.

  @Override
  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
    return new GlobalCommandTargetConfigurer(key, name);
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
  }

  public GlobalCommandTarget(String s) {
    decode(s);
  }

  public String encode() {
    SequenceEncoder se = new SequenceEncoder(ENCODE_DELIMITER);
    se.append(gkcType.name())
      .append(fastMatchLocation)
      .append(targetType.name())
      .append(targetMap)
      .append(targetBoard)
      .append(targetZone)
      .append(targetLocation)
      .append(targetX)
      .append(targetY)
      .append(targetDeck)
      .append(fastMatchProperty)
      .append(targetProperty)
      .append(targetValue);

    return se.getValue();
  }

  public void decode(String code) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ENCODE_DELIMITER);
    final String source = sd.nextToken("");
    gkcType = source.isEmpty() ? GKCtype.MAP : GKCtype.valueOf(source);
    fastMatchLocation = sd.nextBoolean(false);
    final String type = sd.nextToken(Target.MAP.toString());
    targetType = type.isEmpty() ? Target.MAP : Target.valueOf(type);
    targetMap = sd.nextToken("");
    targetBoard = sd.nextToken("");
    targetZone = sd.nextToken("");
    targetLocation = sd.nextToken("");
    targetX = sd.nextInt(0);
    targetY = sd.nextInt(0);
    targetDeck = sd.nextToken("");
    fastMatchProperty = sd.nextBoolean(false);
    targetProperty = sd.nextToken("");
    targetValue = sd.nextToken("");
  }

  @Override
  public boolean equals(Object o) {
    if (! (o instanceof GlobalCommandTarget)) return false;
    final GlobalCommandTarget t = (GlobalCommandTarget) o;
    return encode().equals(t.encode());
  }

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

  public String getTargetMap() {
    return targetMap;
  }

  public void setTargetMap(String targetMap) {
    this.targetMap = targetMap;
  }

  public String getTargetBoard() {
    return targetBoard;
  }

  public void setTargetBoard(String targetBoard) {
    this.targetBoard = targetBoard;
  }

  public String getTargetZone() {
    return targetZone;
  }

  public void setTargetZone(String targetZone) {
    this.targetZone = targetZone;
  }

  public String getTargetLocation() {
    return targetLocation;
  }

  public void setTargetLocation(String targetLocation) {
    this.targetLocation = targetLocation;
  }

  public String getTargetDeck() {
    return targetDeck;
  }

  public void setTargetDeck(String targetDeck) {
    this.targetDeck = targetDeck;
  }

  public String getTargetProperty() {
    return targetProperty;
  }

  public void setTargetProperty(String targetProperty) {
    this.targetProperty = targetProperty;
  }

  public String getTargetValue() {
    return targetValue;
  }

  public void setTargetValue(String targetValue) {
    this.targetValue = targetValue;
  }

  public int getTargetX() {
    return targetX;
  }

  public void setTargetX(int targetX) {
    this.targetX = targetX;
  }

  public int getTargetY() {
    return targetY;
  }

  public void setTargetY(int targetY) {
    this.targetY = targetY;
  }

  public void setCurPiece(GamePiece curPiece) {
    this.curPiece = curPiece;
  }

  public GamePiece getCurPiece() {
    return curPiece;
  }
}
