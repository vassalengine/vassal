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

public class GlobalCommandTarget implements ConfigurerFactory {

  protected static final char ENCODE_DELIMITER = '|';

  protected boolean isCounterGkc;
  protected boolean useLocation = false;
  protected Target targetType = Target.MAP;
  protected String targetMap = "";
  protected String targetBoard = "";
  protected String targetZone = "";
  protected String targetLocation = "";
  protected int targetX = 0;
  protected int targetY = 0;

  protected boolean useProperty = false;
  protected String targetProperty = "";
  protected String targetValue = "";

  @Override
  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
    return new GlobalCommandTargetConfigurer(key, name);
  }

  public enum Target {
    CURSTACK,
    CURMAP,
    CURZONE,
    CURLOC,
    MAP,
    ZONE,
    LOCATION,
    XY;

    public String toTranslatedString() {
      return "Editor.GlobalKeyCommand.target_" + name().toLowerCase();  //NON-NLS
    }

    public static String[] getKeys() {
      return Arrays.stream(values())
        .map(GlobalCommandTarget.Target::name)
        .toArray(String[]::new);
    }

    public static String[] geti18nKeys() {
      return Arrays.stream(values())
        .map(GlobalCommandTarget.Target::toTranslatedString)
        .toArray(String[]::new);
    }
  }

  public GlobalCommandTarget() {
    this(false);
  }

  public GlobalCommandTarget(boolean isCounterGkc) {
    setCounterGkc(isCounterGkc);
  }

  public GlobalCommandTarget(String s) {
    decode(s);
  }

  public String encode() {
    SequenceEncoder se = new SequenceEncoder(ENCODE_DELIMITER);
    se.append(isCounterGkc)
      .append(useLocation)
      .append(targetType.toString())
      .append(targetMap)
      .append(targetBoard)
      .append(targetZone)
      .append(targetLocation)
      .append(targetX)
      .append(targetY)
      .append(useProperty)
      .append(targetProperty)
      .append(targetValue);

    return se.getValue();
  }

  public void decode(String code) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ENCODE_DELIMITER);
    isCounterGkc = sd.nextBoolean(false);
    useLocation = sd.nextBoolean(false);
    final String type = sd.nextToken(Target.MAP.toString());
    targetType = type.isEmpty() ? Target.MAP : Target.valueOf(type);
    targetMap = sd.nextToken("");
    targetBoard = sd.nextToken("");
    targetZone = sd.nextToken("");
    targetLocation = sd.nextToken("");
    targetX = sd.nextInt(0);
    targetY = sd.nextInt(0);
    useProperty = sd.nextBoolean(false);
    targetProperty = sd.nextToken("");
    targetValue = sd.nextToken("");
  }

  @Override
  public boolean equals(Object o) {
    if (! (o instanceof GlobalCommandTarget)) return false;
    final GlobalCommandTarget t = (GlobalCommandTarget) o;
    return encode().equals(t.encode());
  }

  public boolean isCounterGkc() {
    return isCounterGkc;
  }

  public void setCounterGkc(boolean counterGkc) {
    isCounterGkc = counterGkc;
  }

  public boolean isUseLocation() {
    return useLocation;
  }

  public void setUseLocation(boolean useLocation) {
    this.useLocation = useLocation;
  }

  public boolean isUseProperty() {
    return useProperty;
  }

  public void setUseProperty(boolean useProperty) {
    this.useProperty = useProperty;
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

}
