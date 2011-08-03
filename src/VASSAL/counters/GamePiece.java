/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.KeyStroke;

import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;

/**
 * Basic class for representing a physical component of the game
 */
public interface GamePiece extends PropertySource {

  /** Each GamePiece belongs to a single {@link Map} */
  public void setMap(Map map);

  public Map getMap();

  /**
   * Draw this GamePiece
   * @param g
   * @param x x-location of the center of the piece
   * @param y y-location of the center of the piece
   * @param obs the Component on which this piece is being drawn
   * @param zoom the scaling factor.
   */
  public void draw(Graphics g, int x, int y, Component obs, double zoom);

  /**
   * @return the location of this piece on its owning {@link Map}
   */
  public Point getPosition();

  public void setPosition(Point p);

  /**
   * The area which this GamePiece occupies when
   * drawn at the point (0,0)
   */
  public Rectangle boundingBox();

  /**
   * The shape of the piece from the user's viewpoint.  This defines the area
   * in which the user must click to select or move the piece, for example.
   * Like {@link #boundingBox}, it assumes the position is (0,0) and must be translated
   * to the actual location where the piece is being drawn.
   */
  public Shape getShape();

  /**
   * @return the {@link Stack} to which this piece belongs, or null if it doesn't belong to a stack.
   */
  public Stack getParent();

  public void setParent(Stack s);

  /**
   * Keyboard events are forward to this method when a piece is selected
   * The GamePiece can respond in any way it likes
   *
   * @return a {@link Command} that, when executed, will invoke
   * the same response.  Usually a {@link ChangePiece} command.
   *
   * @see VASSAL.build.module.map.ForwardToKeyBuffer
   */
  public Command keyEvent(KeyStroke stroke);

  /** The plain English name for this piece */
  public String getName();

  /** And the translated name for this piece */
  public String getLocalizedName();

  /**
   * Each GamePiece must have a unique String identifier
   * @see GameState#getNewPieceId
   */
  public String getId();

  public void setId(String id);

  /** The type information is information that does not change
   * during the course of a game.  Image file names, popup menu
   * command names, etc., all should be reflected in the type.
   * @see BasicCommandEncoder */
  public String getType();

  /** The state information is information that can change during
   * the course of a game.  State information is saved when the game
   * is saved and is transferred between players on the server.  For
   * example, the relative order of pieces in a stack is state
   * information, but whether the stack is expanded is not */
  public String getState();

  public void setState(String newState);

  /**
   * Other properties, possibly game-specific, can be associated with a piece.
   * The properties may or may not need to be encoded
   * in the piece's {@link #getState} method.  */
  public void setProperty(Object key, Object val);

  public Object getProperty(Object key);
}
