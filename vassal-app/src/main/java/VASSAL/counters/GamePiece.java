/*
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

import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;

/**
 * Interface to deal with the behaviors of a physical component of the game, <b><i>or a Trait of one</i></b>. Although from
 * the name you might think that anything implementing this would be "a whole piece" (and perhaps in Elder Times that was true),
 * a GamePiece is in fact implemented <b>NOT ONLY</b> by all of the three "basic" piece types: {@link BasicPiece}, {@link Stack},
 * or {@link Deck}; <b>BUT ALSO</b> it is implemented by all of the Traits ({@link Decorator} classes) that form the subcomponents
 * of a full "piece" as we would know it on the game board.
 *
 * <br><br>When a GamePiece method of a Trait (Decorator) is called, the Trait first attempts to service the method call, and then
 * normally (though not always, as in the case of "draw" calls to pieces with Invisible/{@link Hideable} or Mask/{@link Obscurable}
 * traits) calls the same method for the next trait inward from it in the Decorator stack for the overall piece, and so on in turn
 * until the "innermost" piece (normally a {@link BasicPiece}) has its method called.
 *
 * <br><b>Methods:</b>
 * <br>{@link #getType} - gets type information fixed throughout game
 * <br>{@link #getState} - gets state information changeable during gameplay
 * <br>{@link #setState} - sets state information changeable during gameplay
 * <br>{@link #setProperty} - sets other properties, *possibly* game-related, about a piece.
 * <br>{@link #getProperty} - gets other properties, *possibly* game-related, about a piece.
 * <br>
 * <br>{@link #draw} - tells this piece or part-of-piece to draw itself
 * <br>{@link #keyEvent} - gives this piece or Trait a chance to process key events. This is the main way for a GamePiece to receive "events".
 * <br>
 * <br>{@link #getName} - the plain English name of the piece
 * <br>{@link #getLocalizedName()} - the localized name of the piece
 * <br>{@link #getMap} and {@link #setMap} - Each GamePiece belongs to a single {@link Map}
 * <br>{@link #getPosition} and {@link #setPosition} - position on the map
 * <br>{@link #getParent} and {@link #setParent} - deals with membership in a {@link Stack}
 * <br>{@link #boundingBox} - bounding box
 * <br>{@link #getShape} - shape
 * <br>{@link #getId} and {@link #setId} - each piece has a unique ID
 * <br>
 * <br><b>See Also:</b>
 * <br><i>extends</i> {@link PropertySource}
 * <br><i>extended by </i> {@link EditablePiece}, {@link VASSAL.i18n.TranslatablePiece}
 */
public interface GamePiece extends PropertySource {

  /** @param map Each GamePiece belongs to a single {@link Map} */
  void setMap(Map map);

  /** @return Each GamePiece belongs to a single {@link Map} */
  Map getMap();

  /**
   * Draw this GamePiece
   * @param g target Graphics object
   * @param x x-location of the center of the piece
   * @param y y-location of the center of the piece
   * @param obs the Component on which this piece is being drawn
   * @param zoom the scaling factor.
   */
  void draw(Graphics g, int x, int y, Component obs, double zoom);

  /**
   * @return the location of this piece on its owning {@link Map}
   */
  Point getPosition();

  /** @param p Sets the location of this piece on its {@link Map} */
  void setPosition(Point p);

  /**
   * @return The area which this GamePiece occupies when drawn at the point (0,0)
   */
  Rectangle boundingBox();

  /**
   * @return The shape of the piece from the user's viewpoint. This defines the area
   * in which the user must click to select or move the piece, for example.
   * Like {@link #boundingBox}, it assumes the position is (0,0) -- which to be clear is
   * normally aligned with the CENTER of the piece image -- and must be translated to the
   * actual location where the piece is being drawn. For most ordinary pieces, the shape
   * returned here will simply be equivalent to the bounding box, but see {@link NonRectangular}.
   */
  Shape getShape();

  /**
   * @return the {@link Stack} to which this piece belongs, or null if it doesn't belong to a stack.
   */
  Stack getParent();

  /** @param s sets the {@link Stack} to which this piece belongs. */
  void setParent(Stack s);

  /**
   * The primary way for the piece or trait to receive events. {@link KeyStroke} events are forward
   * to this method if they are received while the piece is selected (or as the result of e.g. a Global
   * Key Command being sent to the piece). The class implementing GamePiece can respond in any way it
   * likes. Actual key presses by the player, selected  items from the right-click Context Menu, keystrokes
   * "applied on move" by a Map that the piece has just moved on, and Global Key Commands all send KeyStrokes
   * (and NamedKeyStrokes) which are passed to pieces and traits through this interface.
   *
   * @return a {@link Command} that, when executed, will make all changes to the game state (maps, pieces, other
   * pieces, etc) to duplicate what the piece did in response to this event on another machine. Often a
   * {@link ChangePiece} command, but for example if this keystroke caused the piece/trait to decide to fire
   * off a Global Key Command, then the Command returned would include the <i>entire</i> results of that, appended
   * as subcommands.
   *
   * @see VASSAL.build.module.map.ForwardToKeyBuffer
   */
  Command keyEvent(KeyStroke stroke);

  /** The plain English name for this piece */
  String getName();

  /** And the translated name for this piece */
  String getLocalizedName();

  /**
   * Each GamePiece must have a unique String identifier
   * @return unique ID for this piece
   * @see GameState#getNewPieceId
   */
  String getId();

  /**
   * Each GamePiece must have a unique String identifier
   * @param id sets unique ID for this piece
   * @see GameState#getNewPieceId
   */
  void setId(String id);

  /** @return The "type information" of a piece or trait is information
   * that does not change during the course of a game. Image file
   * names, context menu strings, etc., all should be reflected
   * in the type. The type information is returned serialized string
   * form, ready to be decoded by a SequenceEncoder#decode.
   * @see BasicCommandEncoder */
  String getType();

  /**
   * @return The "state information" is game state information that can change during
   * the course of a game. State information is saved when the game
   * is saved and is transferred between players on the server.  For
   * example, the relative order of pieces in a stack is state
   * information, but whether the stack is expanded is not. */
  String getState();

  /**
   * @param newState New state information serialized in string form, ready
   * to be passed to a SequenceEncoder#decode. The "state information" is
   * game state information that can change during the course of a game. State information
   * is saved when the game is saved and is transferred between players on the
   * server. For example, the relative order of pieces in a stack is state
   * information, but whether the stack is expanded is not. */
  void setState(String newState);

  /**
   * Properties can be associated with a piece -- many may be game-specific, but others
   * are standard, such as the LocationName property exposed by BasicPiece -- and can
   * be set through this interface. The properties may or may not need to be encoded in
   * the piece's {@link #getState} method. Properties include the value of e.g. {@link Marker}
   * Traits, {@link DynamicProperty} Traits, and so forth.
   *
   * <br><br><b>NOTE:</b> Properties outside the piece, however, CANNOT be set by this
   * method (e.g. Global Properties), even though they can be read by {@link #getProperty} --
   * in this the two methods are not perfect mirrors.
   *
   * @param key String key of property to be changed
   * @param val Object containing new value of the property
   */
  void setProperty(Object key, Object val);

  /**
   * Properties can be associated with a piece -- many may be game-specific, but others
   * are standard, such as the LocationName property exposed by BasicPiece -- and can
   * be read through this interface. The properties may or may not need to be encoded in
   * the piece's {@link #getState} method. Properties include the value of e.g. {@link Marker}
   * Traits, {@link DynamicProperty} Traits, and so forth. Furthermore they include the values
   * of any visible "Global Property" in a Vassal module, whether at the module level, map
   * level, or zone level -- but these "higher level" properties, coming from "outside the piece",
   * CANNOT be written to by the {@link #setProperty} method even though they can be read by
   * this method -- in this sense the two methods are NOT perfect mirrors.
   *
   * <br><br>When using this interface a piece's own properties are preferred to those of
   * "Global Properties", and those in turn are searched Zone-first then Map, then Module.
   *
   * This method implements the {@link PropertySource} interface, which allows Global Properties
   * to be read by other types of object than GamePieces.
   *
   * @param key String key of property to be returned
   * @return Object containing new value of the specified property
   */
  @Override
  Object getProperty(Object key);

  default Command prepareMove(Command c, boolean mark_moved) {
    return c;
  }

  default Command finishMove(Command c, boolean afterburner, boolean findmat) {
    return c;
  }
}
