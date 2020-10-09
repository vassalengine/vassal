/*
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

package VASSAL.property;

import VASSAL.command.Command;

/**
 * A container holding properties that must be maintained as part of the Game State (saved with saved games, passed to
 * other clients via logfile/server, etc).
 */
public interface PersistentPropertyContainer {

  /**
   * Setting a persistent property writes a property value into the piece (creating a new entry in the piece's persistent
   * property table if the specified key does not yet exist in it). Persistent properties are game-state-robust: they are
   * saved/restored with saved games, and are passed via {@link Command} to other players' clients in a multiplayer game.
   * The persistent property value can then be read from the piece via e.g. #getProperty. When reading back properties
   * out of a piece, the piece's built-in properties are checked first, then scratchpad properties (see #setProperty),
   * then external properties such as Global Properties. If <i>only</i> persistentProperties are to be searched, use
   * {@link #getPersistentProperty} instead.
   *
   * <br><br>In practical terms, setPersistentProperty is used mainly to implement the "Old" properties of BasicPiece (e.g.
   * "OldLocationName", "OldZone", "OldMap", "OldBoard", "OldX", "OldY"). A Persistent Property is indeed nearly identical
   * with DynamicProperty in storage/retrieval characteristics, and simply lacks the in-module interface for setting
   * values, etc. Module Designers are thus recommended to stick with Dynamic Property traits for these functions.
   *
   * @param key String key naming the persistent property to be set. If a corresponding persistent property does not exist it will be created.
   * @param val New value for the persistent property
   * @return a {@link Command} object which, when passed to another player's client via logfile, server, or saved game, will allow the
   * result of the "set" operation to be replicated.
   */
  Command setPersistentProperty(Object key, Object val);

  /**
   * @param key String key naming the persistent property whose value is to be returned.
   * @return the current value of a persistent property, or null if it doesn't exist.
   */
  Object getPersistentProperty(Object key);
}
