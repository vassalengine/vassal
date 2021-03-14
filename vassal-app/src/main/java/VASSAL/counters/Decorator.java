/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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

import VASSAL.build.BadDataReport;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.property.PersistentPropertyContainer;
import VASSAL.search.AbstractImageFinder;
import VASSAL.search.ImageSearchTarget;
import VASSAL.search.SearchTarget;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.SwingUtils;

import java.awt.Component;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;

import javax.swing.KeyStroke;

import org.apache.commons.lang3.ArrayUtils;

/**
 * The abstract class describing a generic 'Trait' of a full GamePiece. Follows the <a href="https://en.wikipedia.org/wiki/Decorator_pattern"></a>Decorator design pattern</a>
 * of wrapping around another instance of GamePiece (the 'inner' piece) and delegating some of the GamePiece methods to it. The
 * "innermost" piece of a Trait/Decorator stack will be a BasicPiece -- but note that although BasicPiece does implement GamePiece, it is NOT
 * a Decorator like traits are, and must be handled differently. Since Traits (Decorators) implement the {@link GamePiece} interface, they are
 * "given a look at" any method call to GamePiece, and may act on it and/or pass it "inward" toward the next Decorator in the stack, eventually
 * arriving at the BasicPiece. This is the reason for the VASSAL situation wherein only traits lower (more outward) in the list of traits are
 * able to affect/control/hide/restrict/etc traits that are higher (more inward, nearer the BasicPiece) in the list -- because the outer traits
 * receive the GamePiece method calls first, and thus have the opportunity to intercept or modify them if appropriate before passing them inward.
 * So a full logical GamePiece (the thing you see on the board), may consist of many Decorator instances (one for each trait) wrapped around the
 * BasicPiece.
 */
public abstract class Decorator extends AbstractImageFinder implements GamePiece, StateMergeable, PropertyNameSource, PersistentPropertyContainer,
  PropertyExporter, SearchTarget, ImageSearchTarget {

  protected GamePiece piece;
  private Decorator dec;
  private boolean selected = false;

  /** @param p Set the inner GamePiece -- usually the next Trait (Decorator) inward, or the BasicPiece itself. */
  public void setInner(GamePiece p) {
    piece = p;
    if (p != null) {
      p.setProperty(Properties.OUTER, this);
    }
  }

  /** @param m Each GamePiece belongs to a single {@link Map}. Default behavior for a trait is to pass the new map inward toward the BasicPiece. */
  @Override
  public void setMap(Map m) {
    piece.setMap(m);
  }

  /** @return the map for this piece. Each GamePiece belongs to a single {@link Map}. Default behavior ask the next
   * member inward which map it is on - the BasicPiece normally hold the map information, so the request will
   * eventually reach it. */
  @Override
  public Map getMap() {
    return piece.getMap();
  }

  /**
   * @return the next member "inward" (toward the BasicPiece) of the full GamePiece.
   * Follows the <a href="https://en.wikipedia.org/wiki/Decorator_pattern"></a>Decorator design pattern</a>
   * of wrapping around another instance of GamePiece (the 'inner' piece) and delegating some of the GamePiece methods to it. The
   * "innermost" piece of a Trait/Decorator stack will be a BasicPiece. Since Traits (Decorators) implement the {@link GamePiece} interface,
   * they are "given a look at" any method call to GamePiece, and may act on it and/or pass it "inward" toward the next Decorator in the stack,
   * eventually arriving at the BasicPiece. This is the reason for the VASSAL situation wherein only traits lower (more outward) in the list
   * of traits are able to affect/control/hide/restrict/etc traits that are higher (more inward, nearer the BasicPiece) in the list -- because
   * the outer traits receive the GamePiece method calls first, and thus have the opportunity to intercept or modify them if appropriate before
   * passing them inward. So a full logical GamePiece (the thing you see on the board), may consist of many Decorator instances (one for each
   * trait) wrapped around the BasicPiece.
   */
  public GamePiece getInner() {
    return piece;
  }

  /** @param s sets the {@link Stack} to which this piece belongs. Default behavior for a trait is to send the request to set the Stack inward (toward the BasicPiece) */
  @Override
  public void setParent(Stack s) {
    piece.setParent(s);
  }

  /** @return the {@link Stack} to which this piece belongs, if any. Default behavior for a trait is to ask the next member inward. */
  @Override
  public Stack getParent() {
    return piece.getParent();
  }

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
   * <br><br><b>Within a Trait/Decorator</b>, default behavior is to process some requests directly
   * (e.g. requests for our "inner" or "outer" link), process our *part* of certain other requests
   * (e.g. request for our game state information we supply state information for this trait and then
   * append any information obtained from passing the same request inward), and then for any other
   * requests that we cannot process we simply pass the request to the next trait/member inward.
   *
   * <br><br>When using this interface a piece's own properties are preferred to those of
   * "Global Properties", and those in turn are searched Zone-first then Map, then Module.
   *
   * <br><br>This method implements the {@link PropertySource} interface, which allows Global Properties
   * to be read by other types of object than GamePieces.
   *
   * @param key String key of property to be returned
   * @return Object containing new value of the specified property
   */
  @Override
  public Object getProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getKeyCommands();
    }
    else if (Properties.INNER.equals(key)) {
      return piece;
    }
    else if (Properties.OUTER.equals(key)) {
      return dec;
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return myGetState() + piece.getProperty(key);
    }
    else if (Properties.SELECTED.equals(key)) {
      return selected;
    }
    else {
      return piece.getProperty(key);
    }
  }

  /**
   * Returns a localized translation of the specified property value, if available. Otherwise
   * returns the non-localized version. See {@link #getProperty}.
   * @param key String key of property to be returned
   * @return Object containing localized text of the specified property, if available, otherwise non-localized value
   */
  @Override
  public Object getLocalizedProperty(Object key) {
    if (List.of(
      Properties.KEY_COMMANDS,
      Properties.INNER,
      Properties.OUTER,
      Properties.VISIBLE_STATE
    ).contains(key)) {
      return getProperty(key);
    }
    /*
     * Return local cached copy of Selection Status
     */
    else if (Properties.SELECTED.equals(key)) {
      return isSelected();
    }
    else {
      return piece.getLocalizedProperty(key);
    }
  }

  /**
   * Within a Trait/Decorator, the default behavior when setting a property is to handle
   * changing our own inner/outer links directly, to cache the selection state (while also
   * passing it inward), and to simply pass every other property change request inward. A
   * Trait with its own properties to maintain would intercept and process those requests,
   * while sending the rest inward to the next member of the piece.
   *
   * <br><br>Properties can be associated with a piece -- many may be game-specific, but others
   * are standard, such as the LocationName property exposed by BasicPiece -- and can
   * be set through this interface. The properties may or may not need to be encoded in
   * the piece's {@link #getState} method. Properties include the value of e.g. {@link Marker}
   * Traits, {@link DynamicProperty} Traits, and so forth.
   *
   * <br><br><b>NOTE:</b> Not all properties maintained by traits can be "set" by setProperty, even
   * though they can be read by getProperty -- they may be "read only" for instance. You will need
   * to check the code for individual traits to see what they support in this regard.
   *
   * <br><br><b>NOTE:</b> Properties outside the piece CANNOT be set by this
   * method (e.g. Global Properties), even though they can be read by {@link #getProperty} --
   * in this the two methods are not perfect mirrors.
   *
   * @param key String key of property to be changed
   * @param val Object containing new value of the property
   */
  @Override
  public void setProperty(Object key, Object val) {
    if (Properties.INNER.equals(key)) {
      setInner((GamePiece) val);
    }
    else if (Properties.OUTER.equals(key)) {
      dec = (Decorator) val;
    }
    /*
     * Cache Selection status and pass it on to all inner traits.
     */
    else if (Properties.SELECTED.equals(key)) {
      if (val instanceof Boolean) {
        setSelected((Boolean) val);
      }
      else {
        setSelected(false);
      }
      piece.setProperty(key, val);
    }
    else {
      piece.setProperty(key, val);
    }
  }

  /**
   * Default behavior for a Trait/Decorator is to pass requests to set persistent properties inward to the next member of
   * the piece.
   *
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
  @Override
  public Command setPersistentProperty(Object key, Object val) {
    // Not all GamePieces have persistent properties (though piece almost certainly will).
    if (piece instanceof PersistentPropertyContainer) {
      return ((PersistentPropertyContainer) piece).setPersistentProperty(key, val);
    }
    return null;
  }

  /**
   * @param key String key naming the persistent property whose value is to be returned.
   * @return the current value of a persistent property, or null if it doesn't exist.
   */
  @Override
  public Object getPersistentProperty(Object key) {
    // Standard getProperty also returns persistent properties
    return piece.getProperty(key);
  }

  /** @return next piece "outward" (away from BasicPiece) in the trait list. This method is required
   * by {@link Obscurable} to handle masking of getProperty calls. */
  public Decorator getOuter() {
    return dec;
  }

  /**
   * @param p Sets the location of this piece on its {@link Map}
   *          Default behavior by a Trait/Decorator is to pass the position request inward to the next member of the piece.
   */
  @Override
  public void setPosition(Point p) {
    piece.setPosition(p);
  }

  /**
   * @return The location of this piece on its {@link Map}. Default behavior by a Trait/Decorator is to request the information
   * from the next inward member of the piece.
   */
  @Override
  public Point getPosition() {
    return piece.getPosition();
  }

  /**
   * Sets the state of this-trait-only (inverse of {@link #myGetState}). The "state information" is
   * information that can change during the course of a game. State information is saved when the
   * game is saved and is transferred between players on the server. For example, the relative order
   * of pieces in a stack is state information, but whether the stack is expanded is not.
   *
   * @param newState New state information serialized in string form, ready
   * to be passed to a SequenceEncoder#decode.
   */
  public abstract void mySetState(String newState);

  /**
   * Extract the string describing this trait's state and forward the remaining string to the inner piece.
   * The "state information" is "game state" information that can change during the course of a game. State
   * information is saved when the game is saved and is transferred between players on the server. For
   * example, the relative order of pieces in a stack is state information, but whether the stack is expanded is not.
   *
   * @param newState the new state of this trait and all inner members of the piece
   */
  @Override
  public void setState(String newState) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, '\t');
    mySetState(st.nextToken());
    try {
      piece.setState(st.nextToken());
    }
    catch (NoSuchElementException e) {
      throw new IllegalStateException("No state for Decorator=" + myGetType());
    }
  }

  /**
   * Compute the difference between <code>newState</code> and <code>oldState</code>
   * and apply that difference to the current state. Default behavior for a Trait/Decorator
   * is to do the same for all inward members of the piece.
   *
   * <br><br>The "state information" is "game state" information that can change during the course of a game. State
   * information is saved when the game is saved and is transferred between players on the server. For
   * example, the relative order of pieces in a stack is state information, but whether the stack is expanded is not.
   *
   * @param newState the new state of this trait and all inner members of the piece
   * @param oldState the old state of this trait and all inner members of the piece
   */
  @Override
  public void mergeState(String newState, String oldState) {
    final SequenceEncoder.Decoder stNew = new SequenceEncoder.Decoder(newState, '\t');
    final String myNewState = stNew.nextToken();
    final String innerNewState = stNew.nextToken();
    final SequenceEncoder.Decoder stOld = new SequenceEncoder.Decoder(oldState, '\t');
    final String myOldState = stOld.nextToken();
    final String innerOldState = stOld.nextToken();
    if (!myOldState.equals(myNewState)) {
      mySetState(myNewState);
    }
    if (piece instanceof StateMergeable) {
      ((StateMergeable)piece).mergeState(innerNewState, innerOldState);
    }
    else {
      piece.setState(innerNewState);
    }
  }

  /**
   * The "state information" is "game state" information that can change during the course of a game. State
   * information is saved when the game is saved and is transferred between players on the server. For
   * example, the relative order of pieces in a stack is state information, but whether the stack is expanded is not.
   *
   * @return the game state information of this trait alone
   * @see #getState which returns state information for all inward piece members as well
   */
  public abstract String myGetState();

  /**
   * @return the state of a Trait/Decorator is a composition of {@link #myGetState} and the inner piece members' states
   *
   * <br><br>The "state information" is game state information that can change during  the course of a game.
   * State information is saved when the game is saved and is transferred between players on the server.  For
   * example, the relative order of pieces in a stack is state information, but whether the stack is expanded is not.
   */
  @Override
  public String getState() {
    final SequenceEncoder se = new SequenceEncoder(myGetState(), '\t');
    se.append(piece.getState());
    return se.getValue();
  }

  /**
   * The "type information" of a piece or trait is information
   * that does not change during the course of a game. Image file
   * names, context menu strings, etc., all should be reflected
   * in the type. The type information is returned serialized string
   * form, ready to be decoded by a SequenceEncoder#decode.
   * @see VASSAL.build.module.BasicCommandEncoder
   *
   * @return the type information of this trait alone
   * @see #getType which returns type information for all inward piece members as well
   */
  public abstract String myGetType();

  /**
   * The type of a Decorator/Trait is a composition of {@link #myGetType} and the type information of its inner piece member(s).
   *
   * <br><br>The "type information" of a piece or trait is information that does not change during the course of a game. Image file
   * names, context menu strings, etc., all should be reflected in the type. The type information is returned serialized string
   * form, ready to be decoded by a SequenceEncoder#decode.
   *
   * @return the combined type of this trait (from {@link #myGetType}) plus all inner piece members
   */
  @Override
  public String getType() {
    final SequenceEncoder se = new SequenceEncoder(myGetType(), '\t');
    se.append(piece.getType());
    return se.getValue();
  }

  /**
   * @return the key commands for this trait alone
   * @see #getKeyCommands which returns the key commands for this trait AND all inner piece members
   */
  protected abstract KeyCommand[] myGetKeyCommands();

  /**
   * @return The set of key commands that will populate the piece's right-click menu.
   * The key commands are accessible through the {@link Properties#KEY_COMMANDS} property.
   * The commands for a Trait/{@link Decorator} are a composite of {@link #myGetKeyCommands} and the
   * commands of its inner piece member(s), so requesting this information of the "outermost" Trait/Decorator of
   * a piece will produce the complete set of Key Commands for the entire logical game piece.
   *
   * See also: {@link #myGetKeyCommands}, which returns the commands for this Trait/Decorator only.
   */
  protected KeyCommand[] getKeyCommands() {
    final KeyCommand[] myC = myGetKeyCommands();
    final KeyCommand[] c =
      (KeyCommand[]) piece.getProperty(Properties.KEY_COMMANDS);

    if (c == null) return myC;
    else if (myC == null)  return c;
    else return ArrayUtils.addAll(myC, c);
  }

  /**
   * The response of this trait alone to the given KeyStroke or the virtual keystroke of a {@link VASSAL.tools.NamedKeyStroke}.
   * NamedKeyStrokes are allocated a unique internal KeyStroke when they are first used in a module, and that KeyStroke is passed
   * up and down the decorator stack to represent them (see {@link NamedKeyStroke#getKeyStroke()}).
   * @param stroke KeyStroke to apply (to apply a Named Keystroke send {@link NamedKeyStroke#getKeyStroke()}
   * @return Generated Command to reproduce any changes just made to to the game state, or null if no effect
   * @see #keyEvent
   */
  public abstract Command myKeyEvent(KeyStroke stroke);

  /**
   * The primary way for the piece or trait to receive events. Appends the command returned by {@link #myKeyEvent}
   * with the command returned by the inner piece member's {@link GamePiece#keyEvent} method. {@link KeyStroke} events
   * are forwarded to this method if they are received while the piece is selected (or as the result of e.g. a Global
   * Key Command being sent to the piece). The class implementing GamePiece can respond in any way it
   * likes. Actual key presses by the player, selected  items from the right-click Context Menu, keystrokes
   * "applied on move" by a Map that the piece has just moved on, and Global Key Commands all send KeyStrokes
   * (and NamedKeyStrokes) which are passed to pieces and traits through this interface.
   *
   * @see VASSAL.build.module.map.ForwardToKeyBuffer
   *
   * @param stroke KeyStroke to apply (note that although they are passed as {@link KeyStroke} objects, they may be {@link VASSAL.tools.NamedKeyStroke} objects)
   *
   * @return a {@link Command} that, when executed, will make all changes to the game state (maps, pieces, other
   * pieces, etc) to duplicate what the Trait/Decorator plus any inner piece members did in response to this event,
   * on another machine. Often a {@link ChangePiece} command, but for example if this keystroke caused the piece/trait
   * to decide to fire off a Global Key Command, then the Command returned would include the <i>entire</i> results of
   * that, appended  as subcommands. Returns null if the keystroke did not produce an effect.
   */
  @Override
  public Command keyEvent(KeyStroke stroke) {
    final Command c = myKeyEvent(stroke);
    return c == null ? piece.keyEvent(stroke)
      : c.append(piece.keyEvent(stroke));
  }

  /**
   * Each GamePiece must have a unique String identifier
   * @return unique ID for this piece
   * @see GameState#getNewPieceId
   */
  @Override
  public String getId() {
    return piece.getId();
  }

  /**
   * Each GamePiece must have a unique String identifier
   * @param id sets unique ID for this piece
   * @see GameState#getNewPieceId
   */
  @Override
  public void setId(String id) {
    piece.setId(id);
  }

  /**
   * @param p Trait to find the outermost trait of.
   * @return the outermost Decorator/Trait of this piece, i.e. the entire logical Game Piece with all traits
   */
  public static GamePiece getOutermost(GamePiece p) {
    while (p.getProperty(Properties.OUTER) != null) {
      p = (GamePiece) p.getProperty(Properties.OUTER);
    }
    return p;
  }

  /**
   *
   * @param p Trait to find the Innermost trait of
   * @return the innermost GamePiece member of this piece. In most cases, an instance of {@link BasicPiece}.
   */
  public static GamePiece getInnermost(GamePiece p) {
    while (p instanceof Decorator) {
      p = ((Decorator) p).piece;
    }
    return p;
  }

  /**
   * @return Working inward from this Trait/Decorator, finds and returns the first Trait/Decorator within the given GamePiece
   * that is an instance of the given Class.
   */
  public static GamePiece getDecorator(GamePiece p, Class<?> type) {
    while (p instanceof Decorator) {
      if (type.isInstance(p)) {
        return p;
      }
      p = ((Decorator) p).piece;
    }
    return null;
  }


  /**
   * {@link SearchTarget}
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Message Format strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Collections.emptyList();
  }



  /** @return the configurer for this trait - the dialog which allows the editing the piece's type information. Default
   * configurer is a {@link SimplePieceEditor}, but many traits will want to provide custom versions. */
  public PieceEditor getEditor() {
    return new SimplePieceEditor(this);
  }

  /** @return string information on trait, for debugging purposes */
  @Override
  public String toString() {
    if (piece == null) {
      return super.toString();
    }
    else {
      return super.toString() + "[name=" + getName() + ",type=" + getType() + ",state=" + getState() + "]"; //$NON-NLS-1$//
    }
  }

  /**
   * @return the translated name for this piece. Most pieces do not have
   * translatable elements, so just return the standard name
   */
  @Override
  public String getLocalizedName() {
    return piece.getLocalizedName();
  }

  /**
   * Return Internationalization (I18n) data for this piece
   * @return I18n data, used to hold translations of strings
   */
  public PieceI18nData getI18nData() {
    return new PieceI18nData(this);
  }

  protected PieceI18nData getI18nData(String command, String description) {
    final PieceI18nData data = new PieceI18nData(this);
    data.add(command, description);
    return data;
  }

  protected PieceI18nData getI18nData(String[] commands, String[] descriptions) {
    final PieceI18nData data = new PieceI18nData(this);
    for (int i = 0; i < commands.length; i++) {
      data.add(commands[i], descriptions[i]);
    }
    return data;
  }

  protected String getCommandDescription(String description, String command) {
    String s = "";
    if (description != null && description.length() > 0) {
      s += description + ": ";
    }
    return s + command;
  }

  protected String getTranslation(String key) {
    final String fullKey = TranslatablePiece.PREFIX + key;
    return Localization.getInstance().translate(fullKey, key);
  }

  /**
   * Report a Data Error detected by a trait
   * @param piece Our GamePiece for the error message
   * @param message Main error message
   * @param data additional data string
   * @param e throwable error associated with the problem
   */
  protected static void reportDataError(EditablePiece piece, String message, String data, Throwable e) {
    ErrorDialog.dataWarning(new BadDataReport(piece, message, data, e));
  }

  /**
   * Report a Data Error detected by a trait
   * @param piece Our GamePiece for the error message
   * @param message Main error message
   * @param data additional data string
   */
  protected static void reportDataError(EditablePiece piece, String message, String data) {
    ErrorDialog.dataWarning(new BadDataReport(piece, message, data));
  }

  /**
   * Report a Data Error detected by a trait
   * @param piece Our GamePiece for the error message
   * @param message Main error message
   */
  protected static void reportDataError(EditablePiece piece, String message) {
    ErrorDialog.dataWarning(new BadDataReport(piece, message));
  }

  /**
   * @return List of property names "exposed" by this piece -- that are available to other Traits and components to read.
   * Default behavior for a Trait is not to expose any properties, but Traits which should expose some (e.g. {@link DynamicProperty}
   * or {@link Marker}) would provide an array of the property names here.
   */
  @Override
  public List<String> getPropertyNames() {
    return new ArrayList<>(0);
  }

  /**
   * Set the Oldxxxx properties related to movement
   * This call has been replaced by {@link #putOldProperties(GamePiece)}
   * Any changes to Oldxxxx properties MUST be recorded in Commands.
   *
   * @param p Piece to set properties on
   */
  @Deprecated(since = "20200906", forRemoval = true)
  public static void setOldProperties(GamePiece p) {
    ProblemDialog.showDeprecated("20200906");
    putOldProperties(p);
  }

  /**
   * * Set the Oldxxxx properties related to movement (e.g. OldLocationName, OldZone, OldBoard, OldMap, OldX, OldY)
   *
   * @param p Piece to set properties on
   * @return Command encoding the property values
   */
  public static Command putOldProperties(GamePiece p) {

    // Not all GamePieces will have persistent properties
    if (!(p instanceof PersistentPropertyContainer)) {
      return null;
    }
    final PersistentPropertyContainer container = (PersistentPropertyContainer) p;

    String mapName = ""; //$NON-NLS-1$
    String boardName = ""; //$NON-NLS-1$
    String zoneName = ""; //$NON-NLS-1$
    String locationName = ""; //$NON-NLS-1$
    final Map m = p.getMap();
    final Point pos = p.getPosition();
    Command comm = new NullCommand();

    if (m != null) {
      mapName = m.getConfigureName();
      final Board b = m.findBoard(pos);
      if (b != null) {
        boardName = b.getName();
      }
      final Zone z = m.findZone(pos);
      if (z != null) {
        zoneName = z.getName();
      }
      locationName = m.locationName(pos);
    }

    comm = comm.append(container.setPersistentProperty(BasicPiece.OLD_X, String.valueOf(pos.x)));
    comm = comm.append(container.setPersistentProperty(BasicPiece.OLD_Y, String.valueOf(pos.y)));
    comm = comm.append(container.setPersistentProperty(BasicPiece.OLD_MAP, mapName));
    comm = comm.append(container.setPersistentProperty(BasicPiece.OLD_BOARD, boardName));
    comm = comm.append(container.setPersistentProperty(BasicPiece.OLD_ZONE, zoneName));
    comm = comm.append(container.setPersistentProperty(BasicPiece.OLD_LOCATION_NAME, locationName));

    return comm;
  }

  /**
   * @deprecated
   * Use {@link #setOldProperties(GamePiece)
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void setOldProperties() {
    ProblemDialog.showDeprecated("2020-08-06");
    putOldProperties(this);
  }

  /**
   * Utility methods to allow Decorator Editors to repack themselves.
   * Ensure the resulting dialog fits and is fully visible on the screen
   *
   * @param c must be one of the Swing components that make up the Decorator's controls.
   */
  public static void repack(Component c) {
    SwingUtils.repack(c);
  }

  /**
   * Utility methods to allow Decorator Editors to repack themselves.
   * Ensure the resulting dialog fits and is fully visible on the screen
   *
   * @param c must be one of the Decoarator's Configurers
   */
  public static void repack(Configurer c) {
    repack(c.getControls());
  }


  /**
   * Support caching Selection status locally
   * @param b current selected status of our piece
   */
  protected void setSelected(boolean b) {
    selected = b;
  }

  /**
   * Support caching Selection status locally
   * @return true if our piece is selected
   */
  protected boolean isSelected() {
    return selected;
  }

  /**
   * {@link ImageSearchTarget}
   * Adds all images used by this component AND any children to the collection
   * @param s Collection to add image names to
   */
  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    addLocalImageNames(s);
    if (piece instanceof ImageSearchTarget) {
      ((ImageSearchTarget) piece).addImageNamesRecursively(s);
    }
  }

  /**
   * Test if this Decorator's Class, Type and State are equal to another trait.
   *
   * Implementations of this method should compare the individual values of the fields that
   * make up the Decorators Type and State. Implementations should NOT compare the values
   * returned by myGetType() or myGetState().
   *
   * This method is intended to be used by Unit Tests to verify that a trait
   * is unchanged after going through a process such as serialization/deserialization.
   *
   * @param o Object to compare this Decorator to
   * @return true if the Class, type and state all match
   */
  public boolean testEquals(Object o) {
    return this.equals(o);
  }

  /**
   * Build a description of a trait of the form
   * Type - Description
   * Where Type is the translated trait type description and Description
   * is a supplied additional description
   *
   * @param i18nKey Translation key for trait type description
   * @param description Optional additional description
   * @return Combined description
   */
  protected String buildDescription(String i18nKey, String description) {
    return buildDescription(i18nKey, "", description);
  }

  protected String buildDescription(String i18nKey) {
    return Resources.getString(i18nKey);
  }

  /**
   * Build a description of a trait of the form
   * Type - value - Description
   *
   * @param i18nKey Translation key for trait type description
   * @param value Optional value to include in the description
   * @param description Optional additional description
   * @return Combined description
   */
  protected String buildDescription(String i18nKey, String value, String description) {
    final StringBuilder desc = new StringBuilder(buildDescription(i18nKey));
    if (value != null && ! value.isBlank()) {
      desc.append(" - ");
      desc.append(value);
    }
    if (description != null && ! description.isBlank()) {
      desc.append(" - ");
      desc.append(description);
    }
    return desc.toString();
  }

  /**
   * Implement PropertyExporter.getProperties at the Decorator level.
   * Add the properties from this Decorator to the supplied Map, then
   * call the next innermost piece to do the same.
   *
   * Do not overwrite values if an outer trait has a property with the same name.
   *
   * @param result Map of property values
   * @return Updated Map of property values
   */
  @Override
  public java.util.Map<String, Object> getProperties(java.util.Map<String, Object> result) {
    PropertyExporter.super.getProperties(result);
    return piece == null ? result : ((PropertyExporter) piece).getProperties(result);
  }
}

