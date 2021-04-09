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

import VASSAL.build.GameModule;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameState;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.command.SetPersistentPropertyCommand;
import VASSAL.configure.ImageSelector;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.property.PersistentPropertyContainer;
import VASSAL.search.AbstractImageFinder;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.ScaledImagePainter;

import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

/**
 * Basic class for representing a physical component of the game. Can be e.g. a counter, a card, or an overlay.
 *
 * Note like traits, BasicPiece implements GamePiece (via TranslatablePiece), but UNLIKE traits it is NOT a
 * Decorator, and thus must be treated specially.
 */
public class BasicPiece extends AbstractImageFinder implements TranslatablePiece, StateMergeable, PropertyNameSource, PersistentPropertyContainer,
  PropertyExporter {

  public static final String ID = "piece;"; // NON-NLS
  private static Highlighter highlighter;
  /**
   * Return information about the current location of the piece through getProperty():
   *
   * LocationName - Current Location Name of piece as displayed in Chat Window CurrentX - Current X position CurrentY -
   * Current Y position CurrentMap - Current Map name or "" if not on a map CurrentBoard - Current Board name or "" if
   * not on a map CurrentZone - If the current map has a multi-zoned grid, then return the name of the Zone the piece is
   * in, or "" if the piece is not in any zone, or not on a map
   */
  public static final String LOCATION_NAME = "LocationName"; // NON-NLS
  public static final String CURRENT_MAP = "CurrentMap"; // NON-NLS
  public static final String CURRENT_BOARD = "CurrentBoard"; // NON-NLS
  public static final String CURRENT_ZONE = "CurrentZone"; // NON-NLS
  public static final String CURRENT_X = "CurrentX"; // NON-NLS
  public static final String CURRENT_Y = "CurrentY"; // NON-NLS
  public static final String OLD_LOCATION_NAME = "OldLocationName"; // NON-NLS
  public static final String OLD_MAP = "OldMap"; // NON-NLS
  public static final String OLD_BOARD = "OldBoard"; // NON-NLS
  public static final String OLD_ZONE = "OldZone"; // NON-NLS
  public static final String OLD_X = "OldX"; // NON-NLS
  public static final String OLD_Y = "OldY"; // NON-NLS
  public static final String BASIC_NAME = "BasicName"; // NON-NLS
  public static final String PIECE_NAME = "PieceName"; // NON-NLS
  public static final String LOCALIZED_BASIC_NAME = "LocalizedBasicName"; //NON-NLS
  public static final String LOCALIZED_PIECE_NAME = "LocalizedPieceName"; //NON-NLS
  public static final String DECK_NAME = "DeckName"; // NON-NLS
  public static final String DECK_POSITION = "DeckPosition"; // NON-NLS
  public static final String CLICKED_X = "ClickedX"; // NON-NLS
  public static final String CLICKED_Y = "ClickedY"; // NON-NLS
  public static Font POPUP_MENU_FONT = new Font(Font.DIALOG, Font.PLAIN, 11);
  protected JPopupMenu popup;
  protected Rectangle imageBounds;
  protected ScaledImagePainter imagePainter = new ScaledImagePainter();
  private Map map;
  private KeyCommand[] commands;
  private Stack parent;
  private Point pos = new Point(0, 0);
  private String id;

  /*
   * A set of properties used as scratch-pad storage by various Traits and processes.
   * These properties are ephemeral and not stored in the GameState.
   */
  private java.util.Map<Object, Object> props;

  /*
   * A Set of properties that must be persisted in the GameState.
   * Will be created as lazily as possible since pieces that don't move will not need them,
   * The current code only supports String Keys and Values. Non-strings should be serialised
   * before set and de-serialised after get.
   */
  private java.util.Map<Object, Object> persistentProps;

  /** @deprecated Moved into own traits, retained for backward compatibility */
  @Deprecated
  private char cloneKey;
  /** @deprecated Moved into own traits, retained for backward compatibility */
  @Deprecated
  private char deleteKey;
  /** @deprecated Replaced by #srcOp. */
  @Deprecated
  protected Image image;           // BasicPiece's own image
  protected String imageName;      // BasicPiece image name
  private String commonName;       // BasicPiece's name for the piece (aka "BasicName" property in Vassal Module)

  public BasicPiece() {
    this(ID + ";;;;");
  }

  /** creates a BasicPiece by passing complete type information
   * @param type serialized type information (data about the piece which does not
   * change during the course of a game) ready to be processed by a {@link SequenceEncoder.Decoder} */
  public BasicPiece(String type) {
    mySetType(type);
  }

  /** Sets the type information for this piece.  See {@link Decorator#myGetType}
   *  @param type a serialized configuration string to
   *              set the "type information" of this piece, which is
   *              information that doesn't change during the course of
   *              a single game (e.g. Image Files, Context Menu strings,
   *              etc). Typically ready to be processed e.g. by
   *              SequenceEncoder.decode() */
  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    cloneKey = st.nextChar('\0');
    deleteKey = st.nextChar('\0');
    imageName = st.nextToken();
    commonName = st.nextToken();
    imagePainter.setImageName(imageName);
    imageBounds = null;  // New image, clear the old imageBounds
    commands = null;
  }

  /** @return The "type information" of a piece or trait is information
   * that does not change during the course of a game. Image file
   * names, context menu strings, etc., all should be reflected
   * in the type. The type information is returned serialized string
   * form, ready to be decoded by a SequenceEncoder#decode.
   * @see BasicCommandEncoder */
  @Override
  public String getType() {
    final SequenceEncoder se =
      new SequenceEncoder(cloneKey > 0 ? String.valueOf(cloneKey) : "", ';');
    return ID + se.append(deleteKey > 0 ? String.valueOf(deleteKey) : "")
                  .append(imageName)
                  .append(commonName).getValue();
  }

  /** @param map Each GamePiece belongs to a single {@link Map} */
  @Override
  public void setMap(Map map) {
    if (map != this.map) {
      commands = null;
      this.map = map;
    }
  }

  /** @return Each GamePiece belongs to a single {@link Map} */
  @Override
  public Map getMap() {
    return getParent() == null ? map : getParent().getMap();
  }

  /**
   * Properties can be associated with a piece -- many may be game-specific, but others
   * are standard, such as the LocationName property exposed by BasicPiece -- and can
   * be read through this interface. The properties may or may not need to be encoded in
   * the piece's {@link #getState} method.
   *
   * A request to getProperty() that reaches the BasicPiece will have already checked for
   * such a property key being available from any outer Decorator/Trait in the stack. Upon
   * reaching BasicPiece, the search hierarchy for a matching property now becomes:
   *
   * (1) Specific named properties supported by BasicPiece. These include BASIC_NAME,
   * PIECE_NAME, LOCATION_NAME, CURRENT_MAP, CURRENT_BOARD, CURRENT_ZONE, CURRENT_X,
   * CURRENT_Y.
   * (2) "Scratchpad" properties - see {@link #setProperty} for full details, but these are
   * highly temporary properties intended to remain valid only during the execution of a
   * single key command.
   * (3) Persistent properties - see {@link #setPersistentProperty} for full details, but
   * they are stored in the piece and "game state robust" - saved during save/load, and
   * propagated to other players' clients in a multiplayer game.
   * (4) The values of any visible "Global Property" in a Vassal module, checking the Zone
   * level first, then the map level, and finally the module level.
   *
   * <br><br>Thus, when using this interface a piece's own properties are preferred to those of
   * "Global Properties", and those in turn are searched Zone-first then Map, then Module.
   * @param key String key of property to be returned
   * @return Object containing new value of the specified property
   */
  @Override
  public Object getProperty(Object key) {
    if (BASIC_NAME.equals(key)) {
      return getName();
    }
    else if (LOCALIZED_BASIC_NAME.equals(key)) {
      return getLocalizedName();
    }
    else
        return getPublicProperty(key);
  }

  /**
   * Properties (see {@link #getProperty}) visible in a masked (see {@link Obscurable}) piece, even when the piece is masked.
   * @param key String key of property to be returned.
   */
  public Object getPublicProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getKeyCommands();
    }
    else if (LOCATION_NAME.equals(key)) {
      return getMap() == null ? "" : getMap().locationName(getPosition());
    }
    else if (PIECE_NAME.equals(key)) {
      return Decorator.getOutermost(this).getName();
    }
    else if (LOCALIZED_PIECE_NAME.equals(key)) {
      return Decorator.getOutermost(this).getLocalizedName();
    }
    else if (CURRENT_MAP.equals(key)) {
      return getMap() == null ? "" : getMap().getConfigureName();
    }
    else if (DECK_NAME.equals(key)) {
      return getParent() instanceof Deck ? ((Deck) getParent()).getDeckName() : "";
    }
    else if (DECK_POSITION.equals(key)) {
      if (getParent() instanceof Deck) {
        final Deck deck = (Deck) getParent();
        final int size = deck.getPieceCount();
        final int pos = deck.indexOf(Decorator.getOutermost(this));
        return String.valueOf(size - pos);
      }
      else {
        return "0";
      }
    }
    else if (CURRENT_BOARD.equals(key)) {
      if (getMap() != null) {
        final Board b = getMap().findBoard(getPosition());
        if (b != null) {
          return b.getName();
        }
      }
      return "";
    }
    else if (CURRENT_ZONE.equals(key)) {
      if (getMap() != null) {
        final Zone z = getMap().findZone(getPosition());
        if (z != null) {
          return z.getName();
        }
      }
      return "";
    }
    else if (CURRENT_X.equals(key)) {
      return String.valueOf(getPosition().x);
    }
    else if (CURRENT_Y.equals(key)) {
      return String.valueOf(getPosition().y);
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return "";
    }

    // Check for a property in the scratch-pad properties
    Object prop = props == null ? null : props.get(key);

    // Check for a persistent property
    if (prop == null && persistentProps != null) {
      prop = persistentProps.get(key);
    }

    // Check for higher level properties. Each level if it exists will check the higher level if required.
    if (prop == null) {
      final Map map = getMap();
      final Zone zone = (map == null ? null : map.findZone(getPosition()));
      if (zone != null) {
        prop = zone.getProperty(key);
      }
      else if (map != null) {
        prop = map.getProperty(key);
      }
      else {
        prop = GameModule.getGameModule().getProperty(key);
      }
    }

    return prop;
  }

  /**
   * Returns the localized text for a specified property if a translation is available, otherwise the non-localized version.
   * Searches the same hierarchy of properties as {@link #getProperty}.
   * @param key String key of property to be returned
   * @return localized text of property, if available, otherwise non-localized value
   */
  @Override
  public Object getLocalizedProperty(Object key) {
    if (BASIC_NAME.equals(key)) {
      return getLocalizedName();
    }
    else {
      return getLocalizedPublicProperty(key);
    }
  }

  /**
   * Returns the localized text for a specified property if a translation is available, otherwise the non-localized version,
   * but in both cases accounting for the unit's visibility (i.e. Mask/{@link Obscurable} Traits).
   * Searches the same hierarchy of properties as {@link #getProperty}.
   * @param key String key of property to be returned
   * @return  Returns localized text of property, if available, otherwise non-localized value, accounting for Mask status.
   */
  public Object getLocalizedPublicProperty(Object key) {
    if (List.of(
      Properties.KEY_COMMANDS,
      DECK_NAME,
      CURRENT_X,
      CURRENT_Y,
      Properties.VISIBLE_STATE
    ).contains(key)) {
      return getProperty(key);
    }
    else if (LOCATION_NAME.equals(key)) {
      return getMap() == null ? "" : getMap().localizedLocationName(getPosition());
    }
    else if (PIECE_NAME.equals(key)) {
      return Decorator.getOutermost(this).getName();
    }
    else if (BASIC_NAME.equals(key)) {
      return getLocalizedName();
    }
    else if (CURRENT_MAP.equals(key)) {
      return getMap() == null ? "" : getMap().getLocalizedConfigureName();
    }
    else if (DECK_POSITION.equals(key)) {
      if (getParent() instanceof Deck) {
        final Deck deck = (Deck) getParent();
        final int size = deck.getPieceCount();
        final int pos = deck.indexOf(Decorator.getOutermost(this));
        return String.valueOf(size - pos);
      }
      else {
        return "0";
      }
    }
    else if (CURRENT_BOARD.equals(key)) {
      if (getMap() != null) {
        final Board b = getMap().findBoard(getPosition());
        if (b != null) {
          return b.getLocalizedName();
        }
      }
      return "";
    }
    else if (CURRENT_ZONE.equals(key)) {
      if (getMap() != null) {
        final Zone z = getMap().findZone(getPosition());
        if (z != null) {
          return z.getLocalizedName();
        }
      }
      return "";
    }
    // Check for a property in the scratch-pad properties
    Object prop = props == null ? null : props.get(key);

    // Check for a persistent property
    if (prop == null && persistentProps != null) {
      prop = persistentProps.get(key);
    }

    // Check for higher level properties. Each level if it exists will check the higher level if required.
    if (prop == null) {
      final Map map = getMap();
      final Zone zone = (map == null ? null : map.findZone(getPosition()));
      if (zone != null) {
        prop = zone.getLocalizedProperty(key);
      }
      else if (map != null) {
        prop = map.getLocalizedProperty(key);
      }
      else {
        prop = GameModule.getGameModule().getLocalizedProperty(key);
      }
    }
    return prop;
  }

  /**
   * Properties can be associated with a piece -- many may be game-specific, but others
   * are standard, such as the LocationName property exposed by BasicPiece -- and can
   * be set through this interface. The properties may or may not need to be encoded in
   * the piece's {@link #getState} method.
   *
   * A setProperty() call which reaches BasicPiece will already have passed through all of the outer
   * Decorator/Traits on the way in without finding one able to match the property.
   *
   * <br><br><b>NOTE:</b> Properties outside the piece CANNOT be set by this  method (e.g. Global
   * Properties), even though they can be read by {@link #getProperty} -- in this the two methods are
   * not perfect mirrors. This method ALSO does not set persistent properties (they can only be set
   * by an explicit call to  {@link #setPersistentProperty}).
   *
   * <br><br>BasicPiece <i>does</i>, however contain a "scratchpad" for temporary properties, and for
   * any call to this method that does not match a known property (which is, currently, ANY call which
   * reaches this method here in BasicPiece), a scratchpad property will be set. Scratchpad properties
   * are NOT saved when the game is saved, and NO arrangement is made to pass their values to other
   * players' machines. Thus they should only be used internally for highly temporary values during the
   * execution of a single key command. Their one other use is to store the piece's Unique ID -- and
   * although this value is obviously used over periods of time much longer than a single key command,
   * this is possible because the value is immutable and is refreshed to the same  value whenever the
   * piece is re-created e.g. when loading a save.
   *
   * @param key String key of property to be changed
   * @param val Object containing new value of the property
   */
  @Override
  public void setProperty(Object key, Object val) {
    if (props == null) {
      props = new HashMap<>();
    }
    if (val == null) {
      props.remove(key);
    }
    else {
      props.put(key, val);
    }
  }

  /**
   * Setting a persistent property writes a property value into the piece (creating a new entry in the piece's persistent
   * property table if the specified key does not yet exist in it). Persistent properties are game-state-robust: they are
   * saved/restored with saved games, and are passed via {@link Command} to other players' clients in a multiplayer game.
   * The persistent property value can then be read from the piece via e.g. {@link #getProperty}. When reading back properties
   * out of a piece, the piece's built-in properties are checked first, then scratchpad properties (see {@link #setProperty}),
   * then external properties such as Global Properties. If <i>only</i> persistentProperties are to be searched, use
   * {@link #getPersistentProperty} instead.
   *
   * <br><br>In practical terms, setPersistentProperty is used mainly to implement the "Old" properties of BasicPiece (e.g.
   * "OldLocationName", "OldZone", "OldMap", "OldBoard", "OldX", "OldY"). A Persistent Property is indeed nearly identical
   * with {@link DynamicProperty} in storage/retrieval characteristics, and simply lacks the in-module interface for setting
   * values, etc. Module Designers are thus recommended to stick with Dynamic Property traits for these functions.
   *
   * @param key String key naming the persistent property to be set. If a corresponding persistent property does not exist it will be created.
   * @param newValue New value for the persistent property
   * @return a {@link Command} object which, when passed to another player's client via logfile, server, or saved game, will allow the
   * result of the "set" operation to be replicated.
   */
  @Override
  public Command setPersistentProperty(Object key, Object newValue) {
    if (persistentProps == null) {
      persistentProps = new HashMap<>();
    }

    final Object oldValue = newValue == null ? persistentProps.remove(key) : persistentProps.put(key, newValue);
    return Objects.equals(oldValue, newValue) ? null : new SetPersistentPropertyCommand(getId(), key, oldValue, newValue);
  }

  /**
   * @param key String key naming the persistent property whose value is to be returned.
   * @return the current value of a persistent property, or null if it doesn't exist.
   */
  @Override
  public Object getPersistentProperty(Object key) {
    return persistentProps == null ? null : persistentProps.get(key);
  }

  /**
   * @param s Name of a module preference to be read
   * @return Value of the preference
   */
  protected Object prefsValue(String s) {
    return GameModule.getGameModule().getPrefs().getValue(s);
  }

  /**
   * Draws the BasicPiece's image, if it has been set
   * @param g target Graphics object
   * @param x x-location of the center of the piece
   * @param y y-location of the center of the piece
   * @param obs the Component on which this piece is being drawn
   * @param zoom the scaling factor.
   */
  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (imageBounds == null) {
      imageBounds = boundingBox();
    }
    imagePainter.draw(g, x + (int) (zoom * imageBounds.x), y + (int) (zoom * imageBounds.y), zoom, obs);
  }

  /**
   * @return the set of key commands that will populate the a BasicPiece's right-click menu.
   * This will normally be an empty array in the present age of the world, but the ability to contain a
   * clone and delete command is retained for compatibility with Modules Of Ancient Times.
   * In the case of BasicPiece, this method also keeps track of whether move up/down/to-top/to-bottom commands are enabled.
   *
   * This method is chained from "outer" Decorator components of a larger logical game piece, in the process of generating
   * the complete list of key commands to build the right-click menu -- this process is originated by calling <code>getKeyCommands()</code>
   * on the piece's outermost Decorator/Trait.
   */
  protected KeyCommand[] getKeyCommands() {
    if (commands == null) {
      final ArrayList<KeyCommand> l = new ArrayList<>();
      final GamePiece target = Decorator.getOutermost(this);
      if (cloneKey > 0) {
        l.add(new KeyCommand(Resources.getString("Editor.Clone.clone"), KeyStroke.getKeyStroke(cloneKey, InputEvent.CTRL_DOWN_MASK), target));
      }
      if (deleteKey > 0) {
        l.add(new KeyCommand(Resources.getString("Editor.Delete.delete"), KeyStroke.getKeyStroke(deleteKey, InputEvent.CTRL_DOWN_MASK), target));
      }
      commands = l.toArray(new KeyCommand[0]);
    }
    final GamePiece outer = Decorator.getOutermost(this);
    // This code has no function that I can see? There is no way to add these Commands.
    // boolean canAdjustPosition = outer.getMap() != null && outer.getParent() != null && outer.getParent().topPiece() != getParent().bottomPiece();
    // enableCommand("Move up", canAdjustPosition);
    // enableCommand("Move down", canAdjustPosition);
    // enableCommand("Move to top", canAdjustPosition);
    // enableCommand("Move to bottom", canAdjustPosition);
    enableCommand(Resources.getString("Editor.Clone.clone"), outer.getMap() != null);
    enableCommand(Resources.getString("Editor.Delete.delete"), outer.getMap() != null);
    return commands;
  }

  /**
   * @param name Name of internal-to-BasicPiece key command whose enabled status it to be set
   * @param enable true to enable, false to disable.
   */
  private void enableCommand(String name, boolean enable) {
    for (final KeyCommand command : commands) {
      if (name.equals(command.getName())) {
        command.setEnabled(enable);
      }
    }
  }

  /**
   * @param stroke KeyStroke to query if corresponding internal-to-BasicPiece command is enabled
   * @return false if no Keystroke, true if not an internal-to-BasicPiece key command, and enabled status of key command otherwise.
   */
  private boolean isEnabled(KeyStroke stroke) {
    if (stroke == null) {
      return false;
    }
    for (final KeyCommand command : commands) {
      if (stroke.equals(command.getKeyStroke())) {
        return command.isEnabled();
      }
    }
    return true;
  }

  /**
   * @return piece's position on its map.
   */
  @Override
  public Point getPosition() {
    return getParent() == null ? new Point(pos) : getParent().getPosition();
  }

  /**
   * @param p Sets the location of this piece on its {@link Map}
   */
  @Override
  public void setPosition(Point p) {
    if (getMap() != null && getParent() == null) {
      getMap().repaint(getMap().boundingBoxOf(Decorator.getOutermost(this)));
    }
    pos = p;
    if (getMap() != null && getParent() == null) {
      getMap().repaint(getMap().boundingBoxOf(Decorator.getOutermost(this)));
    }
  }

  /**
   * @return the parent {@link Stack} of which this piece is a member, or null if not a member of any Stack
   */
  @Override
  public Stack getParent() {
    return parent;
  }

  /**
   * @param s sets the {@link Stack} to which this piece belongs.
   */
  @Override
  public void setParent(Stack s) {
    parent = s;
  }

  /**
   * @return bounding box rectangle for BasicPiece's image, if an image has been specified.
   */
  @Override
  public Rectangle boundingBox() {
    if (imageBounds == null) {
      imageBounds = ImageUtils.getBounds(imagePainter.getImageSize());
    }
    return new Rectangle(imageBounds);
  }

  /**
   * @return the Shape of this piece, for purposes of selecting it by clicking on it with the mouse. In the case
   * of BasicPiece, this is equivalent to the boundingBox of the BasicPiece image, if one exists. Note that the
   * shape should be defined in reference to the piece's location, which is ordinarily the center of the basic
   * image.
   *
   * <br><br>For pieces that need a non-rectangular click volume, add a {@link NonRectangular} trait.
   */
  @Override
  public Shape getShape() {
    return boundingBox();
  }

  /**
   * @param c GamePiece to check if equal to this one
   * @return Equality check with specified game piece
   */
  public boolean equals(GamePiece c) {
    return c == this;
  }

  /**
   * @return the name of this GamePiece. This is the name typed by the module designer in the configuration box
   * for the BasicPiece.
   */
  @Override
  public String getName() {
    return commonName;
  }

  /**
   * @return the localized name of this GamePiece. This is the translated version of the name typed by the module designer
   * in the configuration box for the BasicPiece. It is used to fill the "BasicName" property.
   */
  @Override
  public String getLocalizedName() {
    final String key = TranslatablePiece.PREFIX + getName();
    return Localization.getInstance().translate(key, getName());
  }

  /**
   * The primary way for the piece or trait to receive events. {@link KeyStroke} events are forward
   * to this method if they are received while the piece is selected (or as the result of e.g. a Global
   * Key Command being sent to the piece). The class implementing GamePiece can respond in any way it
   * likes. Actual key presses by the player, selected  items from the right-click Context Menu, keystrokes
   * "applied on move" by a Map that the piece has just moved on, and Global Key Commands all send KeyStrokes
   * (and NamedKeyStrokes) which are passed to pieces and traits through this interface.
   *
   * <br><br>In the case of BasicPiece, if a key command gets here, that means it has already been seen by any and all of
   * its Traits ({@link Decorator}s), as BasicPiece is the innermost member of the Decorator stack. The key events
   * processed here by BasicPiece include the "move up"/"move down"/"move-to-top"/"move-to-bottom" stack-adjustment
   * commands, along with legacy support for cloning and deleting.
   *
   * @return a {@link Command} that, when executed, will make all changes to the game state (maps, pieces, other
   * pieces, etc) to duplicate what the piece did in response to this event on another machine. Often a
   * {@link ChangePiece} command, but for example if this keystroke caused the piece/trait to decide to fire
   * off a Global Key Command, then the Command returned would include the <i>entire</i> results of that, appended
   * as subcommands.
   *
   * @see VASSAL.build.module.map.ForwardToKeyBuffer
   */
  @Override
  public Command keyEvent(KeyStroke stroke) {
    getKeyCommands();
    if (!isEnabled(stroke)) {
      return null;
    }
    Command comm = null;
    final GamePiece outer = Decorator.getOutermost(this);
    if (cloneKey != 0 && KeyStroke.getKeyStroke(cloneKey, InputEvent.CTRL_DOWN_MASK).equals(stroke)) {
      final GamePiece newPiece = ((AddPiece) GameModule.getGameModule().decode(GameModule.getGameModule().encode(new AddPiece(outer)))).getTarget();
      newPiece.setId(null);
      GameModule.getGameModule().getGameState().addPiece(newPiece);
      newPiece.setState(outer.getState());
      comm = new AddPiece(newPiece);
      if (getMap() != null) {
        comm.append(getMap().placeOrMerge(newPiece, getPosition()));
        KeyBuffer.getBuffer().remove(outer);
        KeyBuffer.getBuffer().add(newPiece);
        if (GlobalOptions.getInstance().autoReportEnabled() && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
          final String name = outer.getLocalizedName();
          final String loc = getMap().locationName(outer.getPosition());
          final String s;
          if (loc != null) {
            s = Resources.getString("BasicPiece.clone_report_1", name, loc);
          }
          else {
            s = Resources.getString("BasicPiece.clone_report_2", name);
          }
          final Command report = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), s);
          report.execute();
          comm = comm.append(report);
        }
      }
    }
    else if (deleteKey != 0 && KeyStroke.getKeyStroke(deleteKey, InputEvent.CTRL_DOWN_MASK).equals(stroke)) {
      comm = new RemovePiece(outer);
      if (getMap() != null && GlobalOptions.getInstance().autoReportEnabled() && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
        final String name = outer.getLocalizedName();
        final String loc = getMap().locationName(outer.getPosition());
        final String s;
        if (loc != null) {
          s = Resources.getString("BasicPiece.delete_report_1", name, loc);
        }
        else {
          s = Resources.getString("BasicPiece.delete_report_2", name);
        }
        final Command report = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), s);
        comm = comm.append(report);
      }
      comm.execute();
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveUpKey())) {
      if (parent != null) {
        final String oldState = parent.getState();
        final int index = parent.indexOf(outer);
        if (index < parent.getPieceCount() - 1) {
          parent.insert(outer, index + 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToFront(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToFront(outer);
      }
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveDownKey())) {
      if (parent != null) {
        final String oldState = parent.getState();
        final int index = parent.indexOf(outer);
        if (index > 0) {
          parent.insert(outer, index - 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToBack(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToBack(outer);
      }
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveTopKey())) {
      parent = outer.getParent();
      if (parent != null) {
        final String oldState = parent.getState();
        if (parent.indexOf(outer) < parent.getPieceCount() - 1) {
          parent.insert(outer, parent.getPieceCount() - 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToFront(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToFront(outer);
      }
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveBottomKey())) {
      parent = getParent();
      if (parent != null) {
        final String oldState = parent.getState();
        if (parent.indexOf(outer) > 0) {
          parent.insert(outer, 0);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToBack(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToBack(outer);
      }
    }
    return comm;
  }

  /**
   * @return The "state information" is information that can change during
   * the course of a game. State information is saved when the game
   * is saved and is transferred between players on the server. For
   * example, the relative order of pieces in a stack is state
   * information, but whether the stack is expanded is not.
   *
   * <br><br>In the case of BasicPiece, the state information includes the current
   * map, x/y position, the unique Game Piece ID, and the keys and values
   * of any persistent properties (see {@link #setPersistentProperty})
   */
  @Override
  public String getState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    final String mapName = map == null ? "null" : map.getIdentifier(); // NON-NLS
    se.append(mapName);
    final Point p = getPosition();
    se.append(p.x).append(p.y);
    se.append(getGpId());
    se.append(persistentProps == null ? 0 : persistentProps.size());
    // Persistent Property values will always be String (for now).
    if (persistentProps != null) {
      persistentProps.forEach((key, val) -> {
        se.append(key == null ? "" : key.toString());
        se.append(val == null ? "" : val.toString());
      });
    }
    return se.getValue();
  }

  /**
   * @param s New state information serialized in string form, ready
   * to be passed to a SequenceEncoder#decode. The "state information" is
   * information that can change during the course of a game. State information
   * is saved when the game is saved and is transferred between players on the
   * server. For example, the relative order of pieces in a stack is state
   * information, but whether the stack is expanded is not.
   *
   * <br><br>In the case of BasicPiece, the state information includes the current
   * map, x/y position, the unique Game Piece ID, and the keys and values
   * of any persistent properties (see {@link #setPersistentProperty})
   */
  @Override
  public void setState(String s) {
    final GamePiece outer = Decorator.getOutermost(this);
    final Map oldMap = getMap();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    final String mapId = st.nextToken();
    Map newMap = null;
    if (!"null".equals(mapId)) { // NON-NLS
      newMap = Map.getMapById(mapId);
      if (newMap == null) {
        Decorator.reportDataError(this, Resources.getString("Error.not_found", "Map"), "mapId=" + mapId); // NON-NLS
      }
    }
    final Point newPos = new Point(st.nextInt(0), st.nextInt(0));
    setPosition(newPos);
    if (newMap != oldMap) {
      if (newMap != null) {
        // This will remove from oldMap
        // and set the map to newMap
        newMap.addPiece(outer);
      }
      else { // oldMap can't possibly be null if we get to here
        oldMap.removePiece(outer);
        setMap(null);
      }
    }
    setGpId(st.nextToken(""));

    // Persistent Property values will always be String (for now).
    // Create the HashMap as lazily as possible, no point in creating it for pieces that never move
    if (persistentProps != null) {
      persistentProps.clear();
    }
    final int propCount = st.nextInt(0);
    for (int i = 0; i < propCount; i++) {
      if (persistentProps == null) {
        persistentProps = new HashMap<>();
      }
      final String key = st.nextToken("");
      final String val = st.nextToken("");
      persistentProps.put(key, val);
    }
  }

  /**
   * For BasicPiece, the "merge" of a new state simply involves copying in the
   * new one in its entirety -- if any difference is detected.
   * @param newState new serialized game state string
   * @param oldState old serialized game state string
   */
  @Override
  public void mergeState(String newState, String oldState) {
    if (!newState.equals(oldState)) {
      setState(newState);
    }
  }

  /**
   * Each GamePiece must have a unique String identifier. These are managed by VASSAL internally and should never
   * be changed by custom code.
   * @return unique ID for this piece
   * @see GameState#getNewPieceId
   */
  @Override
  public String getId() {
    return id;
  }

  /**
   * Each GamePiece must have a unique String identifier. These are managed by VASSAL internally and should never
   * be changed by custom code.
   * @param id sets unique ID for this piece
   * @see GameState#getNewPieceId
   */
  @Override
  public void setId(String id) {
    this.id = id;
  }

  /**
   * @return the Highlighter instance for drawing selected pieces. Note that since this is a static method, all pieces in a
   * module will always use the same Highlighter
   */
  public static Highlighter getHighlighter() {
    if (highlighter == null) {
      highlighter = new ColoredBorder();
    }
    return highlighter;
  }

  /**
   * @param h Set the Highlighter for all pieces
   */
  public static void setHighlighter(Highlighter h) {
    highlighter = h;
  }

  /**
   * @return Description of what this kind of piece is. Appears in PieceDefiner list of traits.
   */
  @Override
  public String getDescription() {
    return Resources.getString("Editor.BasicPiece.trait_description");
  }

  /**
   * @return the unique gamepiece ID for this piece, as stored in the Property "scratchpad"
   */
  public String getGpId() {
    final String id = (String) getProperty(Properties.PIECE_ID);
    return id == null ? "" : id;
  }

  /**
   * @param id stores the unique gamepiece ID for this piece into the Property "scratchpad"
   */
  public void setGpId(String id) {
    setProperty(Properties.PIECE_ID, id == null ? "" : id);
  }

  /**
   * @return the help file page for this type of piece.
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("BasicPiece.html"); // NON-NLS
  }

  /**
   * @return The configurer ({@link PieceEditor} for the BasicPiece, which generates the dialog for editing the
   * BasicPiece's type information in the Editor window.
   */
  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * Test if this BasicPiece's Type and State are equal to another
   * This method is intended to be used by Unit Tests to verify that a trait
   * is unchanged after going through a process such as serialization/deserialization.
   *
   * @param o Object to compare this Decorator to
   * @return true if the Class, type and state all match
   */
  public boolean testEquals(Object o) {

    // Check Class type
    if (! (o instanceof BasicPiece)) return false;
    final BasicPiece bp = (BasicPiece) o;

    // Check Type
    if (! Objects.equals(cloneKey, bp.cloneKey)) return false;
    if (! Objects.equals(deleteKey, bp.deleteKey)) return false;
    if (! Objects.equals(imageName, bp.imageName)) return false;
    if (! Objects.equals(commonName, bp.commonName)) return false;

    // Check State
    final String mapName1 = this.map == null ? "null" : this.map.getIdentifier(); // NON-NLS
    final String mapName2 = bp.map == null ? "null" : bp.map.getIdentifier(); // NON-NLS
    if (! Objects.equals(mapName1, mapName2)) return false;

    if (! Objects.equals(getPosition(), bp.getPosition())) return false;
    if (! Objects.equals(getGpId(), bp.getGpId())) return false;

    final int pp1 = persistentProps == null ? 0 : persistentProps.size();
    final int pp2 = bp.persistentProps == null ? 0 : bp.persistentProps.size();
    if (! Objects.equals(pp1, pp2)) return false;

    if (persistentProps != null && bp.persistentProps != null) {
      for (final Object key : persistentProps.keySet()) {
        if (!Objects.equals(persistentProps.get(key), bp.persistentProps.get(key)))
          return false;
      }
    }

    return true;
  }

  /**
   * The configurer ({@link PieceEditor} for the BasicPiece, which generates the dialog for editing the
   * BasicPiece's type information in the Editor window.
   */
  private static class Ed implements PieceEditor {
    private TraitConfigPanel panel;
    private KeySpecifier cloneKeyInput;
    private KeySpecifier deleteKeyInput;
    private StringConfigurer pieceName;
    private ImageSelector picker;
    private final String state;

    /**
     * @param p to create PieceEditor for
     */
    private Ed(BasicPiece p) {
      state = p.getState();
      initComponents(p);
    }

    /**
     * @param p initializes the editor dialog for the specified BasicPiece
     */
    private void initComponents(BasicPiece p) {
      panel = new TraitConfigPanel();

      pieceName = new StringConfigurer(p.commonName);
      panel.add("Editor.name_label", pieceName);

      cloneKeyInput = new KeySpecifier(p.cloneKey);
      if (p.cloneKey != 0) {
        panel.add(new JLabel(Resources.getString("Editor.BasicPiece.to_clone")));
        panel.add(cloneKeyInput);
      }

      deleteKeyInput = new KeySpecifier(p.deleteKey);
      if (p.deleteKey != 0) {
        panel.add(new JLabel(Resources.getString("Editor.BasicPiece.to_delete")));
        panel.add(deleteKeyInput);
      }

      picker = new ImageSelector(p.imageName);
      panel.add("Editor.image_label", picker);

    }

    /**
     * @param p BasicPiece
     */
    public void reset(BasicPiece p) {
    }

    /**
     * @return the Component for the BasicPiece configurer
     */
    @Override
    public Component getControls() {
      return panel;
    }

    /**
     * @return the current state string for the BasicPiece
     */
    @Override
    public String getState() {
      return state;
    }

    /**
     * @return the type information string for the BasicPiece based on the current values of the configurer fields
     */
    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(cloneKeyInput.getKey(), ';');
      final String type = se.append(deleteKeyInput.getKey()).append(picker.getValueString()).append(pieceName.getValueString()).getValue();
      return BasicPiece.ID + type;
    }
  }

  /**
   * @return String enumeration of type and state information.
   */
  @Override
  public String toString() {
    return super.toString() + "[name=" + getName() + ",type=" + getType() + ",state=" + getState() + "]"; // NON-NLS
  }

  /**
   * @return Object encapsulating the internationalization data for the BasicPiece
   */
  @Override
  public PieceI18nData getI18nData() {
    final PieceI18nData data = new PieceI18nData(this);
    data.add(commonName, Resources.getString("Editor.BasicPiece.basic_piece_name_description"));
    return data;
  }

  /**
   * @return Property names exposed by the Trait or Piece. In the case of BasicPiece, there are quite a few, mainly
   * dealing with past and present location.
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(LOCATION_NAME);
    l.add(CURRENT_MAP);
    l.add(CURRENT_BOARD);
    l.add(CURRENT_ZONE);
    l.add(CURRENT_X);
    l.add(CURRENT_Y);
    l.add(OLD_LOCATION_NAME);
    l.add(OLD_MAP);
    l.add(OLD_BOARD);
    l.add(OLD_ZONE);
    l.add(OLD_X);
    l.add(OLD_Y);
    l.add(BASIC_NAME);
    l.add(PIECE_NAME);
    l.add(DECK_NAME);
    l.add(CLICKED_X);
    l.add(CLICKED_Y);
    return l;
  }

  /**
   * See {@link AbstractImageFinder}
   * Adds our image (if any) to the list of images
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (imageName != null) s.add(imageName);
  }
}
