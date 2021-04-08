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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.CompoundPieceCollection;
import VASSAL.build.module.map.PieceCollection;
import VASSAL.build.module.map.StackMetrics;
import VASSAL.command.Command;
import VASSAL.tools.EnumeratedIterator;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.SequenceEncoder;
import VASSAL.search.AbstractImageFinder;
import VASSAL.search.ImageSearchTarget;

/**
 * A Stack is a collection of pieces in the same location that can often be moved with a single drag-and-drop.
 * Because it implements the {@link GamePiece} interface, a Stack is formally a kind of GamePiece itself, which
 * can lead to confusion when the terms pieces, GamePieces, etc are used loosely/interchangeably. The kind of "pieces"
 * a Stack contains are the "regular" kind of pieces that have a {@link BasicPiece} plus an optional group of
 * {@link Decorator} traits.
 * <br><br>
 * A standard Stack will only contain pieces that are "stackable" (i.e. doesn't have a "Does Not Stack" {@link Immobilized}
 * trait, so that {@link Properties.NO_STACK} is false) and share the same X/Y position on the same {@link Map}, and all
 * stackable pieces on a {@link Map} will always be part of <i>some</i> Stack -- even single stackable pieces will have
 * a Stack created to contain them. Stacks <i>should</i> contain only pieces from the same visual layer (see
 * {@link VASSAL.build.module.map.LayeredPieceCollection}), but presently bad behaviors can still develop (e.g. a piece
 * uses a Dynamic Property to control its Game Piece Layer, and ends up changing layers without the Stack noticing) --
 * ideally we should straighten that out in future versions.
 * <br><br>
 * {@link Deck} is a further extension of Stack which utilizes the piece-grouping code but overrides other aspects
 * (e.g. can allow pieces regardless of their "stackability", has different drag-and-drop behavior, etc) to create
 * a different kind of grouping.
 */
public class Stack extends AbstractImageFinder implements GamePiece, StateMergeable {
  public static final String TYPE = "stack"; //$NON-NLS-1$//
  public static final String HAS_LAYER_MARKER = "@@"; // Horrific encoding hack necessitated by equally horrific legacy encoder
  public static final int LAYER_NOT_SET = -1; // Brand new stacks with no pieces do not yet know their visual layer
  protected static final int INCR = 5;

  protected GamePiece[] contents = new GamePiece[INCR]; // array of GamePieces contained by the stack
  protected int pieceCount = 0;        // Number of pieces currently in the stack

  protected Map map;                   // Map that the stack is on
  protected Point pos = new Point(0, 0); // X/Y position of the stack on its map. All pieces in a stack always share the same X/Y position
  protected int layer = LAYER_NOT_SET; // Visual layer for this stack. Once the first piece is added, it is bound permanently.
  private boolean expanded = false;    // Is stack currently visually expanded by the player (for easier viewing, and/or to drag individual pieces)

  private String id;

  private static StackMetrics defaultMetrics;

  public Stack() {
    this(null);
  }

  /**
   * Creates a Stack to contain a specific stackable piece.
   * @param p piece to make a stack for
   */
  public Stack(GamePiece p) {
    if (p != null) {
      setMap(p.getMap());
      setPosition(new Point(p.getPosition()));
      add(p);
    }
  }

  public Iterator<GamePiece> getPiecesIterator() {
    return new AllPieceIterator();
  }

  /**
   * @return an Enumeration of the pieces in the stack, from the bottom up This
   *         is a clone of the contents so add/remove operations during read
   *         won't affect it.
   * @deprecated use {@link #asList()}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Enumeration<GamePiece> getPieces() {
    ProblemDialog.showDeprecated("2020-08-06");
    return new EnumeratedIterator<>(new AllPieceIterator());
  }

  /**
   * A list of the pieces in the stack.
   * @return a {@link List} which is a defensive copy of {@link GamePiece}s
   * contained in this {@link Stack}
   */
  public List<GamePiece> asList() {
    return new ArrayList<>(Arrays.asList(contents).subList(0, pieceCount));
  }

  public Iterator<GamePiece> getPiecesReverseIterator() {
    return new ReversePieceIterator();
  }

  /**
   * Return an enumeration of the pieces in the start, from the top down
   *
   * @return Reverse order Enumerator
   * @deprecated Use {@link #getPiecesInVisibleOrderIterator()}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Enumeration<GamePiece> getPiecesInReverseOrder() {
    ProblemDialog.showDeprecated("2020-08-06");
    return new EnumeratedIterator<>(new ReversePieceIterator());
  }

  /**
   * Returns pieces in the order in which they are visible to the player --
   * topmost first In other words, selected pieces first, then unselected pieces
   * from the top to the bottom.
   * @return iterator
   */
  public Iterator<GamePiece> getPiecesInVisibleOrderIterator() {
    return new VisibleOrderIterator();
  }

  /**
   * Returns pieces in the order in which they are visible to the player --
   * topmost first In other words, selected pieces first, then unselected pieces
   * from the top to the bottom.
   * @deprecated Use {@link #getPiecesInVisibleOrderIterator()}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Enumeration<GamePiece> getPiecesInVisibleOrder() {
    ProblemDialog.showDeprecated("2020-08-06");
    return new EnumeratedIterator<>(new VisibleOrderIterator());
  }


  /**
   * @return the visual layer we're bound to, or LAYER_NOT_SET if it we haven't been bound yet. Keeps the stack oriented to its correct Game Piece Layer - see {@link VASSAL.build.module.map.LayeredPieceCollection}
   */
  public int getLayer() {
    return layer;
  }

  /**
   * @param p Piece to remove from the stack
   */
  public void remove(GamePiece p) {
    removePieceAt(indexOf(p));
    p.setParent(null);
    if (getMap() != null) {
      getMap().repaint();
    }
  }

  /**
   * @param index Index of piece to remove from the stack
   */
  protected void removePieceAt(int index) {
    if (index >= 0 && index < pieceCount) {
      pieceCount--;
      for (int i = index; i < pieceCount; ++i) {
        contents[i] = contents[i + 1];
      }
      expanded = expanded && pieceCount > 1;
    }
  }

  /**
   * Perform some action on a GamePiece that has just been removed this Stack
   * @param p GamePiece
   * @return a {@link Command} that performs the equivalent action when executed
   */
  public Command pieceRemoved(GamePiece p) {
    return null;
  }

  /**
   * Insert a piece at a particular point in the stack
   * @param p piece to insert
   * @param index place to insert it
   */
  protected void insertPieceAt(GamePiece p, int index) {
    if (index < 0) {
      index = 0;
    }
    else if (index > pieceCount) {
      index = pieceCount;
    }

    if (pieceCount >= contents.length) {
      final GamePiece[] newContents = new GamePiece[contents.length + INCR];
      System.arraycopy(contents, 0, newContents, 0, pieceCount);
      contents = newContents;
    }

    for (int i = pieceCount; i > index; --i) {
      contents[i] = contents[i - 1];
    }

    contents[index] = p;
    pieceCount++;
  }

  /**
   * Marks the stack as empty
   */
  public void removeAll() {
    pieceCount = 0;
    expanded = false;
  }

  /**
   * Finds the index of a piece in the stack
   * @param p Piece to locate
   * @return The index of the piece, or -1 if it is not present in the stack
   */
  public int indexOf(GamePiece p) {
    int index = -1;
    for (int i = 0; i < pieceCount; ++i) {
      if (p == contents[i]) {
        index = i;
        break;
      }
    }
    return index;
  }

  /**
   * @param index Index in the stack
   * @return the piece at the specified index
   */
  public GamePiece getPieceAt(int index) {
    return contents[index];
  }

  /**
   * Adds a piece to the stack. If the piece already exists in the stack, moves
   * it to the top
   *
   * @param c Stack to add piece to
   */
  public void add(GamePiece c) {
    if ((pieceCount == 0) && (layer == LAYER_NOT_SET)) {
      final Map m = getMap();
      if (m != null) {
        final PieceCollection p = m.getPieceCollection();
        if (p instanceof CompoundPieceCollection) {
          layer = ((CompoundPieceCollection) p).getLayerForPiece(c); //BR// Bind our stack to the layer of the first piece added
        }
      }
    }
    //FIXME - really, if at this point "layer" is set and the new piece wants to be in a different layer, that's BAD and will produce buggy behavior. But we are presently quietly ignoring that because of all the buggy stacks created in the past.
    insert(c, pieceCount);
  }

  /**
   * Adds a GamePiece to this Stack. Slightly more efficient than
   * {@link #insert} because it assumes the piece does not already belong to
   * this Stack.
   *
   * @param child GamePiece to insert
   * @param index Insert Index
   */
  public void insertChild(GamePiece child, int index) {
    if (child.getParent() != null) {
      child.getParent().remove(child);
    }
    else if (child.getMap() != null) {
      child.getMap().removePiece(child);
    }
    child.setParent(this);
    insertPieceAt(child, index);
  }

  /**
   * @return the number of pieces in the stack
   */
  public int getPieceCount() {
    return pieceCount;
  }

  /**
   * Return the number of pieces that could possible be drawn in the stack, regardless of visibility to any particular player
   * @return Piece Count
   */
  public int getMaximumVisiblePieceCount() {
    return pieceCount;
  }

  /**
   * Inserts a child GamePiece at a given index. If the child piece already
   * belongs to this Stack, it will be repositioned to the given index.
   *
   * @param p GamePiece to insert
   * @param pos Insert position
   */
  public void insert(GamePiece p, int pos) {
    if (p == null) {
      return;
    }
    pos = Math.max(pos, 0);
    pos = Math.min(pos, pieceCount);
    final int index = indexOf(p);
    if (index >= 0) {
      final boolean origExpanded = isExpanded(); // Bug #2766794
      if (pos > index) {
        insertPieceAt(p, pos + 1);
        removePieceAt(index);
      }
      else {
        removePieceAt(index);
        insertPieceAt(p, pos);
      }
      setExpanded(origExpanded);
    }
    else {
      insertChild(p, pos);
    }
  }

  /**
   * Perform some action on a GamePiece that has just been added to this Stack
   * @param p Game Piece
   * @return a {@link Command} that performs the equivalent action when executed
   */
  public Command pieceAdded(GamePiece p) {
    return null;
  }

  /**
   * If the <code>obs</code> parameter is a {@link Map}, delegate drawing of
   * this Stack to the {@link StackMetrics} of that Map. If <code>obs</code>
   * is not a Map, use the default StackMetrics
   *
   * @see StackMetrics#draw
   * @see #getDefaultMetrics
   */
  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (obs instanceof Map.View) {
      ((Map.View) obs).getMap().getStackMetrics().draw(this, g, x, y, obs, zoom);
    }
    else {
      getDefaultMetrics().draw(this, g, x, y, obs, zoom);
    }
  }

  /**
   * @param localized if true, use the localized names
   * @return a comma-separated list of the names of the pieces in this Stack
   */
  public String getName(boolean localized) {
    final StringBuilder val = new StringBuilder();
    final PieceIterator visibleFilter =
      PieceIterator.visible(getPiecesReverseIterator());
    while (visibleFilter.hasMoreElements()) {
      final GamePiece p = visibleFilter.nextPiece();
      val.append(localized ? p.getLocalizedName() : p.getName());
      if (val.length() > 0 && visibleFilter.hasMoreElements()) {
        val.append(", ");
      }
    }
    return val.toString();
  }

  /**
   * @return a comma-separated list of the (non-localized) names of the pieces in this Stack
   */
  @Override
  public String getName() {
    return getName(false);
  }

  /**
   * @return a comma-separated list of the (localized) names of the pieces in this Stack
   */
  @Override
  public String getLocalizedName() {
    return getName(true);
  }

  /**
   * @return bounding box for the stack (minimum rectangle to contain the bounding boxes of all the pieces inside)
   */
  @Override
  public Rectangle boundingBox() {
    final Rectangle r = new Rectangle();
    final Rectangle[] childBounds = new Rectangle[getPieceCount()];
    getMap().getStackMetrics().getContents(this, null, null, childBounds, 0, 0);

    asList().stream()
            .filter(PieceIterator.VISIBLE)
            .forEach(p -> r.add(childBounds[indexOf(p)]));

    return r;
  }

  /**
   * @return shape for the stack (shape to contain the shapes of all the pieces inside)
   */
  @Override
  public Shape getShape() {
    final Area a = new Area();
    final Shape[] childBounds = new Shape[getPieceCount()];
    final StackMetrics metrics = getMap() == null ? getDefaultMetrics() : getMap().getStackMetrics();
    metrics.getContents(this, null, childBounds, null, 0, 0);
    asList().stream()
            .filter(PieceIterator.VISIBLE)
            .forEach(p -> a.add(new Area(childBounds[indexOf(p)])));

    return a;
  }

  /**
   * Finds and selects (in the UI) the next piece in the stack after this one
   * @param c Starting piece
   */
  public void selectNext(GamePiece c) {
    KeyBuffer.getBuffer().remove(c);
    if (pieceCount > 1 && indexOf(c) >= 0) {
      final int newSelectedIndex = indexOf(c) == pieceCount - 1 ? pieceCount - 2 : indexOf(c) + 1;
      for (int i = 0; i < pieceCount; ++i) {
        if (indexOf(contents[i]) == newSelectedIndex) {
          KeyBuffer.getBuffer().add(contents[i]);
          return;
        }
      }
    }
  }

  /**
   * Finds the piece "underneath" the one provided
   * @param c Starting piece
   * @return piece underneath it, or null if none.
   */
  public GamePiece getPieceBeneath(GamePiece p) {
    int index = indexOf(p);
    while (index-- > 0) {
      if (!Boolean.TRUE.equals(contents[index].getProperty(Properties.INVISIBLE_TO_ME))) {
        return contents[index];
      }
    }
    return null;
  }

  /**
   * Finds the piece "above" the one provided
   * @param c Starting piece
   * @return piece above it, or null if none.
   */
  public GamePiece getPieceAbove(GamePiece p) {
    int index = indexOf(p);
    while (++index < getPieceCount()) {
      if (!Boolean.TRUE.equals(contents[index].getProperty(Properties.INVISIBLE_TO_ME))) {
        return contents[index];
      }
    }
    return null;
  }

  /**
   * <b>CAUTION:</b> returns the top VISIBLE piece in the stack, or null if none is visible.
   * @return the top <b>visible</b> piece in this stack
   */
  public GamePiece topPiece() {
    for (int i = pieceCount - 1; i >= 0; --i) {
      if (!Boolean.TRUE.equals(contents[i].getProperty(Properties.INVISIBLE_TO_ME))) {
        return contents[i];
      }
    }
    return null;
  }

  /**
   * @return the top piece in this stack that is visible to the player with the
   *         given id
   * @param playerId Player Id to check
   * @see GameModule#getUserId
   */
  public GamePiece topPiece(String playerId) {
    for (int i = pieceCount - 1; i >= 0; --i) {
      final String hiddenBy = (String) contents[i].getProperty(Properties.HIDDEN_BY);
      if (hiddenBy == null || hiddenBy.equals(playerId)) {
        return contents[i];
      }
    }
    return null;
  }

  /**
   * @return the bottom piece in this stack that is visible to the player with
   *         the given id
   * @param playerId Player Id to Check
   * @see GameModule#getUserId
   */
  public GamePiece bottomPiece(String playerId) {
    for (int i = 0; i < pieceCount; ++i) {
      final String hiddenBy = (String) contents[i].getProperty(Properties.HIDDEN_BY);
      if (hiddenBy == null || hiddenBy.equals(playerId)) {
        return contents[i];
      }
    }
    return null;
  }

  /** @return the bottom visible piece in this stack */
  public GamePiece bottomPiece() {
    for (int i = 0; i < pieceCount; ++i) {
      if (!Boolean.TRUE.equals(contents[i].getProperty(Properties.INVISIBLE_TO_ME))) {
        return contents[i];
      }
    }
    return null;
  }

  /**
   * @return Number of GamePieces that are visible to me
   */
  public int nVisible() {
    return (int) asList().stream()
                         .filter(PieceIterator.VISIBLE)
                         .count();
  }

  /**
   * Processes a key command for this stack, by sending it to the top visible piece in the stack.
   * @param stroke keystroke to process
   * @return Command encapsulating anything that happened as a result
   */
  @Override
  public Command keyEvent(javax.swing.KeyStroke stroke) {
    final GamePiece p = topPiece(); // NOTE: top VISIBLE piece
    if (p != null) {
      return p.keyEvent(stroke);
    }
    else {
      return null;
    }
  }

  /**
   * @return true if stack has been visually expanded by the player
   */
  public boolean isExpanded() {
    return expanded;
  }

  /**
   * Sets the expansion state of the stack. Players can expand (and un-expand) stacks by e.g. double-clicking on them.
   * Expanded stacks are generally shown with the pieces drawn further apart, the better to see the individual pieces.
   * When a stack is expanded, drag-and-drop operations can affect an individual piece rather than only the whole group.
   * @param b true if stack should be expanded, false if not
   */
  public void setExpanded(boolean b) {
    expanded = b && getPieceCount() > 1;
  }

  /**
   * Encodes the game state information of the stack into a string
   * @return Current encoded "game state" string for the stack
   */
  @Override
  public String getState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(getMap() == null ? "null" : getMap().getIdentifier()).append(getPosition().x).append(getPosition().y); //$NON-NLS-1$//
    for (int i = 0; i < pieceCount; ++i) {
      se.append(contents[i].getId());
    }
    se.append(HAS_LAYER_MARKER + layer);
    return se.getValue();
  }

  /**
   * Decodes the game state information of the stack from a string
   * @param s Game state information to be loaded into the stack
   */
  @Override
  public void setState(String s) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');

    final String mapId = st.nextToken();
    if ("null".equals(mapId)) { //NON-NLS //BR// Looooks like if we encode null in getState then there's no position in the file to be read
      setPosition(new Point(0, 0));
    }
    else {
      setPosition(new Point(st.nextInt(0), st.nextInt(0)));
    }
    pieceCount = 0;

    final GameState gs = GameModule.getGameModule().getGameState();
    while (st.hasMoreTokens()) {
      final String token = st.nextToken();
      final GamePiece child = gs.getPieceForId(token);
      if (child != null) {
        insertChild(child, pieceCount);
      }
      else {
        //BR// This encoding format with the "while" at the end made it challenging to work in a new parameter.
        if (token.startsWith(HAS_LAYER_MARKER)) {
          layer = Integer.valueOf(token.substring(HAS_LAYER_MARKER.length()));
        }
      }
    }

    Map m = null;
    if (!"null".equals(mapId)) { //$NON-NLS-1$//
      m = Map.getMapById(mapId);
      if (m == null) {
        ErrorDialog.dataWarning(new BadDataReport("Could not find map", mapId, null)); // NON-NLS
      }
    }

    if (m != getMap()) {
      if (m != null) {
        m.addPiece(this);
      }
      else {
        setMap(null);
      }
    }
  }

  /**
   * Compute the difference between <code>newState</code> and
   * <code>oldState</code> and apply that difference to the current state
   *
   * @param newState New State
   * @param oldState Old State
   */
  @Override
  public void mergeState(String newState, String oldState) {
    String mergedState = newState;
    if (!oldState.equals(getState())) {
      final SequenceEncoder.Decoder stNew = new SequenceEncoder.Decoder(newState, ';');
      final SequenceEncoder.Decoder stOld = new SequenceEncoder.Decoder(oldState, ';');
      final SequenceEncoder merge = new SequenceEncoder(';');
      merge.append(stNew.nextToken());
      stOld.nextToken();
      merge.append(stNew.nextToken());
      stOld.nextToken();
      merge.append(stNew.nextToken());
      stOld.nextToken();
      final ArrayList<String> newContents = new ArrayList<>();
      while (stNew.hasMoreTokens()) {
        newContents.add(stNew.nextToken());
      }
      final ArrayList<String> oldContents = new ArrayList<>();
      while (stOld.hasMoreTokens()) {
        oldContents.add(stOld.nextToken());
      }
      final int j = getPieceCount();
      for (int i = 0; i < j; ++i) {
        final String id = getPieceAt(i).getId();
        if (!newContents.contains(id) && !oldContents.contains(id)) {
          final int index = i == 0 ? -1 :
            newContents.indexOf(getPieceAt(i - 1).getId());
          newContents.add(index + 1, id);
        }
      }
      for (final String s : newContents) {
        merge.append(s);
      }
      mergedState = merge.getValue();
    }
    setState(mergedState);
  }

  /**
   * @return the encoding type
   */
  @Override
  public String getType() {
    return TYPE;
  }

  /**
   * Stacks themselves ignore property sets -- use {@link setPropertyOnContents} to apply
   * a property to the members of a stack.
   * @param key String key of property to be changed
   * @param val Object containing new value of the property
   */
  @Override
  public void setProperty(Object key, Object val) {
  }

  /**
   * @return string list of stack contents, for debugging-type purposes
   */
  @Override
  public String toString() {
    return super.toString() + "[" + getName() + "]";
  }

  /**
   * Calls setProperty() on each piece in this stack
   *
   * @param key Property Key
   * @param val Property Value
   */
  public void setPropertyOnContents(Object key, Object val) {
    asList().forEach(gamePiece -> gamePiece.setProperty(key, val));
  }

  /**
   * Stacks themselves do not have any properties, so always return null.
   * @param key String key of property to be returned
   * @return always null
   */
  @Override
  public Object getProperty(Object key) {
    return null;
  }

  /**
   * Stacks themselves do not have any properties, so always return null.
   * @param key String key of property to be returned
   * @return always null
   */
  @Override
  public Object getLocalizedProperty(Object key) {
    return getProperty(key);
  }

  /**
   * @param map Each stack belongs to a single {@link Map}
   */
  @Override
  public void setMap(Map map) {
    this.map = map;
  }

  /**
   * @return the map for this Stack
   */
  @Override
  public Map getMap() {
    return map;
  }

  /**
   * @return current X/Y position of stack
   */
  @Override
  public Point getPosition() {
    return new Point(pos);
  }

  /**
   * @param p Sets the location of this stack on its {@link Map}
   */
  @Override
  public void setPosition(Point p) {
    pos = p;
  }

  /**
   * Stacks cannot contain other stacks/decks, nor be contained in them, so parent is always null.
   * @return always null
   */
  @Override
  public Stack getParent() {
    return null;
  }

  /**
   * Required for interface but won't be needed for stacks
   * @param s sets the {@link Stack} to which this piece belongs.
   */
  @Override
  public void setParent(Stack s) {
    if (s != null) {
      ErrorDialog.dataWarning(new BadDataReport("Cannot add stack to another stack", toString(), null)); // NON-NLS
    }
  }

  /**
   * @return stack's unique ID
   */
  @Override
  public String getId() {
    return id;
  }

  /**
   * @param id sets unique ID for this piece
   */
  @Override
  public void setId(String id) {
    this.id = id;
  }

  /**
   * {@link StackMetrics} encapsulate information on how to draw expanded/unexpanded views of stacks.
   * This method sets the default metrics for the module, but each map can have its own configuration, which
   * can be found in the [Stacking options] subcomponent of the Map in the Editor.
   * @param s default stack metrics for the module
   */
  public static void setDefaultMetrics(StackMetrics s) {
    defaultMetrics = s;
  }

  /**
   * {@link StackMetrics} encapsulate information on how to draw expanded/unexpanded views of stacks.
   * This method retrieves the appropriate stack metrics to use for a given map
   * @param m a map
   * @return stack metrics for the map, if provided, or the default one for the module.
   */
  public StackMetrics getStackMetrics(Map m) {
    return m == null ? getDefaultMetrics() : m.getStackMetrics();
  }

  /**
   * {@link StackMetrics} encapsulate information on how to draw expanded/unexpanded views of stacks.
   * This method retrieves the appropriate stack metrics to use the stack, based on its map
   * @return stack metrics for the map, if provided, or the default one for the module.
   */
  public StackMetrics getStackMetrics() {
    return getStackMetrics(getMap());
  }

  /**
   * {@link StackMetrics} encapsulate information on how to draw expanded/unexpanded views of stacks.
   * This method retrieves the default stack metrics for the module.
   * @return default stack metrics for the module
   */
  public StackMetrics getDefaultMetrics() {
    if (defaultMetrics == null) {
      setDefaultMetrics(new StackMetrics());
    }
    return defaultMetrics;
  }

  /**
   * See {@link AbstractImageFinder}
   * Tells each of the pieces in the stack to add its images to the collection
   * @param s Collection to add image names to
   */
  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    for (final Iterator<GamePiece> i = getPiecesIterator(); i.hasNext(); ) {
      final GamePiece p = i.next();
      if (p instanceof ImageSearchTarget) {
        ((ImageSearchTarget)p).addImageNamesRecursively(s);
      }
    }
  }

  private class VisibleOrderIterator implements Iterator<GamePiece> {
    private GamePiece next;
    private int index = pieceCount - 1;
    private boolean doingSelected = true;

    public VisibleOrderIterator() {
      next = findNext();
    }

    @Override
    public boolean hasNext() {
      return next != null;
    }

    @Override
    public GamePiece next() {
      final GamePiece ret = next;
      next = findNext();
      return ret;
    }

    private GamePiece findNext() {
      GamePiece ret = null;
      while (index >= 0) {
        final GamePiece p = getPieceAt(index--);
        if (doingSelected ^ !Boolean.TRUE.equals(
                              p.getProperty(Properties.SELECTED))) {
          ret = p;
          break;
        }
      }

      if (ret == null && doingSelected) {
        doingSelected = false;
        index = pieceCount - 1;
        ret = findNext();
      }
      return ret;
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  private class AllPieceIterator implements Iterator<GamePiece> {
    private int index = 0;
    private final GamePiece[] p;

    public AllPieceIterator() {
      p = Arrays.copyOf(contents, pieceCount);
    }

    @Override
    public boolean hasNext() {
      return index < p.length;
    }

    @Override
    public GamePiece next() {
      return p[index++];
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  private class ReversePieceIterator implements Iterator<GamePiece> {
    private int index = pieceCount - 1;
    private final GamePiece[] p;

    public ReversePieceIterator() {
      p = Arrays.copyOf(contents, pieceCount);
    }

    @Override
    public boolean hasNext() {
      return index >= 0;
    }

    @Override
    public GamePiece next() {
      return p[index--];
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }
}
