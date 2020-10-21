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

import VASSAL.search.AbstractImageFinder;
import VASSAL.search.ImageSearchTarget;
import VASSAL.tools.ProblemDialog;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.StackMetrics;
import VASSAL.command.Command;
import VASSAL.tools.EnumeratedIterator;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.SequenceEncoder;

/**
 * A collection of GamePieces which can be moved as a single unit
 */
public class Stack extends AbstractImageFinder implements GamePiece, StateMergeable {
  public static final String TYPE = "stack"; //$NON-NLS-1$//
  protected static final int INCR = 5;
  protected GamePiece[] contents = new GamePiece[INCR];
  protected int pieceCount = 0;

  protected Point pos = new Point(0, 0);

  private String id;
  private boolean expanded = false;

  protected Map map;
  private static StackMetrics defaultMetrics;

  public Stack() {
    this(null);
  }

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
    //    return new AllPieceEnum();
  }

  /**
   * @return an unmodifiable {@link List} which is a defensive copy of {@link GamePiece}s contained in
   * this {@link Stack}
   */
  public List<GamePiece> asList() {
    List<GamePiece> result = new ArrayList<>();
    for (int i = 0; i < pieceCount; i++) {
      result.add(contents[i]);
    }
    return Collections.unmodifiableList(result);
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
//    return new ReversePieceEnum();
  }

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
//    return new VisibleOrderEnum();
  }

  public void remove(GamePiece p) {
    removePieceAt(indexOf(p));
    p.setParent(null);
    if (getMap() != null) {
      getMap().repaint();
    }
  }

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

  protected void insertPieceAt(GamePiece p, int index) {
    if (index < 0) {
      index = 0;
    }
    else if (index > pieceCount) {
      index = pieceCount;
    }

    if (pieceCount >= contents.length) {
      GamePiece[] newContents = new GamePiece[contents.length + INCR];
      System.arraycopy(contents, 0, newContents, 0, pieceCount);
      contents = newContents;
    }

    for (int i = pieceCount; i > index; --i) {
      contents[i] = contents[i - 1];
    }

    contents[index] = p;
    pieceCount++;
  }

  public void removeAll() {
    pieceCount = 0;
    expanded = false;
  }

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
    int index = indexOf(p);
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
   * Return a comma-separated list of the names of the pieces in this Stack
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

  @Override
  public String getName() {
    return getName(false);
  }

  @Override
  public String getLocalizedName() {
    return getName(true);
  }

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

  @Override
  public Shape getShape() {
    Area a = new Area();
    Shape[] childBounds = new Shape[getPieceCount()];
    StackMetrics metrics = getMap() == null ? getDefaultMetrics() : getMap().getStackMetrics();
    metrics.getContents(this, null, childBounds, null, 0, 0);
    asList().stream()
            .filter(PieceIterator.VISIBLE)
            .forEach(p -> a.add(new Area(childBounds[indexOf(p)])));

    return a;
  }

  public void selectNext(GamePiece c) {
    KeyBuffer.getBuffer().remove(c);
    if (pieceCount > 1 && indexOf(c) >= 0) {
      int newSelectedIndex = indexOf(c) == pieceCount - 1 ? pieceCount - 2 : indexOf(c) + 1;
      for (int i = 0; i < pieceCount; ++i) {
        if (indexOf(contents[i]) == newSelectedIndex) {
          KeyBuffer.getBuffer().add(contents[i]);
          return;
        }
      }
    }
  }

  public GamePiece getPieceBeneath(GamePiece p) {
    int index = indexOf(p);
    while (index-- > 0) {
      if (!Boolean.TRUE.equals(contents[index].getProperty(Properties.INVISIBLE_TO_ME))) {
        return contents[index];
      }
    }
    return null;
  }

  public GamePiece getPieceAbove(GamePiece p) {
    int index = indexOf(p);
    while (++index < getPieceCount()) {
      if (!Boolean.TRUE.equals(contents[index].getProperty(Properties.INVISIBLE_TO_ME))) {
        return contents[index];
      }
    }
    return null;
  }

  /** @return the top visible piece in this stack */
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
      String hiddenBy = (String) contents[i].getProperty(Properties.HIDDEN_BY);
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
      String hiddenBy = (String) contents[i].getProperty(Properties.HIDDEN_BY);
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
  protected int nVisible() {
    return (int) asList().stream()
                         .filter(PieceIterator.VISIBLE)
                         .count();
  }

  @Override
  public Command keyEvent(javax.swing.KeyStroke stroke) {
    GamePiece p = topPiece();
    if (p != null) {
      return p.keyEvent(stroke);
    }
    else {
      return null;
    }
  }

  public boolean isExpanded() {
    return expanded;
  }

  public void setExpanded(boolean b) {
    expanded = b && getPieceCount() > 1;
  }

  @Override
  public String getState() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getMap() == null ? "null" : getMap().getIdentifier()).append(getPosition().x).append(getPosition().y); //$NON-NLS-1$//
    for (int i = 0; i < pieceCount; ++i) {
      se.append(contents[i].getId());
    }
    return se.getValue();
  }

  @Override
  public void setState(String s) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    final String mapId = st.nextToken();
    setPosition(new Point(st.nextInt(0), st.nextInt(0)));
    pieceCount = 0;

    final GameState gs = GameModule.getGameModule().getGameState();
    while (st.hasMoreTokens()) {
      final GamePiece child = gs.getPieceForId(st.nextToken());
      if (child != null) insertChild(child, pieceCount);
    }

    Map m = null;
    if (!"null".equals(mapId)) { //$NON-NLS-1$//
      m = Map.getMapById(mapId);
      if (m == null) {
        ErrorDialog.dataWarning(new BadDataReport("Could not find map", mapId, null));
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
   * <code>oldState</code> and appy that difference to the current state
   *
   * @param newState New State
   * @param oldState Old State
   */
  @Override
  public void mergeState(String newState, String oldState) {
    String mergedState = newState;
    if (!oldState.equals(getState())) {
      SequenceEncoder.Decoder stNew = new SequenceEncoder.Decoder(newState, ';');
      SequenceEncoder.Decoder stOld = new SequenceEncoder.Decoder(oldState, ';');
      SequenceEncoder merge = new SequenceEncoder(';');
      merge.append(stNew.nextToken());
      stOld.nextToken();
      merge.append(stNew.nextToken());
      stOld.nextToken();
      merge.append(stNew.nextToken());
      stOld.nextToken();
      ArrayList<String> newContents = new ArrayList<>();
      while (stNew.hasMoreTokens()) {
        newContents.add(stNew.nextToken());
      }
      ArrayList<String> oldContents = new ArrayList<>();
      while (stOld.hasMoreTokens()) {
        oldContents.add(stOld.nextToken());
      }
      for (int i = 0, j = getPieceCount(); i < j; ++i) {
        String id = getPieceAt(i).getId();
        if (!newContents.contains(id) && !oldContents.contains(id)) {
          int index = i == 0 ? -1 :
            newContents.indexOf(getPieceAt(i - 1).getId());
          newContents.add(index + 1, id);
        }
      }
      for (String s : newContents) {
        merge.append(s);
      }
      mergedState = merge.getValue();
    }
    setState(mergedState);
  }

  @Override
  public String getType() {
    return TYPE;
  }

  @Override
  public void setProperty(Object key, Object val) {
  }

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

  @Override
  public Object getProperty(Object key) {
    return null;
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    return getProperty(key);
  }

  @Override
  public void setMap(Map map) {
    this.map = map;
  }

  @Override
  public Map getMap() {
    return map;
  }

  @Override
  public Point getPosition() {
    return new Point(pos);
  }

  @Override
  public void setPosition(Point p) {
    pos = p;
  }

  @Override
  public Stack getParent() {
    return null;
  }

  @Override
  public void setParent(Stack s) {
    if (s != null) {
      ErrorDialog.dataWarning(new BadDataReport("Cannot add stack to another stack", toString(), null));
    }
  }

  @Override
  public String getId() {
    return id;
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  public static void setDefaultMetrics(StackMetrics s) {
    defaultMetrics = s;
  }

  public StackMetrics getStackMetrics(Map m) {
    return m == null ? getDefaultMetrics() : m.getStackMetrics();
  }

  public StackMetrics getStackMetrics() {
    return getStackMetrics(getMap());
  }

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
    for (Iterator<GamePiece> i = getPiecesIterator(); i.hasNext(); ) {
      GamePiece p = i.next();
      if (p instanceof ImageSearchTarget)
      ((ImageSearchTarget)p).addImageNamesRecursively(s);
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
