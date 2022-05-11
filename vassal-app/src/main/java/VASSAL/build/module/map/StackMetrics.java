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
package VASSAL.build.module.map;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.KeyEvent;
import java.awt.geom.AffineTransform;
import java.util.List;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Highlighter;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

/**
 * StackMetrics provides the [Stacking options] component of a {@link Map}. It encapsulates information on how to draw
 * expanded and unexpanded views of a stack.
 */
public class StackMetrics extends AbstractConfigurable {
  protected int exSepX, exSepY;
  protected int unexSepX, unexSepY;
  protected boolean disabled;

  protected KeyStroke topKey = KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0);
  protected KeyStroke bottomKey = KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0);
  protected KeyStroke upKey = KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0);
  protected KeyStroke downKey = KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0);

  protected PieceFilter unselectedVisible;
  protected PieceFilter selectedVisible;

  protected Color blankColor;

  public static final String EXSEP_X = "exSepX"; //NON-NLS
  public static final String EXSEP_Y = "exSepY"; //NON-NLS
  public static final String UNEXSEP_X = "unexSepX"; //NON-NLS
  public static final String UNEXSEP_Y = "unexSepY"; //NON-NLS
  public static final String DISABLED = "disabled"; //NON-NLS
  public static final String TOP_KEY = "top"; //NON-NLS
  public static final String BOTTOM_KEY = "bottom"; //NON-NLS
  public static final String UP_KEY = "up"; //NON-NLS //NON-NLS
  public static final String DOWN_KEY = "down"; //NON-NLS
  public static final String COLOR = "color"; //NON-NLS

  public static final int DEFAULT_EXSEP_X = 6;
  public static final int DEFAULT_EXSEP_Y = 18;
  public static final int DEFAULT_UNEXSEP_X = 2;
  public static final int DEFAULT_UNEXSEP_Y = 4;

  protected Map map;

  @Override
  public void setAttribute(String name, Object value) {
    if (EXSEP_X.equals(name)) {
      if (value instanceof String) {
        try {
          exSepX = Integer.parseInt((String) value);
        }
        catch (final NumberFormatException NaN) {
          exSepX = DEFAULT_EXSEP_X;
          ErrorDialog.dataWarning(
              new BadDataReport(
                  Resources.getString("Error.bad_preference", EXSEP_X, "StackMetrics"), (String) value, NaN)); //NON-NLS
        }
      }
      else if (value != null) {
        exSepX = (Integer) value;
      }
    }
    else if (EXSEP_Y.equals(name)) {
      if (value instanceof String) {
        try {
          exSepY = Integer.parseInt((String) value);
        }
        catch (final NumberFormatException NaN) {
          exSepY = DEFAULT_EXSEP_Y;
          ErrorDialog.dataWarning(
              new BadDataReport(
                  Resources.getString("Error.bad_preference", EXSEP_Y, "StackMetrics"), (String) value, NaN)); //NON-NLS
        }
      }
      else if (value != null) {
        exSepY = (Integer) value;
      }
    }
    else if (UNEXSEP_X.equals(name)) {
      if (value instanceof String) {
        try {
          unexSepX = Integer.parseInt((String) value);
        }
        catch (final NumberFormatException NaN) {
          unexSepX = DEFAULT_UNEXSEP_X;
          ErrorDialog.dataWarning(
              new BadDataReport(
                  Resources.getString("Error.bad_preference", UNEXSEP_X, "StackMetrics"), (String) value, NaN)); //NON-NLS
        }
      }
      else if (value != null) {
        unexSepX = (Integer) value;
      }
    }
    else if (UNEXSEP_Y.equals(name)) {
      if (value instanceof String) {
        try {
          unexSepY = Integer.parseInt((String) value);
        }
        catch (final NumberFormatException NaN) {
          unexSepY = DEFAULT_UNEXSEP_Y;
          ErrorDialog.dataWarning(
              new BadDataReport(
                  Resources.getString("Error.bad_preference", UNEXSEP_Y, "StackMetrics"), (String) value, NaN)); //NON-NLS
        }
      }
      else if (value != null) {
        unexSepY = (Integer) value;
      }
    }
    else if (DISABLED.equals(name)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      disabled = (Boolean) value;
    }
    else if (TOP_KEY.equals(name)) {
      topKey = HotKeyConfigurer.decode((String) value);
    }
    else if (BOTTOM_KEY.equals(name)) {
      bottomKey = HotKeyConfigurer.decode((String) value);
    }
    else if (UP_KEY.equals(name)) {
      upKey = HotKeyConfigurer.decode((String) value);
    }
    else if (DOWN_KEY.equals(name)) {
      downKey = HotKeyConfigurer.decode((String) value);
    }
    else if (COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      blankColor = (Color) value;
    }
  }

  @Override
  public String getAttributeValueString(String name) {
    if (EXSEP_X.equals(name)) {
      return String.valueOf(exSepX);
    }
    else if (EXSEP_Y.equals(name)) {
      return String.valueOf(exSepY);
    }
    else if (UNEXSEP_X.equals(name)) {
      return String.valueOf(unexSepX);
    }
    else if (UNEXSEP_Y.equals(name)) {
      return String.valueOf(unexSepY);
    }
    else if (DISABLED.equals(name)) {
      return String.valueOf(disabled);
    }
    else if (TOP_KEY.equals(name)) {
      return HotKeyConfigurer.encode(topKey);
    }
    else if (BOTTOM_KEY.equals(name)) {
      return HotKeyConfigurer.encode(bottomKey);
    }
    else if (UP_KEY.equals(name)) {
      return HotKeyConfigurer.encode(upKey);
    }
    else if (DOWN_KEY.equals(name)) {
      return HotKeyConfigurer.encode(downKey);
    }
    else if (COLOR.equals(name)) {
      return blankColor == null ? null
          : ColorConfigurer.colorToString(blankColor);
    }
    return null;
  }

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    map.setStackMetrics(this);
  }

  public StackMetrics() {
    this(false, DEFAULT_EXSEP_X, DEFAULT_EXSEP_Y, DEFAULT_UNEXSEP_X, DEFAULT_UNEXSEP_Y);
  }

  public StackMetrics(boolean dis,
                      int exSx, int exSy,
                      int unexSx, int unexSy) {
    disabled = dis;
    exSepX = exSx;
    exSepY = exSy;
    unexSepX = unexSx;
    unexSepY = unexSy;

    unselectedVisible = piece -> !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
        && !Boolean.TRUE.equals(piece.getProperty(Properties.SELECTED));
    selectedVisible = piece -> !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
        && Boolean.TRUE.equals(piece.getProperty(Properties.SELECTED));
  }

  /**
   * Different instances of StackMetrics may render a {@link Stack}
   * in different ways.  The default algorithm is: If not expanded,
   * all but the top visible GamePiece is drawn as a white square
   * with size given by {@link GamePiece#getShape}.  The
   * separation between GamePieces is given by {@link
   * #relativePosition}
   *
   * If expanded, all GamePieces are drawn with separation given by
   * {@link #relativePosition}.  GamePiece that are selected are
   * drawn in front of other GamePieces, even those above them in
   * the stack.
   */
  public void draw(Stack stack, Graphics g, int x, int y, Component obs, double zoom) {
    final Highlighter highlighter = stack.getMap() == null ? BasicPiece.getHighlighter() : stack.getMap().getHighlighter();
    final Point[] positions = new Point[stack.getPieceCount()];
    getContents(stack, positions, null, null, x, y);

    for (final PieceIterator e = new PieceIterator(stack.getPiecesIterator(),
                                             unselectedVisible);
         e.hasMoreElements();) {

      final GamePiece next = e.nextPiece();
      final int index = stack.indexOf(next);
      if (index >= 0) { //BR// Bounds-check index as a bandaid against getting drawn during e.g. a screenshot or loadgame
        final int nextX = x + (int) (zoom * (positions[index].x - x));
        final int nextY = y + (int) (zoom * (positions[index].y - y));
        if (stack.isExpanded() || !e.hasMoreElements()) {
          next.draw(g, nextX, nextY, obs, zoom);
        }
        else {
          drawUnexpanded(next, g, nextX, nextY, obs, zoom);
        }
      }
    }

    stack.asList().stream()
         .filter(gamePiece -> selectedVisible.accept(gamePiece))
         .forEach(gamePiece -> {
           final int index = stack.indexOf(gamePiece);
           if (index >= 0) { //BR// Bounds-check index as a bandaid against getting drawn during e.g. a screenshot or loadgame
             final int nextX = x + (int) (zoom * (positions[index].x - x));
             final int nextY = y + (int) (zoom * (positions[index].y - y));
             gamePiece.draw(g, nextX, nextY, obs, zoom);
             highlighter.draw(gamePiece, g, nextX, nextY, obs, zoom);
           }
         });
  }

  /**
   * Draw only those pieces in the target stack whose boundingBoxes fall within the given visibleRect
   * This method is considerably faster than the other draw method.
   * @param stack
   * @param g
   * @param location the location of the stack in component coordinates
   * @param zoom
   * @param visibleRect the visible rectangle in component coordinates
   */
  public void draw(Stack stack, Point location, Graphics g, Map map, double zoom, Rectangle visibleRect) {
    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    final Component view = map.getView();
    final Highlighter highlighter = map.getHighlighter();
    final Point mapLocation = map.drawingToMap(location, os_scale);
    final Rectangle region = visibleRect == null ? null : map.drawingToMap(visibleRect, os_scale);
    final Point[] positions = new Point[stack.getPieceCount()];
    final Rectangle[] bounds = region == null ? null : new Rectangle[stack.getPieceCount()];
    getContents(stack, positions, null, bounds, mapLocation.x, mapLocation.y);

    for (final PieceIterator e = new PieceIterator(stack.getPiecesIterator(),
                                             unselectedVisible);
         e.hasMoreElements();) {

      final GamePiece next = e.nextPiece();
      final int index = stack.indexOf(next);
      if (index >= 0) { //BR// Bounds-check index as a bandaid against getting drawn during e.g. a screenshot or loadgame
        final Point pt = map.mapToDrawing(positions[index], os_scale);
        if (bounds == null || isVisible(region, bounds[index])) {
          if (stack.isExpanded() || !e.hasMoreElements()) {
            next.draw(g, pt.x, pt.y, view, zoom);
          }
          else {
            drawUnexpanded(next, g, pt.x, pt.y, view, zoom);
          }
        }
      }
    }

    stack.asList().stream()
         .filter(gamePiece -> selectedVisible.accept(gamePiece))
         .forEach(gamePiece -> {
           final int index = stack.indexOf(gamePiece);
           if ((index >= 0) && (bounds == null || isVisible(region, bounds[index]))) {  //BR// Bounds-check index as a bandaid against getting drawn during e.g. a screenshot or loadgame
             final Point pt = map.mapToDrawing(positions[index], os_scale);
             gamePiece.draw(g, pt.x, pt.y, view, zoom);
             highlighter.draw(gamePiece, g, pt.x, pt.y, view, zoom);
           }
         });
  }

  private boolean isVisible(Rectangle region, Rectangle bounds) {
    boolean visible = true;
    if (region != null) {
      visible = region.intersects(bounds);
    }
    return visible;
  }

  /**
   * Draw a {@link GamePiece} that is not the top piece in an unexpanded {@link Stack}
   *
   * Default implementation is a white square with a black border
   */
  protected void drawUnexpanded(GamePiece p, Graphics g,
                                int x, int y, Component obs, double zoom) {
    if (blankColor == null) {
      p.draw(g, x, y, obs, zoom);
    }
    else {
      final Graphics2D g2d = (Graphics2D) g;
      g.setColor(blankColor);
      Shape s = p.getShape();
      final AffineTransform t = AffineTransform.getScaleInstance(zoom, zoom);
      t.translate(x / zoom, y / zoom);
      s = t.createTransformedShape(s);
      g2d.fill(s);
      g.setColor(Color.black);
      g2d.draw(s);
    }
  }

  /**
   * The color used to draw boxes representing counters beneath the top one in a stack.
   * A value of null indicates that the counters should be drawn fully
   * @return
   */
  public Color getBlankColor() {
    return blankColor;
  }

  /**
   * Fill the argument arrays with the positions, selection bounds and bounding boxes of the pieces in the argument stack
   * @param parent The parent Stack
   * @param positions If non-null will contain a {@link Point} giving the position of each piece in <code>parent</code>
   * @param shapes If non-null will contain a {@link Shape} giving the shape of for each piece in <code>parent</code>
   * @param boundingBoxes If non-null will contain a {@link Rectangle} giving the bounding box for each piece in <code>parent</code>
   * @param x the x-location of the parent
   * @param y the y-location of the parent
   * @return the number of pieces processed in the stack
   */
  public int getContents(Stack parent, Point[] positions, Shape[] shapes, Rectangle[] boundingBoxes, int x, int y) {
    int count = parent.getMaximumVisiblePieceCount();
    if (positions != null) {
      count = Math.min(count, positions.length);
    }
    if (boundingBoxes != null) {
      count = Math.min(count, boundingBoxes.length);
    }
    if (shapes != null) {
      count = Math.min(count, shapes.length);
    }
    final int dx = parent.isExpanded() ? exSepX : unexSepX;
    final int dy = parent.isExpanded() ? exSepY : unexSepY;
    Point currentPos = null, nextPos;
    Rectangle currentSelBounds = null, nextSelBounds;
    for (int index = 0; index < count; ++index) {
      final GamePiece child = parent.getPieceAt(index);
      if (Boolean.TRUE.equals(child.getProperty(Properties.INVISIBLE_TO_ME))) {
        final Rectangle blank = new Rectangle(x, y, 0, 0);
        if (positions != null) {
          positions[index] = blank.getLocation();
        }
        if (boundingBoxes != null) {
          boundingBoxes[index] = blank;
        }
        if (shapes != null) {
          shapes[index] = blank;
        }
      }
      else {
        child.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.TRUE);
        nextSelBounds = child.getShape().getBounds();
        child.setProperty(Properties.USE_UNROTATED_SHAPE, Boolean.FALSE);
        nextPos = new Point(0, 0);
        if (currentPos == null) {
          currentSelBounds = nextSelBounds;
          currentSelBounds.translate(x, y);
          currentPos = new Point(x, y);
          nextPos = currentPos;
        }
        else {
          nextPosition(currentPos, currentSelBounds, nextPos, nextSelBounds, dx, dy);
        }
        if (positions != null) {
          positions[index] = nextPos;
        }
        if (boundingBoxes != null) {
          final Rectangle bbox = child.boundingBox();
          bbox.translate(nextPos.x, nextPos.y);
          boundingBoxes[index] = bbox;
        }
        if (shapes != null) {
          Shape s = child.getShape();
          s = AffineTransform.getTranslateInstance(nextPos.x, nextPos.y).createTransformedShape(s);
          shapes[index] = s;
        }
        currentPos = nextPos;
        currentSelBounds = nextSelBounds;
      }
    }
    return count;
  }

  protected void nextPosition(Point currentPos, Rectangle currentBounds, Point nextPos, Rectangle nextBounds, int dx, int dy) {
    final int deltaX;
    final int deltaY;
    if (dx > 0) {
      deltaX = currentBounds.x + dx - nextBounds.x;
    }
    else if (dx < 0) {
      deltaX = currentBounds.x + currentBounds.width - nextBounds.width + dx - nextBounds.x;
    }
    else {
      deltaX = currentPos.x - nextPos.x;
    }
    if (dy > 0) {
      deltaY = currentBounds.y + currentBounds.height - nextBounds.height - nextBounds.y - dy;
    }
    else if (dy < 0) {
      deltaY = currentBounds.y - dy - nextBounds.y;
    }
    else {
      deltaY = currentPos.y - nextPos.y;
    }
    nextBounds.translate(deltaX, deltaY);
    nextPos.translate(deltaX, deltaY);
  }

  public Point relativePosition(Stack parent, GamePiece c) {
    final int index =
      Math.min(parent.indexOf(c), parent.getMaximumVisiblePieceCount() - 1);

    if (index < 0) {
      return new Point(0, 0);
    }

    final Point[] pos = new Point[parent.getMaximumVisiblePieceCount()];
    getContents(parent, pos, null, null, 0, 0);
    return pos[index];
  }

  public boolean isStackingEnabled() {
    return !disabled;
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public String getConfigureName() {
    return null;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Stacking.component_type"); //$NON-NLS-1$
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "StackingOptions"); //NON-NLS
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      DISABLED,
      EXSEP_X,
      EXSEP_Y,
      UNEXSEP_X,
      UNEXSEP_Y,
      COLOR,
      TOP_KEY,
      BOTTOM_KEY,
      UP_KEY,
      DOWN_KEY
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString("Editor.Stacking.disable"), //$NON-NLS-1$
        Resources.getString("Editor.Stacking.h_expand"), //$NON-NLS-1$
        Resources.getString("Editor.Stacking.v_expand"), //$NON-NLS-1$
        Resources.getString("Editor.Stacking.hnon_expand"), //$NON-NLS-1$
        Resources.getString("Editor.Stacking.vnon_expand"), //$NON-NLS-1$
        Resources.getString("Editor.Stacking.color_nonexpand"), //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Boolean.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Color.class
    };
  }

  private final VisibilityCondition cond = () -> !disabled;

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(EXSEP_X, EXSEP_Y, UNEXSEP_X, UNEXSEP_Y, COLOR).contains(name)) {
      return cond;
    }
    else {
      return null;
    }
  }

  public Stack createStack(GamePiece p) {
    return createStack(p, false);
  }

  public Stack createStack(GamePiece p, boolean force) {
    return isStackingEnabled() || force ? new Stack(p) : null;
  }

  public KeyStroke getMoveUpKey() {
    return upKey;
  }

  public KeyStroke getMoveDownKey() {
    return downKey;
  }

  public KeyStroke getMoveTopKey() {
    return topKey;
  }

  public KeyStroke getMoveBottomKey() {
    return bottomKey;
  }

  /**
   * Merge the two pieces if stacking is enabled.
   * If stacking is disabled, place the moving piece at the same location as the fixed piece
   * @param fixed
   * @param moving
   * @return a Command that accomplishes this task
   * @see #merge
   */
  public Command placeOrMerge(GamePiece fixed, GamePiece moving) {
    if (disabled) {
      return fixed.getMap().placeAt(moving, fixed.getPosition());
    }
    else {
      return merge(fixed, moving);
    }
  }

  /**
   * Place a GamePiece on top of another GamePiece
   * Create/remove stacks as necessary, even if stacking is disabled for this instance
   * @param moving the GamePiece that will be merged into the stack
   * @param fixed the GamePiece defining the location and contents of the existing stack
   * @return a Command that accomplishes this task
   */
  public Command merge(GamePiece fixed, GamePiece moving) {
    Command comm;

    // Pause logging to capture any Deck Empty Hotkeys that may get fired by merging this piece
    final GameModule gm = GameModule.getGameModule();
    final boolean loggingPausedByMe = gm.pauseLogging();

    if (fixed instanceof Stack && ((Stack) fixed).topPiece() != null) {  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
      comm = merge(((Stack) fixed).topPiece(), moving);
    }
    else {
      final MoveTracker tracker = new MoveTracker(moving);
      comm = new NullCommand();
      Stack fixedParent = fixed.getParent();
      int index = fixedParent == null ? 0 : fixedParent.indexOf(fixed) + 1;

      if (moving != fixed && moving != fixedParent) {
        final GameState gs = GameModule.getGameModule().getGameState();

        final boolean isNewPiece = gs.getPieceForId(moving.getId()) == null;
        if (fixedParent == null) {
          if (fixed instanceof Stack) {
            fixedParent = (Stack) fixed;
            index = fixedParent.getPieceCount();
          }
          else {
            fixedParent = createStack(fixed, true);
            comm = comm.append(fixed.getMap().placeAt(
              fixedParent, fixedParent.getPosition()));
            index = 1;
          }
        }

        if (isNewPiece) {
          gs.addPiece(moving);
          comm = comm.append(new AddPiece(moving));
        }

        if (moving instanceof Stack) {
          for (final GamePiece p : ((Stack) moving).asList()) {
            final MoveTracker t = new MoveTracker(p);
            fixedParent.insertChild(p, index++);
            comm = comm.append(t.getMoveCommand());
          }
        }
        else {
          if (moving.getParent() == fixedParent && fixedParent != null) {
            index--;
          }
          fixedParent.insert(moving, index);
          comm = comm.append(tracker.getMoveCommand());
        }
      }
    }

    // Restart logging and append any Commands generated by Deck Empty Hotkeys
    if (loggingPausedByMe) {
      comm = comm.append(gm.resumeLogging());
    }

    return comm;
  }
}
