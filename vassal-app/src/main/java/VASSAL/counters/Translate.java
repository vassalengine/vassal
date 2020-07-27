/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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
import java.awt.Window;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MovementReporter;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * Give a piece a command that moves it a fixed amount in a particular
 * direction, optionally tracking the current rotation of the piece.
 */
public class Translate extends Decorator implements TranslatablePiece {
  private static final String _0 = "0";
  public static final String ID = "translate;";
  protected KeyCommand[] commands;
  protected String commandName;
  protected NamedKeyStroke keyCommand;
  protected FormattedString xDist = new FormattedString("");
  protected FormattedString xIndex = new FormattedString("");
  protected FormattedString xOffset = new FormattedString("");
  protected FormattedString yDist = new FormattedString("");
  protected FormattedString yIndex = new FormattedString("");
  protected FormattedString yOffset = new FormattedString("");
  protected String description;
  protected boolean moveStack;
  protected KeyCommand moveCommand;
  protected static MoveExecuter mover;

  public Translate() {
    this(ID + "Move Forward", null);
  }

  public Translate(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public String getDescription() {
    String d = "Move fixed distance";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken("Move Forward");
    keyCommand = st.nextNamedKeyStroke('M');
    xDist.setFormat(st.nextToken(_0));
    yDist.setFormat(st.nextToken("60"));
    moveStack = st.nextBoolean(false); // Default move whole stack option to false
    xIndex.setFormat(st.nextToken(_0));
    yIndex.setFormat(st.nextToken(_0));
    xOffset.setFormat(st.nextToken(_0));
    yOffset.setFormat(st.nextToken(_0));
    description = st.nextToken("");
    commands = null;
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      moveCommand = new KeyCommand(commandName, keyCommand, Decorator.getOutermost(this), this);
      if (commandName.length() > 0 && keyCommand != null && !keyCommand.isNull()) {
        commands = new KeyCommand[]{moveCommand};
      }
      else {
        commands = new KeyCommand[0];
      }
    }
    moveCommand.setEnabled(getMap() != null);
    return commands;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName)
      .append(keyCommand)
      .append(xDist.getFormat())
      .append(yDist.getFormat())
      .append(moveStack)
      .append(xIndex.getFormat())
      .append(yIndex.getFormat())
      .append(xOffset.getFormat())
      .append(yOffset.getFormat())
      .append(description);
    return ID + se.getValue();
  }

  @Override
  public Command keyEvent(KeyStroke stroke) {
    // Classic MFD delays the execution of the inner piece's key event until after this piece has moved
    // This unexpectedly changes the order of trait execution, but is required for the old Move Batcher to work correctly
    if (GlobalOptions.getInstance().isUseClassicMoveFixedDistance()) {
      myGetKeyCommands();
      if (moveCommand.matches(stroke)) {        
        return myKeyEvent(stroke);
      }
    }
    // For New MFD, use standard trait execution timing
    return super.keyEvent(stroke);
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (moveCommand.matches(stroke)) {      
      return GlobalOptions.getInstance().isUseClassicMoveFixedDistance() ? classicTranslate(stroke) : newTranslate(stroke);
    }
    return null;
  }
  
  /*
   * New Translate code.
   * Simplified. Get rid of Move Batcher. Use same technique as Send To Location to
   * move counters. A series of Translate commands issues by a Trigger Action will now act as
   * expected and Undo properly.
   * 
   * NOTE: If the Stack Move option is used and a series of MFD commands are issued by a Trigger Action
   * then the moving pieces will 'pick up' any pieces they land on along the way. The Stack Move option is not
   * recommended for this reason and now defaults to 'N' and is marked in the Editor as 'Not Recommended'.
   */
  protected Command newTranslate (KeyStroke stroke) {
    
    GamePiece target = findTarget(stroke);
    if (target == null) {
      return null;
    }
    
    // Current Position
    Point p = getPosition();
    
    // Calculate the destination
    translate(p);
    
    // Handle rotation of the piece, movement is relative to the current facing of the unit.
    // Use the first Rotator trait below us, if none, use the highest in the stack.
    FreeRotator myRotation = (FreeRotator) Decorator.getDecorator(this, FreeRotator.class);
    if (myRotation == null) {
      myRotation = (FreeRotator) Decorator.getDecorator(Decorator.getOutermost(this), FreeRotator.class);
    }
    if (myRotation != null) {
      Point2D myPosition = getPosition().getLocation();
      Point2D p2d = p.getLocation();
      p2d = AffineTransform.getRotateInstance(myRotation.getCumulativeAngleInRadians(), myPosition.getX(), myPosition.getY()).transform(p2d, null);
      p = new Point((int) p2d.getX(), (int) p2d.getY());
    }

    // And snap to the grid if required.
    if (!Boolean.TRUE.equals(Decorator.getOutermost(this).getProperty(Properties.IGNORE_GRID))) {
      p = getMap().snapTo(p);
    }
    
    // Move the piece(s)
    Command c = new NullCommand();
    if (target instanceof Stack) {
      for (GamePiece gp : ((Stack) target).asList()) {
        final boolean pieceSelected = Boolean.TRUE.equals(gp.getProperty(Properties.SELECTED));
        if (pieceSelected || moveStack) {
          c = c.append(movePiece(gp, p));
        }
      }
    }
    else {
      c = c.append(movePiece(target, p));
    }
    
    return c;
  }
  
  /*
   * Move a single piece to a destination
   */
  protected Command movePiece(GamePiece gp, Point dest) {
    
    // Is the piece on a map?
    final Map map = gp.getMap();
    if (map == null) {
      return null;      
    }

    // Set the Old... properties
    Command c = setOldProperties(this);
    
    // Move the piece
    final GamePiece outer = Decorator.getOutermost(gp);
    c = c.append(map.placeOrMerge(outer, dest));
   
    // Apply after Move Key
    if (map.getMoveKey() != null) {
      c = c.append(outer.keyEvent(map.getMoveKey()));
    }
    
    // Unlink from Parent Stack (in case it is a Deck).
    final Stack parent = outer.getParent();
    if (parent != null) {
      c = c.append(parent.pieceRemoved(outer));
    }
      
    return c;
  }

  /**
   * Classic Translate code.
   * The original Move Fixed Distance code does not work properly in Triggers, creates additional Null
   * actions and does not undo properly. Some modules may depend on this behaviour. Now depends on a Module level
   * preference being turned on to use it.
   */
  protected Command classicTranslate(KeyStroke stroke) {
    Command c = new NullCommand();

    if (mover == null) {
      mover = new MoveExecuter();
      mover.setKeyEvent(stroke);
      mover.setAdditionalCommand(setOldProperties(this));
      SwingUtilities.invokeLater(mover);
    }
    GamePiece target = findTarget(stroke);
    if (target != null) {
      c = c.append(moveTarget(target));
    }
    mover.addKeyEventTarget(piece);
    // Return a non-null command to indicate that a change actually happened
    // Note: Looks weird to wipe out the Commands, but they have all been added to the Move Executor.
    c = new NullCommand() {
      @Override
      public boolean isNull() {
        return false;
      }
    };

    return c;
  }

  protected Command moveTarget(GamePiece target) {
    // Has this piece already got a move scheduled? If so, then we
    // need to use the endpoint of any existing moves as our
    // starting point.
    Point p = mover.getUpdatedPosition(target);

    // First move, so use the current location.
    if (p == null) {
      p = new Point(getPosition());
    }

    // Perform the move fixed distance
    translate(p);

    // Handle rotation of the piece
    FreeRotator myRotation = (FreeRotator) Decorator.getDecorator(this, FreeRotator.class);
    if (myRotation != null) {
      Point2D myPosition = getPosition().getLocation();
      Point2D p2d = p.getLocation();
      p2d = AffineTransform.getRotateInstance(myRotation.getCumulativeAngleInRadians(), myPosition.getX(), myPosition.getY()).transform(p2d, null);
      p = new Point((int) p2d.getX(), (int) p2d.getY());
    }

    // And snap to the grid if required.
    if (!Boolean.TRUE.equals(Decorator.getOutermost(this).getProperty(Properties.IGNORE_GRID))) {
      p = getMap().snapTo(p);
    }

    // Add to the list of scheduled moves
    mover.add(target.getMap(), target, p);
    return null;
  }

  protected void translate(Point p) {
    int x = 0;
    int y = 0;
    final GamePiece outer = Decorator.getOutermost(this);
    final Board b = outer.getMap().findBoard(p);

    final int Xdist = xDist.getTextAsInt(outer, "Xdistance", this);
    final int Xindex = xIndex.getTextAsInt(outer, "Xindex", this);
    final int Xoffset = xOffset.getTextAsInt(outer, "Xoffset", this);

    x = Xdist + Xindex * Xoffset;
    if (b != null) {
      x = (int)Math.round(b.getMagnification()*x);
    }

    final int Ydist = yDist.getTextAsInt(outer, "Ydistance", this);
    final int Yindex = yIndex.getTextAsInt(outer, "Yindex", this);
    final int Yoffset = yOffset.getTextAsInt(outer, "Yoffset", this);

    y = Ydist + Yindex * Yoffset;
    if (b != null) {
      y = (int)Math.round(b.getMagnification()*y);
    }

    p.translate(x, -y);
  }

  protected GamePiece findTarget(KeyStroke stroke) {
    final GamePiece outer = Decorator.getOutermost(this);
    GamePiece target = outer;
    if (moveStack
        && outer.getParent() != null
        && !outer.getParent().isExpanded()) {
      // Only move entire stack if this is the top piece
      // Otherwise moves the stack too far if the whole stack is multi-selected
      if (outer != outer.getParent().topPiece(GameModule.getUserId())) {
        target = null;
      }
      else {
        target = outer.getParent();
      }
    }
    return target;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return getInner().getName();
  }

  @Override
  public Shape getShape() {
    return getInner().getShape();
  }

  @Override
  public PieceEditor getEditor() {
    return new Editor(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Translate.htm");
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(commandName, getCommandDescription(description, "Move Fixed Distance command"));
  }


  public static class Editor implements PieceEditor {
    private FormattedExpressionConfigurer xDist;
    private FormattedExpressionConfigurer yDist;
    private StringConfigurer name;
    private NamedHotKeyConfigurer key;
    private JPanel controls;
    private BooleanConfigurer moveStack;
    protected BooleanConfigurer advancedInput;
    protected FormattedExpressionConfigurer xIndexInput;
    protected FormattedExpressionConfigurer xOffsetInput;
    protected FormattedExpressionConfigurer yIndexInput;
    protected FormattedExpressionConfigurer yOffsetInput;
    protected StringConfigurer descInput;

    public Editor(Translate t) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      descInput = new StringConfigurer(null, "Description:  ", t.description);
      controls.add(descInput.getControls());
      name = new StringConfigurer(null, "Command Name:  ", t.commandName);
      controls.add(name.getControls());
      key = new NamedHotKeyConfigurer(null, "Keyboard shortcut:  ", t.keyCommand);
      controls.add(key.getControls());
      xDist = new FormattedExpressionConfigurer(null, "Distance to the right:  ", t.xDist.getFormat(), t);
      controls.add(xDist.getControls());
      yDist = new FormattedExpressionConfigurer(null, "Distance upwards:  ", t.yDist.getFormat(), t);
      controls.add(yDist.getControls());
      // Hint that Move Entire Stack, even in fixed code, has problems.
      moveStack = new BooleanConfigurer(null, "Move entire stack? (Not Recommended)",
                                        Boolean.valueOf(t.moveStack));
      controls.add(moveStack.getControls());

      advancedInput = new BooleanConfigurer(null, "Advanced Options", false);
      advancedInput.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent e) {
          updateAdvancedVisibility();
        }});
      controls.add(advancedInput.getControls());

      Box b = Box.createHorizontalBox();
      xIndexInput = new FormattedExpressionConfigurer(null, "Additional offset to the right:  ", t.xIndex.getFormat(), t);
      b.add(xIndexInput.getControls());
      xOffsetInput = new FormattedExpressionConfigurer(null, " times ", t.xOffset.getFormat(), t);
      b.add(xOffsetInput.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      yIndexInput = new FormattedExpressionConfigurer(null, "Additional offset upwards:  ", t.yIndex.getFormat(), t);
      b.add(yIndexInput.getControls());
      yOffsetInput = new FormattedExpressionConfigurer(null, " times ", t.yOffset.getFormat(), t);
      b.add(yOffsetInput.getControls());
      controls.add(b);

      updateAdvancedVisibility();
    }

    private void updateAdvancedVisibility() {
      boolean visible = advancedInput.booleanValue();
      xIndexInput.getControls().setVisible(visible);
      xOffsetInput.getControls().setVisible(visible);
      yIndexInput.getControls().setVisible(visible);
      yOffsetInput.getControls().setVisible(visible);
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(name.getValueString())
        .append(key.getValueString())
        .append(xDist.getValueString())
        .append(yDist.getValueString())
        .append(moveStack.getValueString())
        .append(xIndexInput.getValueString())
        .append(yIndexInput.getValueString())
        .append(xOffsetInput.getValueString())
        .append(yOffsetInput.getValueString())
        .append(descInput.getValueString());
      return ID + se.getValue();
    }
  }

  /**
   * Batches up all the movement commands resulting from a single KeyEvent
   * and executes them at once. Ensures that pieces that are moving won't
   * be merged with other moving pieces until they've been moved.
   */
  public static class MoveExecuter implements Runnable {
    private List<Move> moves = new ArrayList<>();
    private Set<GamePiece> pieces = new HashSet<>();
    private KeyStroke stroke;
    private List<GamePiece> innerPieces = new ArrayList<>();
    private Command additionalCommand;

    @Override
    public void run() {
      mover = null;
      Command comm = new NullCommand();
      comm = comm.append(additionalCommand);
      
      for (final Move move : moves) {
        final Map.Merger merger =
          new Map.Merger(move.map, move.pos, move.piece);
        DeckVisitor v = new DeckVisitor() {
          @Override
          public Object visitDeck(Deck d) {
            return merger.visitDeck(d);
          }

          @Override
          public Object visitStack(Stack s) {
            if (!pieces.contains(s) &&
                move.map.getPieceCollection().canMerge(s, move.piece)) {
              return merger.visitStack(s);
            }
            else {
              return null;
            }
          }

          @Override
          public Object visitDefault(GamePiece p) {
            if (!pieces.contains(p) &&
                move.map.getPieceCollection().canMerge(p, move.piece)) {
              return merger.visitDefault(p);
            }
            else {
              return null;
            }
          }
        };

        DeckVisitorDispatcher dispatch = new DeckVisitorDispatcher(v);
        Command c = move.map.apply(dispatch);
        if (c == null) {
          c = move.map.placeAt(move.piece, move.pos);
          // Apply Auto-move key
          if (move.map.getMoveKey() != null) {
            c.append(Decorator.getOutermost(move.piece)
                              .keyEvent(move.map.getMoveKey()));
          }
        }
        comm.append(c);
        if (move.piece.getMap() == move.map) {
          move.map.ensureVisible(move.map.selectionBoundsOf(move.piece));
        }
        pieces.remove(move.piece);
        move.map.repaint();
      }
      MovementReporter r = new MovementReporter(comm);
      if (GlobalOptions.getInstance().autoReportEnabled()) {
        Command reportCommand = r.getReportCommand();
        if (reportCommand != null) {
          reportCommand.execute();
        }
        comm.append(reportCommand);
      }
      comm.append(r.markMovedPieces());
      if (stroke != null) {
        for (GamePiece gamePiece : innerPieces) {
          comm.append(gamePiece.keyEvent(stroke));
        }
      }
      GameModule.getGameModule().sendAndLog(comm);
    }

    public void add(Map map, GamePiece piece, Point pos) {
      moves.add(new Move(map, piece, pos));
      pieces.add(piece);
    }

    public void addKeyEventTarget(GamePiece piece) {
      innerPieces.add(piece);
    }

    public void setKeyEvent(KeyStroke stroke) {
      this.stroke = stroke;
    }
    
    public void setAdditionalCommand(Command c) {
      additionalCommand = c;
    }

    public Command getAdditionalCommand() {
      return additionalCommand;
    }
    /**
     * Return the updated position of a piece that has a move
     * calculation recorded
     *
     * @param target piece to check
     * @return updated position
     */
    public Point getUpdatedPosition(GamePiece target) {
      Point p = null;
      for (Move move : moves) {
        if (move.piece == target) {
          p = move.pos;
        }
      }
      return p;
    }

    private static class Move {
      private Map map;
      private GamePiece piece;
      private Point pos;

      public Move(Map map, GamePiece piece, Point pos) {
        this.map = map;
        this.piece = piece;
        this.pos = pos;
      }
    }
  }
}
