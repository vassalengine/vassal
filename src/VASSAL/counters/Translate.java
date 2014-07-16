/*
 * $Id$
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

  public String getDescription() {
    String d = "Move fixed distance";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken("Move Forward");
    keyCommand = st.nextNamedKeyStroke('M');
    xDist.setFormat(st.nextToken(_0));
    yDist.setFormat(st.nextToken("60"));
    moveStack = st.nextBoolean(true);
    xIndex.setFormat(st.nextToken(_0));
    yIndex.setFormat(st.nextToken(_0));
    xOffset.setFormat(st.nextToken(_0));
    yOffset.setFormat(st.nextToken(_0));
    description = st.nextToken("");
    commands = null;
  }

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

  public String myGetState() {
    return "";
  }

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

  public Command keyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (moveCommand.matches(stroke)) {
      // Delay the execution of the inner piece's key event until this piece has moved
      return myKeyEvent(stroke);
    }
    else {
      return super.keyEvent(stroke);
    }
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (moveCommand.matches(stroke)) {
      setOldProperties();
      if (mover == null) {
        mover = new MoveExecuter();
        mover.setKeyEvent(stroke);
        SwingUtilities.invokeLater(mover);
      }
      GamePiece target = findTarget(stroke);
      if (target != null) {
        c = moveTarget(target);
      }
      mover.addKeyEventTarget(piece);
      // Return a non-null command to indicate that a change actually happened
      c = new NullCommand() {
        public boolean isNull() {
          return false;
        }
      };
    }
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

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return getInner().getName();
  }

  public Shape getShape() {
    return getInner().getShape();
  }

  public PieceEditor getEditor() {
    return new Editor(this);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Translate.htm");
  }

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
      moveStack = new BooleanConfigurer(null, "Move entire stack?",
                                        Boolean.valueOf(t.moveStack));
      controls.add(moveStack.getControls());

      advancedInput = new BooleanConfigurer(null, "Advanced Options", false);
      advancedInput.addPropertyChangeListener(new PropertyChangeListener() {
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
      boolean visible = advancedInput.booleanValue().booleanValue();
      xIndexInput.getControls().setVisible(visible);
      xOffsetInput.getControls().setVisible(visible);
      yIndexInput.getControls().setVisible(visible);
      yOffsetInput.getControls().setVisible(visible);
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }

    public Component getControls() {
      return controls;
    }

    public String getState() {
      return "";
    }

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
    private List<Move> moves = new ArrayList<Move>();
    private Set<GamePiece> pieces = new HashSet<GamePiece>();
    private KeyStroke stroke;
    private List<GamePiece> innerPieces = new ArrayList<GamePiece>();

    public void run() {
      mover = null;
      Command comm = new NullCommand();
      for (final Move move : moves) {
        final Map.Merger merger =
          new Map.Merger(move.map, move.pos, move.piece);
        DeckVisitor v = new DeckVisitor() {
          public Object visitDeck(Deck d) {
            return merger.visitDeck(d);
          }

          public Object visitStack(Stack s) {
            if (!pieces.contains(s) &&
                move.map.getPieceCollection().canMerge(s, move.piece)) {
              return merger.visitStack(s);
            }
            else {
              return null;
            }
          }

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
