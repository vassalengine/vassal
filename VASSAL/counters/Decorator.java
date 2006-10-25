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

import java.awt.Point;
import javax.swing.KeyStroke;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.tools.SequenceEncoder;

/**
 * The abstract class describing a generic 'trait' of a GamePiece.  Follows the Decorator design pattern
 * of wrapping around another instance of GamePiece (the 'inner' piece) and delegating some of the GamePiece methods to it
 */
public abstract class Decorator implements GamePiece, StateMergeable {
  protected GamePiece piece;
  private Decorator dec;

  public Decorator() {
  }

  /** Set the inner GamePiece */
  public void setInner(GamePiece p) {
    piece = p;
    if (p != null) {
      p.setProperty(Properties.OUTER, this);
    }
  }

  public void setMap(Map m) {
    piece.setMap(m);
  }

  /**
   * @return the piece decorated by this Decorator
   */
  public GamePiece getInner() {
    return piece;
  }

  public Map getMap() {
    return piece.getMap();
  }

  public void setParent(Stack s) {
    piece.setParent(s);
  }

  public Stack getParent() {
    return piece.getParent();
  }

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
      return myGetState()+piece.getProperty(key);
    }
    else {
      return piece.getProperty(key);
    }
  }

  public void setProperty(Object key, Object val) {
    if (Properties.INNER.equals(key)) {
      setInner((GamePiece) val);
    }
    else if (Properties.OUTER.equals(key)) {
      dec = (Decorator) val;
    }
    else {
      piece.setProperty(key, val);
    }
  }

  public void setPosition(Point p) {
    piece.setPosition(p);
  }

  public Point getPosition() {
    return piece.getPosition();
  }

  /** Set just the state of this trait
   * @see #myGetState
   */
  public abstract void mySetState(String newState);

  /**
   * Extract the string describing this trait's state and forward the remaining string to the inner piece
   * @param newState the new state of this trait and all inner pieces
   */
  public void setState(String newState) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, '\t');
    mySetState(st.nextToken());
    piece.setState(st.nextToken());
  }

  /**
   * Compute the difference between <code>newState</code> and <code>oldState</code>
   * and appy that difference to the current state
   * @param newState
   * @param oldState
   */
  public void mergeState(String newState, String oldState) {
    SequenceEncoder.Decoder stNew = new SequenceEncoder.Decoder(newState, '\t');
    String myNewState = stNew.nextToken();
    String innerNewState = stNew.nextToken();
    SequenceEncoder.Decoder stOld = new SequenceEncoder.Decoder(oldState, '\t');
    String myOldState = stOld.nextToken();
    String innerOldState = stOld.nextToken();
    if (!myOldState.equals(myNewState)) {
      mySetState(myNewState);
    }
    if (piece instanceof StateMergeable) {
      ((StateMergeable)piece).mergeState(innerNewState,innerOldState);
    }
    else {
      piece.setState(innerNewState);
    }
  }

  /**
   *
   * @return the state of this trait alone
   * @see #getState
   */
  public abstract String myGetState();

  /** The state of a Decorator is a composition of {@link #myGetState} and the inner piece's state */
  public String getState() {
    SequenceEncoder se = new SequenceEncoder(myGetState(), '\t');
    se.append(piece.getState());
    return se.getValue();
  }

  /**
   *
   * @return the type of this trait alone
   * @see #getType
   */
  public abstract String myGetType();

  /**
   * The type of a Decorator is a composition of {@link #myGetType} and the type of its inner piece
   * @return the combined type of this trait and its inner piece
   */
  public String getType() {
    SequenceEncoder se = new SequenceEncoder(myGetType(), '\t');
    se.append(piece.getType());
    return se.getValue();
  }

  /**
   *
   * @return the commands for this trait alone
   * @see #getKeyCommands
   */
  protected abstract KeyCommand[] myGetKeyCommands();

  /**
   * The set of key commands that will populate the piece's right-click menu.
   * The key commands are accessible through the {@link Properties#KEY_COMMANDS} property.
   * The commands for a Decorator are a composite of {@link #myGetKeyCommands} and the
   * commands of its inner piece.
   * @return the commands for this piece and its inner piece
   */
  protected KeyCommand[] getKeyCommands() {
    KeyCommand myC[] = myGetKeyCommands();
    KeyCommand c[] = (KeyCommand[]) piece.getProperty(Properties.KEY_COMMANDS);
    if (c == null) {
      return myC;
    }
    else if (myC == null) {
      return c;
    }
    else {
      KeyCommand all[] = new KeyCommand[c.length + myC.length];
      System.arraycopy(myC, 0, all, 0, myC.length);
      System.arraycopy(c, 0, all, myC.length, c.length);
      return all;
    }
  }

  /**
   * The response of this trait alone to the given KeyStroke
   * @param stroke
   * @return null if no effect
   * @see #keyEvent
   */
  public abstract Command myKeyEvent(KeyStroke stroke);

  /**
   * Append the command returned by {@link #myKeyEvent} with the command returned
   * by the inner piece's {@link GamePiece#keyEvent} method.
   * @param stroke
   * @return
   */
  public Command keyEvent(KeyStroke stroke) {
    Command c = myKeyEvent(stroke);
    return c == null ? piece.keyEvent(stroke)
      : c.append(piece.keyEvent(stroke));
  }

  public String getId() {
    return piece.getId();
  }

  public void setId(String id) {
    piece.setId(id);
  }

  /**
   * @param p
   * @return the outermost Decorator instance of this piece, i.e. the entire piece with all traits
   */
  public static GamePiece getOutermost(GamePiece p) {
    while (p.getProperty(Properties.OUTER) != null) {
      p = (GamePiece) p.getProperty(Properties.OUTER);
    }
    return p;
  }

  /**
   *
   * @param p
   * @return the innermost GamePiece of this piece.  In most cases, an instance of {@link BasicPiece}
   */
  public static GamePiece getInnermost(GamePiece p) {
    while (p instanceof Decorator) {
      p = ((Decorator) p).piece;
    }
    return p;
  }

  /**
   * @return the first Decorator within the given GamePiece
   * that is an instance of the given Class
   */
  public static GamePiece getDecorator(GamePiece p, Class type) {
    while (p instanceof Decorator) {
      if (type.isInstance(p)) {
        return p;
      }
      p = ((Decorator) p).piece;
    }
    return null;
  }

  public PieceEditor getEditor() {
    return new SimplePieceEditor(this);
  }
  
  public String toString() {
    if (piece == null) {
      return super.toString();
    }
    else {
      return super.toString()+"[name="+getName()+",type="+getType()+",state="+getState()+"]";
    }
  }
}
