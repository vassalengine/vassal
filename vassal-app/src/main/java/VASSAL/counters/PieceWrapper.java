/*
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

import VASSAL.build.widget.PieceSlot;
import VASSAL.tools.NamedKeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.KeyStroke;

import VASSAL.command.Command;

/**
 * PieceWrapper
 *
 * The PieceWrapper class acts as a 'top-hat' Decorator added to every GamePiece in play in
 * a game. The PieceWrapper class allows for the coding of improved and efficient services
 * to the whole Decorator Stack that cannot easily be coded within the Decorator base class.
 *
 * A PieceWrapper is added to the top of the Decorator Stack when a piece is added to a game:-
 *  - When dragging a piece from a palette See {@link PieceSlot}
 *  - When creating a piece by Place Marker or Replace See {@link PlaceMarker}
 *  - When creating a piece in an At-start Stack or Deck See {@link VASSAL.build.module.map.SetupStack}
 *  - When creating a piece via an AddPiece Command from another client or save See {@link VASSAL.command.AddPiece}
 *
 *  PieceWrappers are not created for Piece or Prototype Definitions and will never appear in the Piece Editor.
 *  PieceWrappers are not saved in log or game files and are not transmitted across the network to other clients. Any
 *  PieceWrappers are stripped from GamePieces during the encoding of the AddPiece Command See {@link VASSAL.build.module.BasicCommandEncoder#encode(Command)}
 */
public class PieceWrapper extends Decorator {

  public static final String ID = "pw;"; // NON-NLS

  public PieceWrapper() {
    this(null);
  }

  public PieceWrapper(GamePiece p) {
    setInner(p);
  }

  public PieceWrapper(String type, GamePiece p) {
    this(p);
  }

  public PieceWrapper(GamePiece p, String state) {
    this(p);
    p.setState(state);
  }

  // A PieceWrapper has no Type
  @Override
  public String myGetType() {
    return ID;
  }

  // TODO This KeyCommand is for testing ONLY to give a visualisation of whether or not a GamePiece has a PieceWrapper
  // TODO THIS MUST BE REMOVED BEFORE RELEASE
  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[] { new KeyCommand("I have a PieceWrapper", new NamedKeyStroke("xxx"), this) }; // NON-NLS
    //return new KeyCommand[0];
  }

  // A PieceWrapper has no State
  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }
}
