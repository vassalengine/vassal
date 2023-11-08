/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.List;
import java.util.Objects;

/**
 * A trait that exists in a Piece Definition, but is not included when
 * the piece is created in a real game.
 * Behaviour should be exactly the same as a UsePrototype trait that does
 * not have a matching definition for the selected protoype.
 */
public class Comment extends Decorator implements EditablePiece {
  public static final String ID = "cmt;"; // NON-NLS
  private String comment;

  public Comment() {
    this(ID, null);
  }

  public Comment(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public String getDescription() {
    return comment;
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.Comment.trait_description");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Comment.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    comment = st.nextToken("");
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(comment);
    return ID + se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void mySetState(String newState) {
  }

  public String getComment() {
    return comment;
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

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Comment)) return false;
    final Comment c = (Comment) o;
    return Objects.equals(comment, c.comment);
  }

  @Override
  public PieceEditor getEditor() {
    return new Editor(this);
  }

  public static class Editor implements PieceEditor {
    private final TraitConfigPanel controls;
    private final StringConfigurer commentConfig;

    public Editor(Comment cmt) {
      controls = new TraitConfigPanel();

      commentConfig = new StringConfigurer(cmt.comment);
      controls.add("Editor.Comment.comment", commentConfig);
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
      return ID + commentConfig.getValueString();
    }
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(comment);
  }
}
