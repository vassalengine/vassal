package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * This trait adds a command that creates a duplicate of the selected Gamepiece
 */
public class Clone extends Decorator implements TranslatablePiece {
  public static final String ID = "clone;";
  protected KeyCommand[] command;
  protected String commandName;
  protected NamedKeyStroke key;
  protected KeyCommand cloneCommand;

  public Clone() {
    this(ID + "Clone;C", null);
  }

  public Clone(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken();
    key = st.nextNamedKeyStroke('C');
    command = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(key);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      cloneCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this), this);
      if (commandName.length() > 0 && key != null && ! key.isNull()) {
        command =
            new KeyCommand[]{cloneCommand};
      }
      else {
        command = new KeyCommand[0];
      }
    }
    if (command.length > 0) {
      command[0].setEnabled(getMap() != null);
    }
    return command;
  }

  public String myGetState() {
    return "";
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (cloneCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      GamePiece newPiece = ((AddPiece) GameModule.getGameModule().decode(GameModule.getGameModule().encode(new AddPiece(outer)))).getTarget();
      newPiece.setId(null);
      GameModule.getGameModule().getGameState().addPiece(newPiece);
      newPiece.setState(outer.getState());
      c = new AddPiece(newPiece);
      if (getMap() != null) {
        c.append(getMap().placeOrMerge(newPiece,outer.getPosition()));
        KeyBuffer.getBuffer().remove(outer);
        KeyBuffer.getBuffer().add(newPiece);
      }
    }
    return c;
  }

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public String getDescription() {
    return "Clone";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePiece.htm","Clone");
  }

  public PieceI18nData getI18nData() {
    return getI18nData(commandName, "Clone command");
  }

  public static class Ed implements PieceEditor {
    private StringConfigurer nameInput;
    private NamedHotKeyConfigurer keyInput;
    private JPanel controls;

    public Ed(Clone p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new NamedHotKeyConfigurer(null,"Keyboard Command:  ",p.key);
      controls.add(keyInput.getControls());

    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString()).append(keyInput.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
