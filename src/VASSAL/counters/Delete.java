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

package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * This trait adds a command that creates a duplicate of the selected Gamepiece
 */
public class Delete extends Decorator implements TranslatablePiece {
  public static final String ID = "delete;";
  protected KeyCommand[] keyCommands;
  protected KeyCommand deleteCommand;
  protected String commandName;
  protected NamedKeyStroke key;

  public Delete() {
    this(ID + "Delete;D", null);
  }

  public Delete(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken();
    key = st.nextNamedKeyStroke('D');
    keyCommands = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(key);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (keyCommands == null) {
      deleteCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this), this);
      if (commandName.length() > 0 && key != null && ! key.isNull()) {
        keyCommands = new KeyCommand[]{deleteCommand};
      }
      else {
        keyCommands = new KeyCommand[0];
      }
    }
    deleteCommand.setEnabled(getMap() != null);
    return keyCommands;
  }

  public String myGetState() {
    return "";
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (deleteCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      if (getParent() != null) {
        GamePiece next = getParent().getPieceBeneath(outer);
        if (next == null)
          next = getParent().getPieceAbove(outer);
        if (next != null) {
          final GamePiece selected = next;
          Runnable runnable = new Runnable() {
            public void run() {
              // Don't select if the next piece has itself been deleted
              if (GameModule.getGameModule().getGameState().getPieceForId(selected.getId()) != null) {
                KeyBuffer.getBuffer().add(selected);
              }
            }
          };
          SwingUtilities.invokeLater(runnable);
        }
      }
      c = new RemovePiece(outer);
      c.execute();
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
    return "Delete";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePiece.htm", "Delete");
  }

  public PieceI18nData getI18nData() {
    return getI18nData(commandName, "Delete command");
  }

  public static class Ed implements PieceEditor {
    private StringConfigurer nameInput;
    private NamedHotKeyConfigurer keyInput;
    private JPanel controls;

    public Ed(Delete p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new NamedHotKeyConfigurer(null, "Keyboard Command:  ", p.key);
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
