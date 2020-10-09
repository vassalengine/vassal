/*
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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * This trait adds a command that creates a duplicate of the selected Gamepiece
 */
public class MenuSeparator extends Decorator implements TranslatablePiece {
  public static final String ID = "menuSeparator;";
  public static final String SEPARATOR_NAME = "<separator>";
  protected KeyCommand[] command;
  protected String desc;
  protected NamedKeyStroke key;
  protected KeyCommand separatorCommand;

  public MenuSeparator() {
    this(ID + ";", null);
  }

  public MenuSeparator(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }


  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    desc        = st.nextToken();
    key         = st.nextNamedKeyStroke(null);
    command     = null;
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(desc).append(key);
    return ID + se.getValue();
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      separatorCommand = new KeyCommand(SEPARATOR_NAME, key, Decorator.getOutermost(this), this);
      command = new KeyCommand[]{separatorCommand};
    }
    if (command.length > 0) {
      command[0].setEnabled(getMap() != null);
    }
    return command;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null; // We don't ever actually "do" anything to the game state, we're just here to mark a menu separator
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

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public String getDescription() {
    if ((desc != null) && !desc.isEmpty()) {
      return "Menu Separator" + " - " + desc;
    }
    else {
      return "Menu Separator";
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MenuSeparator.html");
  }


  public static class Ed implements PieceEditor {
    private StringConfigurer descInput;
    private NamedHotKeyConfigurer keyInput;
    private JPanel controls;

    public Ed(MenuSeparator p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      descInput = new StringConfigurer(null, "Separator Description:  ", p.desc);
      controls.add(descInput.getControls());

      keyInput = new NamedHotKeyConfigurer(null, "If this keystroke is hidden, hide separator:  ", p.key);
      controls.add(keyInput.getControls());
    }


    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(descInput.getValueString()).append(keyInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
