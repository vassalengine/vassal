/*
 *
 * Copyright (c) 2020 by VassalEngine.org, Brian Reynolds
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
import java.awt.Point;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import java.util.ArrayList;
import java.util.List;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.configure.StringConfigurer;


/**
 * Implements a trait to allow a piece to be deselected from the KeyBuffer in response to a Key Command.
 * @author Brian Reynolds
 */
public class Deselect extends Decorator implements TranslatablePiece {
  private static final char DELIMITER = '\t'; //$NON-NLS-1$
  public static final String ID = "deselect" + DELIMITER;
  protected KeyCommand[] command;
  protected String commandName;
  protected NamedKeyStroke key;
  protected KeyCommand deselectCommand;
  protected String description;
  protected Boolean unstack;

  public Deselect() {
    commandName = "Deselect";
    key = new NamedKeyStroke(KeyStroke.getKeyStroke("K"));
    description = "";
    unstack = false;
  }

  public Deselect(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, DELIMITER);
    commandName = st.nextToken();
    key = st.nextNamedKeyStroke('K');
    description = st.nextToken("");
    unstack = st.nextBoolean(false);
    command = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(DELIMITER);
    se.append(commandName).append(key).append(description).append(unstack);
    return ID + se.getValue();
  }

  public KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      deselectCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this), this);
      if (commandName.length() > 0 && key != null && ! key.isNull()) {
        command =
            new KeyCommand[]{deselectCommand};
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
    if (deselectCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);

      final Map m = getMap();

      if (unstack) {
        Stack stack = outer.getParent();      //BR// If we're now being dragged around as part of a stack
        if (stack != null) {
          Point pos = outer.getPosition();    //BR// Figure out where stack was/is
          stack.setExpanded(true);            //BR// Expand the stack
          stack.remove(outer);                //BR// Remove our piece from the stack
          c = m.placeAt(outer, pos);          //BR// Put it back on the map so it won't be missing
        }
      }
      outer.setProperty(Properties.SELECTED, false); //BR// Mark as not selected
      DragBuffer.getBuffer().remove(outer); //BR// Remove from the drag buffer
      KeyBuffer.getBuffer().remove(outer);  //BR// Remove from the key buffer
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
    return (description == null || description.length() == 0) ? Resources.getString("Deselect.Deselect") : Resources.getString("Deselect.Deselect") + " - " + description;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Deselect.htm", "");
  }

  public PieceI18nData getI18nData() {
    return getI18nData(commandName, "Deselect command");
  }

  public static class Ed implements PieceEditor {
    private StringConfigurer nameInput;
    private StringConfigurer descInput;
    private NamedHotKeyConfigurer keyInput;
    private BooleanConfigurer unstackInput;
    private JPanel controls;

    public Ed(Deselect p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      descInput = new StringConfigurer(null, Resources.getString("Editor.description_label"), p.description);
      controls.add(descInput.getControls());

      nameInput = new StringConfigurer(null, Resources.getString("Editor.menu_command"), p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new NamedHotKeyConfigurer(null, Resources.getString("Editor.keyboard_command"), p.key);
      controls.add(keyInput.getControls());

      unstackInput = new BooleanConfigurer(null, Resources.getString("Editor.Deselect.remove_piece_from_stack"), p.unstack);
      controls.add(unstackInput.getControls());
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(DELIMITER);
      se.append(nameInput.getValueString()).append(keyInput.getValueString()).append(descInput.getValueString()).append(unstackInput.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }


  /**
   * Return Property names exposed by this trait
   */
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<String>();
    l.add(Properties.SELECTED);
    return l;
  }
}
