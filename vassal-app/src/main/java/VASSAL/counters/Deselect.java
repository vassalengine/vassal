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

import java.util.Objects;
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
  private static final char DELIMITER = ';'; //$NON-NLS-1$
  public static final String ID = "deselect" + DELIMITER; // NON-NLS
  protected KeyCommand[] command;
  protected String commandName;
  protected NamedKeyStroke key;
  protected KeyCommand deselectCommand;
  protected String description;
  protected Boolean unstack;

  public Deselect() {
    commandName = Resources.getString("Editor.Deselect.deselect");
    key = NamedKeyStroke.of(KeyStroke.getKeyStroke("K"));
    description = "";
    unstack = false;
  }

  public Deselect(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, DELIMITER);
    commandName = st.nextToken();
    key = st.nextNamedKeyStroke('K');
    description = st.nextToken("");
    unstack = st.nextBoolean(false);
    command = null;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(DELIMITER);
    se.append(commandName).append(key).append(description).append(unstack);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      deselectCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this), this);
      if (commandName.length() > 0 && key != null && !key.isNull()) {
        command =
          new KeyCommand[]{deselectCommand};
      }
      else {
        command = KeyCommand.NONE;
      }
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
    Command c = null;
    myGetKeyCommands();
    if (deselectCommand.matches(stroke)) {
      final GamePiece outer = Decorator.getOutermost(this);

      final Map m = getMap();

      if (unstack) {
        final Stack stack = outer.getParent();      //BR// If we're now being dragged around as part of a stack
        if (stack != null) {
          final Point pos = outer.getPosition();    //BR// Figure out where stack was/is
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
    return buildDescription("Editor.Deselect.deselect", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Deselect.html", ""); // NON-NLS
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(commandName, Resources.getString("Editor.Deselect.deselect_command"));
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Deselect)) return false;
    final Deselect c = (Deselect) o;
    if (! Objects.equals(commandName, c.commandName)) return false;
    if (! Objects.equals(description, c.description)) return false;
    if (! Objects.equals(unstack, c.unstack)) return false;
    return Objects.equals(key, c.key);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer nameInput;
    private final StringConfigurer descInput;
    private final NamedHotKeyConfigurer keyInput;
    private final BooleanConfigurer unstackInput;
    private final TraitConfigPanel controls;

    public Ed(Deselect p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.description);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      nameInput = new StringConfigurer(p.commandName);
      nameInput.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", nameInput);

      keyInput = new NamedHotKeyConfigurer(p.key);
      controls.add("Editor.keyboard_command", keyInput);

      unstackInput = new BooleanConfigurer(p.unstack);
      controls.add("Editor.Deselect.remove_piece_from_stack", unstackInput);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(DELIMITER);
      se.append(nameInput.getValueString()).append(keyInput.getValueString()).append(descInput.getValueString()).append(unstackInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(Properties.SELECTED);
    return l;
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return List.of(key);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(commandName);
  }
}
