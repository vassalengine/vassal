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

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import javax.swing.JLabel;
import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


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
  protected String deselectType;

  // Translated Destination descriptions
  protected static final String DESELECT_THIS = "D"; // NON-NLS
  protected static final String DESELECT_ALL = "A"; // NON-NLS
  protected static final String DESELECT_SELECT_ONLY = "S"; // NON-NLS

  protected static final String[] DESELECT_OPTIONS = {
    DESELECT_THIS, DESELECT_ALL, DESELECT_SELECT_ONLY
  };
  // Actual valued recorded for Destination option
  protected static final String[] DESELECT_KEYS = {
    "Editor.Deselect.deselect_this_piece",
    "Editor.Deselect.deselect_all_pieces",
    "Editor.Deselect.select_only_this_piece"
  };


  public Deselect() {
    commandName = Resources.getString("Editor.Deselect.deselect");
    key = NamedKeyStroke.of(KeyStroke.getKeyStroke("K"));
    description = "";
    unstack = false;
    deselectType = DESELECT_THIS;
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
    deselectType = st.nextToken(DESELECT_THIS);

    command = null;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(DELIMITER);
    se.append(commandName).append(key).append(description).append(unstack).append(deselectType);
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

      if (DESELECT_ALL.equals(deselectType)) {
        DragBuffer.getBuffer().clear();
        KeyBuffer.getBuffer().clear();
      }
      else if (DESELECT_SELECT_ONLY.equals(deselectType)) {
        DragBuffer.getBuffer().clear();
        KeyBuffer.getBuffer().clear();
        DragBuffer.getBuffer().add(outer);
        KeyBuffer.getBuffer().add(outer);
      }
      else {
        if (unstack) {
          final Stack stack = outer.getParent();       //BR// If we're now being dragged around as part of a stack
          if (stack != null) {
            final Point pos = outer.getPosition();     //BR// Figure out where stack was/is

            c = m.placeAt(outer, pos);                 //BR// Place it right on the map (which auto-removes it from stack)

            final Stack parent = m.getStackMetrics().createStack(outer);
            if (parent != null) {
              c = c.append(m.placeAt(parent, pos));    //BR// Place it in a new stack at the same location
            }
          }
        }
        DragBuffer.getBuffer().remove(outer);          //BR// Remove from the drag buffer
        KeyBuffer.getBuffer().remove(outer);           //BR// Remove from the key buffer
      }
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
    String s = buildDescription("Editor.Deselect.deselect", description);
    s += getCommandDesc(commandName, key);
    return s;
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.Deselect.deselect");
  }

  @Override
  public String getDescriptionField() {
    return description;
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
    if (! Objects.equals(deselectType, c.deselectType)) return false;
    return Objects.equals(key, c.key);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer nameInput;
    private final StringConfigurer descInput;
    private final NamedHotKeyConfigurer keyInput;
    private final BooleanConfigurer unstackInput;
    private final TraitConfigPanel controls;
    private final TranslatingStringEnumConfigurer deselectTypeInput;
    private final JLabel unstackLabel;

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

      unstackLabel = new JLabel(Resources.getString("Editor.Deselect.remove_piece_from_stack"));
      unstackInput = new BooleanConfigurer(p.unstack);
      controls.add(unstackLabel, unstackInput);

      deselectTypeInput = new TranslatingStringEnumConfigurer(DESELECT_OPTIONS, DESELECT_KEYS);
      deselectTypeInput.setValue(DESELECT_THIS);
      for (final String deselectOption : DESELECT_OPTIONS) {
        if (deselectOption.substring(0, 1).equals(p.deselectType)) {
          deselectTypeInput.setValue(deselectOption);
        }
      }
      deselectTypeInput.addPropertyChangeListener(arg0 -> updateVisibility());
      controls.add("Editor.Deselect.deselection_type", deselectTypeInput);

      updateVisibility();
    }

    private void updateVisibility() {
      unstackLabel.setVisible(DESELECT_THIS.equals(deselectTypeInput.getValueString()));
      unstackInput.getControls().setVisible(DESELECT_THIS.equals(deselectTypeInput.getValueString()));
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(DELIMITER);
      se.append(nameInput.getValueString()).append(keyInput.getValueString()).append(descInput.getValueString()).append(unstackInput.getValueString()).append(deselectTypeInput.getValueString());
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
    return new ArrayList<>();
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
