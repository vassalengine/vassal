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
import java.util.Arrays;
import java.util.List;

import java.util.Objects;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * This trait puts a menu separator bar in the context menu for the piece
 */
public class MenuSeparator extends Decorator implements TranslatablePiece {
  public static final String ID = "menuSeparator;"; // NON-NLS
  public static final String SEPARATOR_NAME = "<separator>"; // NON-NLS
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
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    desc        = st.nextToken();
    key         = st.nextNamedKeyStroke(null);
    command     = null;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(desc).append(key);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
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
    return buildDescription("Editor.MenuSeparator.trait_description", desc);
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.MenuSeparator.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return desc;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof MenuSeparator)) return false;
    final MenuSeparator c = (MenuSeparator) o;
    if (!Objects.equals(desc, c.desc)) return false;
    return Objects.equals(key, c.key);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MenuSeparator.html"); // NON-NLS
  }


  public static class Ed implements PieceEditor {
    private final StringConfigurer descInput;
    private final NamedHotKeyConfigurer keyInput;
    private final TraitConfigPanel controls;

    public Ed(MenuSeparator p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      keyInput = new NamedHotKeyConfigurer(p.key);
      controls.add("Editor.MenuSeparator.if_hidden", keyInput);
    }


    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(descInput.getValueString()).append(keyInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(key);
  }
}
