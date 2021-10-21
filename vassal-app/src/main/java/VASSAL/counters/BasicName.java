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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Objects;

/**
 * This trait overrides the $BasicName$ property provided by the "Basic Piece", allowing a module designer
 * deeking to define pieces entirely by prototype to safely ignore & leave empty the Basic Piece.
 */
public class BasicName extends Decorator implements TranslatablePiece {
  public static final String ID = "basicName;"; // NON-NLS
  protected String name;
  protected String localizedName = null;

  public BasicName() {
    this(ID + ";", null);
  }

  public BasicName(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    name = st.nextToken();
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(name);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public Object getProperty(Object key) {
    if (BasicPiece.BASIC_NAME.equals(key)) {
      return name;
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (BasicPiece.BASIC_NAME.equals(key)) {
      if (localizedName != null) {

        localizedName = getI18nData().translate(name);
        return localizedName;
      }
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(
      new String[] { name },
      new String[] {
        Resources.getString("Editor.BasicName.trait_description"),
      });
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
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
    return buildDescription("Editor.BasicName.trait_description", name);
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.BasicName.trait_description");
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof BasicName)) return false;
    final BasicName c = (BasicName) o;
    return Objects.equals(name, c.name);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("BasicName.html"); // NON-NLS
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer descInput;
    private final TraitConfigPanel controls;

    public Ed(BasicName p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.name);
      descInput.setHintKey("Editor.BasicName.hint");
      controls.add("Editor.name_label", descInput);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
