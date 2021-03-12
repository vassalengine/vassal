/*
 *
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
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
import VASSAL.configure.NamedKeyStrokeArrayConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.KeyStroke;

/**
 * RestrictCommands
 * Restrict the availability of Key Commands, depending on a
 * Property Match String.
 *  - Variable list of Key Commands to restrict
 *  - Disable or Invisible
 * */
public class RestrictCommands extends Decorator implements EditablePiece {

  public static final String ID = "hideCmd;"; // NON-NLS
  protected static final String HIDE = "Hide"; // NON-NLS
  protected static final String DISABLE = "Disable"; // NON-NLS
  protected static final String[] restrictionKeys = { "Editor.RestrictCommands.hide", "Editor.RestrictCommands.disable" };

  protected String name = "";
  protected PropertyExpression propertyMatch = new PropertyExpression();
  protected String action = HIDE;
  protected NamedKeyStroke[] watchKeys = new NamedKeyStroke[0];

  public RestrictCommands() {
    this(ID, null);
  }

  public RestrictCommands(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
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
    se.append(name)
      .append(action)
      .append(propertyMatch.getExpression())
      .append(NamedKeyStrokeArrayConfigurer.encode(watchKeys));

    return ID + se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  /*
   * Cancel execution of watched KeyStrokes
   */
  @Override
  public Command keyEvent(KeyStroke stroke) {
    if (!matchesFilter()) {
      return super.keyEvent(stroke);
    }
    else {
      for (final NamedKeyStroke watchKey : watchKeys) {
        if (watchKey.equals(stroke)) {
          return null;
        }
      }
    }
    return super.keyEvent(stroke);
  }

  @Override
  protected KeyCommand[] getKeyCommands() {
    KeyCommand[] commands = super.getKeyCommands();
    final ArrayList<KeyCommand> newCommands =
      new ArrayList<>(commands.length);
    if (matchesFilter()) {
      for (final KeyCommand command : commands) {
        boolean matches = false;
        for (int j = 0; j < watchKeys.length && !matches; j++) {
          matches = (watchKeys[j].equals(command.getKeyStroke()));
        }
        if (matches) {
          if (action.equals(DISABLE)) {
            final KeyCommand newCommand = new KeyCommand(command);
            newCommand.setEnabled(false);
            newCommands.add(newCommand);
          }
        }
        else {
          newCommands.add(command);
        }
      }
      commands  = newCommands.toArray(new KeyCommand[0]);
    }
    return commands;
  }

  protected boolean matchesFilter() {
    final GamePiece outer = Decorator.getOutermost(this);
    if (!propertyMatch.isNull()) {
      return propertyMatch.accept(outer);
    }
    return true;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.RestrictCommands.trait_description", name);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("RestrictCommands.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    name = st.nextToken("");
    action = st.nextToken(HIDE);
    propertyMatch.setExpression(st.nextToken(""));

    final String keys = st.nextToken("");
    if (keys.indexOf(',') > 0) {
      watchKeys = NamedKeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      watchKeys = new NamedKeyStroke[keys.length()];
      for (int i = 0; i < watchKeys.length; i++) {
        watchKeys[i] = NamedKeyStroke.of(keys.charAt(i), InputEvent.CTRL_DOWN_MASK);
      }
    }
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }


  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof RestrictCommands)) return false;
    final RestrictCommands c = (RestrictCommands) o;
    if (! Objects.equals(name, c.name)) return false;
    if (! Objects.equals(action, c.action)) return false;
    if (! Objects.equals(propertyMatch, c.propertyMatch)) return false;
    return Objects.equals(NamedKeyStrokeArrayConfigurer.encode(watchKeys), NamedKeyStrokeArrayConfigurer.encode(c.watchKeys));
  }

  public static class Ed implements PieceEditor {

    protected StringConfigurer name;
    protected PropertyExpressionConfigurer propertyMatch;
    protected NamedKeyStrokeArrayConfigurer watchKeys;
    protected TranslatingStringEnumConfigurer actionOption;
    protected TraitConfigPanel box;

    public Ed(RestrictCommands piece) {

      box = new TraitConfigPanel();

      name = new StringConfigurer(piece.name);
      name.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", name);

      actionOption = new TranslatingStringEnumConfigurer(new String[] { HIDE, DISABLE }, restrictionKeys, piece.action);
      box.add("Editor.RestrictCommands.restriction", actionOption);

      propertyMatch = new PropertyExpressionConfigurer(piece.propertyMatch, Decorator.getOutermost(piece));
      box.add("Editor.RestrictCommands.restrict_when_properties_match", propertyMatch);

      watchKeys = new NamedKeyStrokeArrayConfigurer(piece.watchKeys);
      box.add("Editor.RestrictCommands.restrict_these_key_commands", watchKeys);

    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(name.getValueString())
        .append(actionOption.getValueString())
        .append(propertyMatch.getValueString())
        .append(watchKeys.getValueString());
      return ID + se.getValue();
    }
  }

  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return List.of(propertyMatch.getExpression());
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    final List<NamedKeyStroke> l = new ArrayList<>();
    Collections.addAll(l, watchKeys);
    return l;
  }
}
