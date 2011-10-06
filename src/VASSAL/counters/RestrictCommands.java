/*
 * $Id$
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.NamedKeyStrokeArrayConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * RestrictCommands
 * Restrict the availability of Key Commands, depending on a
 * Property Match String.
 *  - Variable list of Key Commands to restrict
 *  - Disable or Invisible
 * */
public class RestrictCommands extends Decorator implements EditablePiece {

  public static final String ID = "hideCmd;";
  protected static final String HIDE = "Hide";
  protected static final String DISABLE = "Disable";

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

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(name)
      .append(action)
      .append(propertyMatch.getExpression())
      .append(NamedKeyStrokeArrayConfigurer.encode(watchKeys));

    return ID + se.getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  /*
   * Cancel execution of watched KeyStrokes
   */
  public Command keyEvent(KeyStroke stroke) {
    if (!matchesFilter()) {
      return super.keyEvent(stroke);
    }
    else {
      for (int j = 0; j < watchKeys.length ; j++) {
        if (watchKeys[j].equals(stroke)) {
          return null;
        }
      }
    }
    return super.keyEvent(stroke);
  }

  protected KeyCommand[] getKeyCommands() {
    KeyCommand[] commands = super.getKeyCommands();
    ArrayList<KeyCommand> newCommands =
      new ArrayList<KeyCommand>(commands.length);
    if (matchesFilter()) {
      for (int i = 0; i < commands.length; i++) {
        boolean matches = false;
        for (int j = 0; j < watchKeys.length && ! matches; j++) {
          matches = (watchKeys[j].equals(commands[i].getKeyStroke()));
        }
        if (matches) {
          if (action.equals(DISABLE)) {
            KeyCommand newCommand = new KeyCommand(commands[i]);
            newCommand.setEnabled(false);
            newCommands.add(newCommand);
          }
        }
        else {
          newCommands.add(commands[i]);
        }
      }
      commands  = newCommands.toArray(new KeyCommand[newCommands.size()]);
    }
    return commands;
  }

  protected boolean matchesFilter() {
    GamePiece outer = Decorator.getOutermost(this);
    if (!propertyMatch.isNull()) {
      if (!propertyMatch.accept(outer)) {
        return false;
      }
    }
    return true;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    String s = "Restrict Commands";
    if (name.length() > 0) {
      s += " - " + name;
    }
    return s;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("RestrictCommands.htm");
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    name = st.nextToken("");
    action = st.nextToken(HIDE);
    propertyMatch.setExpression(st.nextToken(""));

    String keys = st.nextToken("");
    if (keys.indexOf(',') > 0) {
      watchKeys = NamedKeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      watchKeys = new NamedKeyStroke[keys.length()];
      for (int i = 0; i < watchKeys.length; i++) {
        watchKeys[i] = new NamedKeyStroke(keys.charAt(i),InputEvent.CTRL_MASK);
      }
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {

    protected StringConfigurer name;
    protected PropertyExpressionConfigurer propertyMatch;
    protected NamedKeyStrokeArrayConfigurer watchKeys;
    protected JComboBox actionOption;
    protected JPanel box;

    public Ed(RestrictCommands piece) {

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));

      name = new StringConfigurer(null, "Description:  ", piece.name);
      box.add(name.getControls());

      actionOption = new JComboBox();
      actionOption.addItem(HIDE);
      actionOption.addItem(DISABLE);
      actionOption.setSelectedIndex((piece.action.equals(HIDE)) ? 0 : 1);
      Box b = Box.createHorizontalBox();
      b.add(new JLabel("Restriction:  "));
      b.add(actionOption);
      box.add(b);

      propertyMatch = new PropertyExpressionConfigurer(null, "Restrict when properties match:  ", piece.propertyMatch, Decorator.getOutermost(piece));
      box.add(propertyMatch.getControls());

      watchKeys = new NamedKeyStrokeArrayConfigurer(null, "Restrict these Key Commands  ", piece.watchKeys);
      box.add(watchKeys.getControls());

    }


    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(name.getValueString())
      .append((actionOption.getSelectedIndex()==0) ? HIDE : DISABLE)
      .append(propertyMatch.getValueString())
      .append(watchKeys.getValueString());
      return ID + se.getValue();
    }
  }
}
