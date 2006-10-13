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
import java.io.File;
import java.net.MalformedURLException;
import java.util.HashSet;
import java.util.Set;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.KeyStrokeArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * Macro
 * Execute a series of Keystrokes against this same piece
 *  - Triggered by own KeyCommand or list of keystrokes
 *  - Match against an optional Property Filter
 * */
public class TriggerAction extends Decorator implements EditablePiece {

  public static final String ID = "macro;";

  protected String name = "";
  protected String command = "";
  protected KeyStroke key = null;
  protected String propertyMatch = "";
  protected KeyStroke[] watchKeys = new KeyStroke[0];
  protected KeyStroke[] actionKeys = new KeyStroke[0];
  protected Set triggeredKeys; // Safeguard against infinite loops

  public TriggerAction() {
    this(ID, null);
  }

  public TriggerAction(String type, GamePiece inner) {
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
    if (command.length() > 0 && key != null) {
      return new KeyCommand[] {new KeyCommand(command, key, Decorator.getOutermost(this), matchesFilter())};
    }
    else {
      return new KeyCommand[0];
    }
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(name)
      .append(command)
      .append(key)
      .append(propertyMatch)
      .append(KeyStrokeArrayConfigurer.encode(watchKeys))
      .append(KeyStrokeArrayConfigurer.encode(actionKeys));

    return ID + se.getValue();
  }

  /**
   * Apply key commands to inner pieces first
   * @param stroke
   * @return
   */
  public Command keyEvent(KeyStroke stroke) {
    Command c = piece.keyEvent(stroke);
    return c == null ? myKeyEvent(stroke)
        : c.append(myKeyEvent(stroke));
  }

  public Command myKeyEvent(KeyStroke stroke) {
    if (triggeredKeys == null) {
      // Keep track of the keystrokes that we've already responded to within this event loop
      triggeredKeys = new HashSet();
      Runnable runnable = new Runnable() {
        public void run() {
          triggeredKeys = null;
        }
      };
      SwingUtilities.invokeLater(runnable);
    }

    /*
     * 1. Are we interested in this key command?
     *     Is it our command key?
     *     Does it match one of our watching keystrokes?
     */
    boolean seen = false;
    if (stroke.equals(key) && !triggeredKeys.contains(key)) {
      seen = true;
      triggeredKeys.add(key);
    }

    for (int i = 0; i < watchKeys.length && !seen; i++) {
      if (stroke.equals(watchKeys[i])
          && !triggeredKeys.contains(watchKeys[i])) {
        seen = true;
        triggeredKeys.add(watchKeys[i]);
      }
    }

    if (!seen) {
      return null;
    }

    // 2. Check the Property Filter if it exists.
    if (! matchesFilter()) {
      return null;
    }


    // 3. Issue the outgoing keystrokes
    GamePiece outer = Decorator.getOutermost(this);
    Command c = new NullCommand();
    for (int i = 0; i < actionKeys.length; i++) {
      c.append(outer.keyEvent(actionKeys[i]));
    }
    return c;
  }
  
  protected boolean matchesFilter() {
    GamePiece outer = Decorator.getOutermost(this);
    if (propertyMatch != null && propertyMatch.length() > 0) {
      PieceFilter filter = PropertiesPieceFilter.parse(new FormattedString(propertyMatch).getText(outer));
      if (!filter.accept(outer)) {
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
    String s = "Trigger Action";
    if (name.length() > 0) {
      s += " - " + name;
    }
    return s;
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "TriggerAction.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    name = st.nextToken("");
    command = st.nextToken("Trigger");
    key = st.nextKeyStroke('T');
    propertyMatch = st.nextToken("");

    String keys = st.nextToken("");
    if (keys.indexOf(',') > 0) {
      watchKeys = KeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      watchKeys = new KeyStroke[keys.length()];
      for (int i = 0; i < watchKeys.length; i++) {
        watchKeys[i] = KeyStroke.getKeyStroke(keys.charAt(i),InputEvent.CTRL_MASK);
      }
    }

    keys = st.nextToken("");
    if (keys.indexOf(',') > 0) {
      actionKeys = KeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      actionKeys = new KeyStroke[keys.length()];
      for (int i = 0; i < actionKeys.length; i++) {
        actionKeys[i] = KeyStroke.getKeyStroke(keys.charAt(i),InputEvent.CTRL_MASK);
      }
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {

    private StringConfigurer name;
    private StringConfigurer command;
    private HotKeyConfigurer key;
    private StringConfigurer propertyMatch;
    private KeyStrokeArrayConfigurer watchKeys;
    private KeyStrokeArrayConfigurer actionKeys;
    private JPanel box;

    public Ed(TriggerAction piece) {

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));

      name = new StringConfigurer(null, "Description:  ", piece.name);
      box.add(name.getControls());

      propertyMatch = new StringConfigurer(null, "Trigger when properties match:  ", piece.propertyMatch);
      box.add(propertyMatch.getControls());

      Box commandBox = Box.createHorizontalBox();
      command = new StringConfigurer(null, "Menu Command:  ", piece.command);
      commandBox.add(command.getControls());
      key = new HotKeyConfigurer(null, "  KeyStroke:  ", piece.key);
      commandBox.add(key.getControls());
      box.add(commandBox);

      watchKeys = new KeyStrokeArrayConfigurer(null, "Watch for these Keystrokes:  ", piece.watchKeys);
      box.add(watchKeys.getControls());

      actionKeys = new KeyStrokeArrayConfigurer(null, "Perform these Keystrokes:  ", piece.actionKeys);
      box.add(actionKeys.getControls());

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
      .append(command.getValueString())
      .append((KeyStroke) key.getValue())
      .append(propertyMatch.getValueString())
      .append(watchKeys.getValueString())
      .append(actionKeys.getValueString());
      return ID + se.getValue();
    }
  }
}
