/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * A generic Decorator that retains in its state the value of a property.
 * That is, if {@link #setProperty(Object,Object)} is invoked with a key
 * that is one of {@link #getKeys()}, the <code>String</code> value of that
 * property will be reflected in the {@link #myGetState(String)} method.
 */
public class Marker extends Decorator implements EditablePiece {
  public static final String ID = "mark;";

  protected String[] keys;
  protected String[] values;

  public Marker() {
    this(ID, null);
  }

  public Marker(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public String[] getKeys() {
    return keys;
  }

  @Override
  public void mySetType(String s) {
    s = s.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    ArrayList<String> l = new ArrayList<>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    keys = l.toArray(new String[0]);
    values = new String[keys.length];
    Arrays.fill(values, "");
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
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public Object getProperty(Object key) {
    for (int i = 0; i < keys.length; ++i) {
      if (keys[i].equals(key)) {
        return values[i];
      }
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    for (int i = 0; i < keys.length; ++i) {
      if (keys[i].equals(key)) {
        return values[i];
      }
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public void setProperty(Object key, Object value) {
    for (int i = 0; i < keys.length; ++i) {
      if (keys[i].equals(key)) {
        values[i] = (String) value;
        return;
      }
    }
    super.setProperty(key, value);
  }

  @Override
  public String myGetState() {
    SequenceEncoder se = new SequenceEncoder(',');
    for (String value : values) {
      se.append(value);
    }
    return se.getValue();
  }

  @Override
  public void mySetState(String state) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(state, ',');
    int i = 0;
    while (st.hasMoreTokens() && i < values.length) {
      values[i++] = st.nextToken();
    }
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(',');
    for (String key : keys) {
      se.append(key);
    }
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public String getDescription() {
    if (keys != null
        && keys.length > 0 && keys[0].length() > 0
        && values.length > 0 && values[0].length() > 0) {
      return "Marker - " + keys[0] + " = " + values[0];
    }
    else
      return "Marker";
  }

  @Override
  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PropertyMarker.htm");
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    for (String key : keys) {
      l.add(key);
    }
    return l;
  }

  private static class Ed implements PieceEditor {
    private StringConfigurer propName;
    private StringConfigurer propValue;
    private JPanel panel;

    private Ed(Marker m) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      SequenceEncoder seKeys = new SequenceEncoder(',');
      for (int i = 0; i < m.keys.length; ++i) {
        seKeys.append(m.keys[i]);
      }

      SequenceEncoder seValues = new SequenceEncoder(',');
      for (int i = 0; i < m.values.length; ++i) {
        seValues.append(m.values[i]);
      }

      propName = new StringConfigurer(null, "Property name:  ", m.keys.length == 0 ? "" : seKeys.getValue());
      propValue = new StringConfigurer(null, "Property value:  ", m.values.length == 0 ? "" : seValues.getValue());
      panel.add(propName.getControls());
      panel.add(propValue.getControls());
    }

    @Override
    public Component getControls() {
      return panel;
    }

    @Override
    public String getState() {
      return propValue.getValueString();
    }

    @Override
    public String getType() {
      return Marker.ID + propName.getValueString();
    }
  }
}
