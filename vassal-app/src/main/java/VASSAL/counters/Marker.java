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

import VASSAL.i18n.Resources;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * A generic Decorator that retains in its state the value of a property.
 * That is, if {@link #setProperty(Object,Object)} is invoked with a key
 * that is one of {@link #getKeys()}, the <code>String</code> value of that
 * property will be reflected in the #myGetState() method.
 */
public class Marker extends Decorator implements EditablePiece {
  public static final String ID = "mark;"; // NON-NLS

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
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    final ArrayList<String> l = new ArrayList<>();
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
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final String value : values) {
      se.append(value);
    }
    return se.getValue();
  }

  @Override
  public void mySetState(String state) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(state, ',');
    int i = 0;
    while (st.hasMoreTokens() && i < values.length) {
      values[i++] = st.nextToken();
    }
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final String key : keys) {
      se.append(key);
    }
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public String getDescription() {
    String result = Resources.getString("Editor.Marker.trait_description");

    if (keys != null && keys.length > 0 && keys[0].length() > 0) {
      result += " - " + keys[0];
      if (values.length > 0 && values[0].length() > 0) {
        result += " = " + values[0];
      }

      if (keys.length > 1) {
        result += " " + Resources.getString("Editor.Marker.more_keys", keys.length - 1);
      }
    }

    return result;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PropertyMarker.html"); // NON-NLS
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
    final ArrayList<String> l = new ArrayList<>();
    Collections.addAll(l, keys);
    return l;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Marker)) return false;
    final Marker c = (Marker) o;
    if (!Arrays.equals(keys, c.keys)) return false;
    return Arrays.equals(values, c.values);
  }

  private static class Ed implements PieceEditor {
    private final StringConfigurer propName;
    private final StringConfigurer propValue;
    private final TraitConfigPanel panel;

    private Ed(Marker m) {
      panel = new TraitConfigPanel();

      final SequenceEncoder seKeys = new SequenceEncoder(',');
      for (int i = 0; i < m.keys.length; ++i) {
        seKeys.append(m.keys[i]);
      }

      final SequenceEncoder seValues = new SequenceEncoder(',');
      for (int i = 0; i < m.values.length; ++i) {
        seValues.append(m.values[i]);
      }

      propName = new StringConfigurer(m.keys.length == 0 ? "" : seKeys.getValue());
      panel.add("Editor.Marker.property_name", propName);

      propValue = new StringConfigurer(m.values.length == 0 ? "" : seValues.getValue());
      panel.add("Editor.Marker.property_value", propValue);
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


  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return Arrays.asList(values);
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(values);
  }
}
