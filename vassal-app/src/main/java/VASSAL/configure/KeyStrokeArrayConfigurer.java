/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.configure;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

/**
 * Configures an array of keystrokes
 */
public class KeyStrokeArrayConfigurer extends Configurer {
  private final List<HotKeyConfigurer> configs = new ArrayList<>();
  private Box controls;
  private JPanel panel;

  public KeyStrokeArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public KeyStrokeArrayConfigurer(String key, String name, KeyStroke[] val) {
    super(key, name, val);
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel(new BorderLayout());
      controls = Box.createVerticalBox();
      final JScrollPane scroll = new JScrollPane(controls);
      final Box b = Box.createHorizontalBox();
      controls.add(b);
      final JLabel l = new JLabel(getName());
      b.add(l);
      final JButton button = new JButton(Resources.getString("Editor.KeyStrokeArrayConfigurer.add"));
      b.add(button);
      button.addActionListener(e -> addKey(null));

      panel.add(scroll, BorderLayout.CENTER);

      final KeyStroke[] keyStrokes = (KeyStroke[]) value;
      if (keyStrokes != null) {
        for (final KeyStroke keyStroke : keyStrokes) {
          addKey(keyStroke);
        }
      }
      addKey(null);
    }
    return panel;
  }

  private void addKey(KeyStroke keyStroke) {
    final HotKeyConfigurer config = new HotKeyConfigurer(null, null, keyStroke);
    configs.add(config);
    controls.add(config.getControls());
    if (configs.size() > 5) {
      panel.setPreferredSize(new Dimension(panel.getPreferredSize().width, 150));
    }
    else {
      panel.setPreferredSize(null);
    }
    final Window w = SwingUtilities.getWindowAncestor(controls);
    if (w != null) {
      w.pack();
    }
  }

  @Override
  public String getValueString() {
    return encode(getKeyStrokes());
  }

  @Override
  public void setValue(String s) {
    setValue(decode(s));
  }

  @Override
  public void setValue(Object o) {
    super.setValue(o);
    if (controls != null) {
      KeyStroke[] keyStrokes = (KeyStroke[]) o;
      if (keyStrokes == null) {
        keyStrokes = new KeyStroke[0];
      }

      for (int i = 0; i < keyStrokes.length; ++i) {
        if (i > configs.size()) {
          addKey(keyStrokes[i]);
        }
        else {
          configs.get(i).setValue(keyStrokes[i]);
        }
      }

      for (int i = keyStrokes.length; i < configs.size(); ++i) {
        configs.get(i).setValue(null);
      }
    }
  }

  public KeyStroke[] getKeyStrokes() {
    final List<KeyStroke> l = new ArrayList<>();
    for (final HotKeyConfigurer hotKeyConfigurer : configs) {
      final KeyStroke value = (KeyStroke) hotKeyConfigurer.getValue();
      if (value != null) {
        l.add(value);
      }
    }
    return l.toArray(new KeyStroke[0]);
  }

  public static KeyStroke[] decode(String s) {
    if (s == null) {
      return null;
    }
    final List<KeyStroke> l = new ArrayList<>();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    while (st.hasMoreTokens()) {
      l.add(HotKeyConfigurer.decode(st.nextToken()));
    }
    return l.toArray(new KeyStroke[0]);
  }

  public static String encode(KeyStroke[] keys) {
    if (keys == null) {
      return null;
    }
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final KeyStroke key : keys) {
      se.append(HotKeyConfigurer.encode(key));
    }
    return se.getValue() != null ? se.getValue() : "";
  }
}
