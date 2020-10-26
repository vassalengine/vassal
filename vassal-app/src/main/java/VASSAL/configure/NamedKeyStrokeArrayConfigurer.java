/*
 *
 * Copyright (c) 2004-2011 by Rodney Kinney, Brent Easton
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

import VASSAL.i18n.Resources;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.UIManager;

import net.miginfocom.swing.MigLayout;

/**
 * Configures an array of {link NamedKeyStrokes}
 */
public class NamedKeyStrokeArrayConfigurer extends Configurer {
  private final List<NamedHotKeyConfigurer> configs = new ArrayList<>();
  private JPanel controls;
  private JPanel panel;
  private JPanel scrollPanel;

  public NamedKeyStrokeArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public NamedKeyStrokeArrayConfigurer(String key, String name, NamedKeyStroke[] val) {
    super(key, name, val);
  }

  public NamedKeyStrokeArrayConfigurer(NamedKeyStroke[] val) {
    this(null, "", val);
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new ConfigurerPanel(getName(), "[grow,fill]rel[]", "[]rel[grow,fill]rel[]"); // NON-NLS

      controls = new JPanel(new MigLayout("ins 0," + ConfigurerLayout.STANDARD_GAPY, "[grow,fill]")); // NON-NLS

      final JScrollPane scroll = new JScrollPane(controls);
      scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

      scrollPanel = new JPanel(new MigLayout("ins 0", "[grow]", "[grow]")); // NON-NLS
      scrollPanel.add(scroll, "grow"); // NON-NLS
      panel.add(scrollPanel);

      final NamedKeyStroke[] keyStrokes = (NamedKeyStroke[]) value;
      if (keyStrokes == null || keyStrokes.length == 0) {
        addKey(null);
      }
      else {
        for (final NamedKeyStroke keyStroke : keyStrokes) {
          addKey(keyStroke);
        }
      }

      final JButton button = new JButton(Resources.getString("Editor.add"));
      button.addActionListener(e -> addKey(null));
      panel.add(button, "aligny top"); // NON-NLS
    }
    return panel;
  }

  private void addKey(NamedKeyStroke keyStroke) {
    final NamedHotKeyConfigurer config = new NamedHotKeyConfigurer(null, null, keyStroke);
    configs.add(config);
    controls.add(config.getControls(), "grow,wrap"); // NON-NLS
    if (configs.size() == 6) {
      final Dimension s = controls.getPreferredSize();
      final int t = (Integer) UIManager.get("ScrollBar.width");
      scrollPanel.setPreferredSize(new Dimension(s.width + t + 2, s.height));
    }
    repack(controls);
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
      NamedKeyStroke[] keyStrokes = (NamedKeyStroke[]) o;
      if (keyStrokes == null) {
        keyStrokes = new NamedKeyStroke[0];
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

  public NamedKeyStroke[] getKeyStrokes() {
    final ArrayList<NamedKeyStroke> l = new ArrayList<>();
    for (final NamedHotKeyConfigurer hotKeyConfigurer : configs) {
      final NamedKeyStroke value = hotKeyConfigurer.getValueNamedKeyStroke();
      if (value != null) {
        l.add(value);
      }
    }
    return l.toArray(new NamedKeyStroke[0]);
  }

  public static NamedKeyStroke[] decode(String s) {
    if (s == null) {
      return null;
    }
    final ArrayList<NamedKeyStroke> l = new ArrayList<>();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    while (st.hasMoreTokens()) {
      final String token = st.nextToken();
      if (!token.isEmpty()) {
        l.add(NamedHotKeyConfigurer.decode(token));
      }
    }
    return l.toArray(new NamedKeyStroke[0]);
  }

  public static String encode(NamedKeyStroke[] keys) {
    if (keys == null) {
      return null;
    }
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final NamedKeyStroke key : keys) {
      if (!key.isNull()) {
        se.append(NamedHotKeyConfigurer.encode(key));
      }
    }
    return se.getValue() != null ? se.getValue() : "";
  }
}
