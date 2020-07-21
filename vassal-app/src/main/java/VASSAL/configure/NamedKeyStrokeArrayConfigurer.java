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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * Configures an array of {@link NamedKeyStrokes}
 */
public class NamedKeyStrokeArrayConfigurer extends Configurer {
  private List<NamedHotKeyConfigurer> configs = new ArrayList<>();
  private Box controls;
  private JPanel panel;

  public NamedKeyStrokeArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public NamedKeyStrokeArrayConfigurer(String key, String name, NamedKeyStroke[] val) {
    super(key, name, val);
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel(new BorderLayout());
      controls = Box.createVerticalBox();
      final JScrollPane scroll = new JScrollPane(controls);
      Box b = Box.createHorizontalBox();
      controls.add(b);
      JLabel l = new JLabel(getName());
      b.add(l);
      JButton button = new JButton("Add");
      b.add(button);
      button.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          addKey(null);
        }
      });

      panel.add(scroll, BorderLayout.CENTER);

      NamedKeyStroke[] keyStrokes = (NamedKeyStroke[]) value;
      if (keyStrokes != null) {
        for (NamedKeyStroke keyStroke : keyStrokes) {
          addKey(keyStroke);
        }
      }
      addKey(null);
    }
    return panel;
  }

  private void addKey(NamedKeyStroke keyStroke) {
    NamedHotKeyConfigurer config = new NamedHotKeyConfigurer(null, null, keyStroke);
    configs.add(config);
    controls.add(config.getControls());
    if (configs.size() > 5) {
      panel.setPreferredSize(new Dimension(panel.getPreferredSize().width, 150));
    }
    else {
      panel.setPreferredSize(null);
    }
    Window w = SwingUtilities.getWindowAncestor(controls);
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
    ArrayList<NamedKeyStroke> l = new ArrayList<>();
    for (NamedHotKeyConfigurer hotKeyConfigurer : configs) {
      NamedKeyStroke value = hotKeyConfigurer.getValueNamedKeyStroke();
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
    ArrayList<NamedKeyStroke> l = new ArrayList<>();
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    while (st.hasMoreTokens()) {
      final String token = st.nextToken();
      if (token != "") {
        l.add(NamedHotKeyConfigurer.decode(token));
      }
    }
    return l.toArray(new NamedKeyStroke[0]);
  }

  public static String encode(NamedKeyStroke[] keys) {
    if (keys == null) {
      return null;
    }
    SequenceEncoder se = new SequenceEncoder(',');
    for (NamedKeyStroke key : keys) {
      if (!key.isNull()) {
        se.append(NamedHotKeyConfigurer.encode(key));
      }
    }
    return se.getValue() != null ? se.getValue() : "";
  }
}
