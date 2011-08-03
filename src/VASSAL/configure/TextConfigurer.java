/*
 * $Id$
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
package VASSAL.configure;

import java.util.StringTokenizer;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;

import org.apache.commons.lang.StringUtils;

import VASSAL.build.AutoConfigurable;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;

/**
 * A Configurer that allows multi-line string input via a JTextArea
 */
public class TextConfigurer extends Configurer implements ConfigurerFactory {
  private JTextArea textArea;
  private JPanel p;
  private boolean wordWrap;

  public TextConfigurer() {
    this(null, null, null);
  }

  public TextConfigurer(String key, String name) {
    this(key, name, "");
  }

  public TextConfigurer(String key, String name, String val) {
    super(key, name, val);
  }

  public TextConfigurer(String key, String name, String val, boolean wrap) {
    this(key, name, val);
    setWordWrap(wrap);
  }

  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
    this.key = key;
    this.name = name;
    return this;
  }

  public String getValueString() {
    return escapeNewlines((String) getValue());
  }

  public void setWordWrap(boolean b) {
    wordWrap = b;
  }

  /**
   * Encodes a string by replacing newlines with '|' characters
   *
   * @param s
   * @return
   */
  public static String escapeNewlines(String s) {
    SequenceEncoder se = new SequenceEncoder('|');
    StringTokenizer st = new StringTokenizer(s, "\n\r", true);
    boolean wasNewLine = true;
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      switch (token.charAt(0)) {
      case '\n':
        if (wasNewLine) {
          se.append("");
        }
        wasNewLine = true;
        break;
      case '\r':
        break;
      default:
        se.append(token);
        wasNewLine = false;
      }
    }
    return se.getValue() == null ? "" : se.getValue();
  }

  public void setValue(String s) {
    String text = restoreNewlines(s);
    setValue((Object) text);
  }

  public void setValue(Object o) {
    super.setValue(o);
    if (!noUpdate && textArea != null) {
      textArea.setText((String) o);
    }
  }

  /**
   * Restores a string by replacing '|' with newlines
   *
   * @param s
   * @return
   */
  public static String restoreNewlines(String s) {
    return StringUtils.join(new SequenceEncoder.Decoder(s, '|'), '\n');
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      textArea = new JTextArea(6, 20);
      if (wordWrap) {
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
      }
      textArea.addKeyListener(new java.awt.event.KeyAdapter() {
        public void keyReleased(java.awt.event.KeyEvent evt) {
          queueForUpdate(textArea.getText());
        }
      });
      textArea.setText((String) getValue());
      JScrollPane scroll = new ScrollPane(textArea);
      if (name != null) {
        scroll.setBorder(new TitledBorder(name));
      }
      p.add(scroll);
    }
    return p;
  }

  private long lastUpdate = System.currentTimeMillis();
  private String updatedValue;
  private boolean updateQueued=false;
  private long updateFrequencey = 1000L;

  private void queueForUpdate(String s) {
    updatedValue = s;
    if (System.currentTimeMillis() > lastUpdate + updateFrequencey) {
      executeUpdate();
    }
    else if (!updateQueued) {
      updateQueued = true;
      Runnable delayedUpdate = new Runnable() {
        public void run() {
          try {
            Thread.sleep(updateFrequencey);
          }
          catch (InterruptedException e) {
          }

          SwingUtilities.invokeLater(new Runnable() {
            public void run() {
              executeUpdate();
            }
          });
        }
      };
      new Thread(delayedUpdate).start();
    }
  }

  private void executeUpdate() {
    noUpdate = true;
    setValue((Object) updatedValue);
    lastUpdate = System.currentTimeMillis();
    updateQueued = false;
    noUpdate = false;
  }
}
