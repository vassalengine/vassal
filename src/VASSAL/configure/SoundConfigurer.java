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

import java.applet.Applet;
import java.applet.AudioClip;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import VASSAL.build.GameModule;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.URLUtils;
import VASSAL.tools.filechooser.AudioFileFilter;
import VASSAL.tools.filechooser.FileChooser;

/**
 * Configurer for specifying an AudioClip. This class is intended to allow
 * players to override a default sound with their own sound file on their
 * local file system.
 */
public class SoundConfigurer extends Configurer {
  public static final String DEFAULT = "default";
  private String defaultResource;
  private String clipName;
  private JPanel controls;
  private JTextField textField;
  private AudioClipFactory clipFactory;
  private static final String NO_VALUE = "<disabled>";

  public SoundConfigurer(String key, String name, String defaultResource) {
    super(key, name);
    this.defaultResource = defaultResource;
    clipFactory = createAudioClipFactory();
    setValue(DEFAULT);
  }

  public Component getControls() {
    if (controls == null) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.X_AXIS));
      controls.add(new JLabel(name));
      JButton b = new JButton("Play");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          play();
        }
      });
      controls.add(b);
      b = new JButton("Default");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setValue(DEFAULT);
        }
      });
      controls.add(b);
      b = new JButton("Select");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          chooseClip();
        }
      });
      controls.add(b);
      textField = new JTextField();
      textField.setMaximumSize(
        new Dimension(textField.getMaximumSize().width,
                      textField.getPreferredSize().height));
      textField.setEditable(false);
      textField.setText(DEFAULT.equals(clipName) ?
                        defaultResource : clipName);
      controls.add(textField);
    }
    return controls;
  }

  public String getValueString() {
    String s = NO_VALUE;
    if (clipName != null) {
      s = clipName;
    }
    return s;
  }

  public void setValue(String s) {
    if (clipFactory == null) {
      return;
    }
    URL url = null;
    if (DEFAULT.equals(s)) {
      url = getClass().getResource("/images/" + defaultResource);
      clipName = s;
    }
    else if (NO_VALUE.equals(s)) {
      clipName = s;
    }
    else if (s != null) {
      try {
        url = URLUtils.toURL(new File(s));
        clipName = s;
      }
      catch (MalformedURLException e) {
        ReadErrorDialog.error(e, s);
        clipName = null;
      }
    }
    if (textField != null) {
      textField.setText(DEFAULT.equals(clipName) ? defaultResource : clipName);
    }
    if (url != null) {
      try {
        setValue(clipFactory.getAudioClip(url));
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, url.toString());
      }
    }
    else {
      if (textField != null) {
        textField.setText(null);
      }
      setValue((Object) null);
    }
  }

  protected interface AudioClipFactory {
    public AudioClip getAudioClip(URL url) throws IOException;
  }

  protected AudioClipFactory createAudioClipFactory() {
    return new AudioClipFactory() {
      public AudioClip getAudioClip(URL url) {
        return Applet.newAudioClip(url);
      }
    };
  }

  public void play() {
    final AudioClip clip = (AudioClip) getValue();
    if (clip != null) {
      clip.play();
    }
  }

  public void chooseClip() {
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.setFileFilter(new AudioFileFilter());

    if (fc.showOpenDialog(getControls()) != FileChooser.APPROVE_OPTION) {
      setValue(NO_VALUE);
    }
    else {
      File f = fc.getSelectedFile();
      setValue(f.getName());
    }
  }
}
