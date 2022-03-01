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
package VASSAL.configure;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.AudioClip;
import VASSAL.tools.AudioSystemClip;
import VASSAL.tools.Mp3AudioClip;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.URLUtils;
import VASSAL.tools.filechooser.AudioFileFilter;
import VASSAL.tools.filechooser.FileChooser;

import javax.swing.JButton;
import javax.swing.JTextField;
import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Configurer for specifying a Clip. This class is intended to allow
 * players to override a default sound with their own sound file on their
 * local file system.
 */
public class SoundConfigurer extends Configurer {
  public static final String DEFAULT = "default"; //NON-NLS
  private final String defaultResource;
  private String clipName;
  private ConfigurerPanel controls;
  private JTextField textField;
  private final AudioClipFactory clipFactory;

  //FIXME this needs some i18n scheme and preferably the display version should be [disabled] while leaving file version alone.
  private static final String NO_VALUE = "<disabled>";

  public SoundConfigurer(String key, String name, String defaultResource) {
    super(key, name);
    this.defaultResource = defaultResource;
    clipFactory = createAudioClipFactory();
    setValue(DEFAULT);
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new ConfigurerPanel(getName(), "[]rel[]rel[]rel[grow,fill]", "[]rel[]rel[]rel[]rel[grow,fill]"); // NON-NLS

      JButton b = new JButton(Resources.getString("Editor.SoundConfigurer.play"));
      b.addActionListener(e -> play());
      controls.add(b);
      b = new JButton(Resources.getString("Editor.SoundConfigurer.default"));
      b.addActionListener(e -> setValue(DEFAULT));
      controls.add(b);
      b = new JButton(Resources.getString("Editor.SoundConfigurer.select"));
      b.addActionListener(e -> chooseClip());
      controls.add(b);
      textField = new JTextField(20);
      textField.setEditable(false);
      textField.setText(DEFAULT.equals(clipName) ? defaultResource : clipName);
      controls.add(textField, "growx"); // NON-NLS
    }
    return controls;
  }

  @Override
  public String getValueString() {
    String s = NO_VALUE;
    if (clipName != null) {
      s = clipName;
    }
    return s;
  }

  @Override
  public void setValue(String s) {
    if (clipFactory == null) {
      return;
    }
    URL url = null;
    if (DEFAULT.equals(s)) {
      url = getClass().getResource("/images/" + defaultResource); //NON-NLS
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

  @FunctionalInterface
  protected interface AudioClipFactory {
    AudioClip getAudioClip(URL url) throws IOException;
  }

  protected AudioClipFactory createAudioClipFactory() {
    return url -> {
      if (url.toString().toLowerCase().endsWith(".mp3")) { //NON-NLS
        return new Mp3AudioClip(url);
      }
      else {
        try (InputStream in = url.openStream()) {
          return new AudioSystemClip(in);
        }
        catch (IOException e) {
          return null;
        }
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
    final FileChooser fc = GameModule.getGameModule().getEditorSoundChooser();
    fc.setFileFilter(new AudioFileFilter());

    if (fc.showOpenDialog(getControls()) != FileChooser.APPROVE_OPTION) {
      setValue(NO_VALUE);
    }
    else {
      final File f = fc.getSelectedFile();
      setValue(f.getName());
    }
  }

  @Override
  public void setLabelVisibile(boolean visible) {
    controls.setLabelVisibility(visible);
  }
}
