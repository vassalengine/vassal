/*
 * $Id: DiceButton.java 2151 2007-06-01 13:31:10 +0000 (Fri, 01 Jun 2007) swampwallaby $
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
package VASSAL.build.module;

import java.applet.AudioClip;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.AudioClipConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button plays a sounf */
public class PlaySoundButton extends AbstractConfigurable {

  public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String SOUND_CLIP = "soundClip"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$

  protected LaunchButton launch;
  protected FormattedString reportFormat = new FormattedString(GameModule.getGameModule());
  protected String soundClip;
  
  public PlaySoundButton() {
    ActionListener rollAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        playSound();
      }
    };
    launch = new LaunchButton("Play Sound", TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, rollAction);
    setAttribute(NAME, "Play Sound");
    setAttribute(TOOLTIP, "Play Sound");
    launch.setAttribute(BUTTON_TEXT, "Play Sound");
  }

  public static String getConfigureTypeName() {
    return "Play Sound Button";
  }

  public String[] getAttributeNames() {
    String s[] = {NAME, BUTTON_TEXT, TOOLTIP, HOTKEY, ICON, SOUND_CLIP, REPORT_FORMAT};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  ",
                        "Button text:  ",
                        "Tooltip text:  ",
                        "Hot key:  ",
                        "Button icon:  ",
                        "Sound clip:  ",
                        "Report Format:  "};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, null); //$NON-NLS-1$
    }
  }

  public static class SoundConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new AudioClipConfigurer(key, name, GameModule.getGameModule().getArchiveWriter());
    }
  }
  
  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{});
    }
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       String.class,
                       String.class,
                       KeyStroke.class,
                       IconConfig.class,
                       SoundConfig.class,
                       ReportFormatConfig.class};
  }

  public void addTo(Buildable parent) {
    GameModule.getGameModule().getToolBar().add(getComponent());
  }

  /**
   * The component to be added to the control window toolbar
   */
  protected java.awt.Component getComponent() {
    return launch;
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (SOUND_CLIP.equals(key)) {
      if (o instanceof File) {
        o = ((File) o).getName();
      }
      soundClip = (String) o;
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) o);
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (SOUND_CLIP.equals(key)) {
      return soundClip;
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getToolBar().revalidate();
  }

  public HelpFile getHelpFile() {
    return null;
  }

  protected void playSound() {
    String clipName = new FormattedString(soundClip).getText(GameModule.getGameModule());
    try {
      AudioClip clip = GameModule.getGameModule().getDataArchive().getCachedAudioClip(clipName);
      if (clip != null) {
        clip.play();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
}
