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
package VASSAL.build.module;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.command.PlayAudioClipCommand;
import VASSAL.configure.AudioClipConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.ListConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.ThrowableUtils;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button displays a message, plays a sound and/or sends hotkeys */
public class DoActionButton extends AbstractConfigurable {

  public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String DO_REPORT = "doReport"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$
  public static final String DO_SOUND = "doSound"; //$NON-NLS-1$
  public static final String SOUND_CLIP = "soundClip"; //$NON-NLS-1$
  public static final String DO_HOTKEY = "doHotkey"; //$NON-NLS-1$
  public static final String HOTKEYS = "hotkeys"; //$NON-NLS-1$
 
  protected LaunchButton launch;
  protected boolean doReport = false;
  protected FormattedString reportFormat = new FormattedString(GameModule.getGameModule());
  protected boolean doSound = false;
  protected String soundClip = "";
  protected boolean doHotkey = false;
  protected List<KeyStroke> hotkeys = new ArrayList<KeyStroke>();
  
  // Detect looping Action Buttons
  protected static final int RECURSION_LIMIT = 50;
  protected static int recursionDepth = 0;
  
  public DoActionButton() {
    ActionListener rollAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          execute();
        }
        catch (InfiniteLoopException ex) {
          ErrorDialog.showDetails(
            ex,
            ThrowableUtils.getStackTrace(ex),
            "Error.infinite_loop",
            getConfigureTypeName(),
            getConfigureName()
          );
        }        
      }
    };
    launch = new LaunchButton(
      "Do Action", TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, rollAction);
    setAttribute(NAME, "Do Action");
    setAttribute(TOOLTIP, "Do Action");
    launch.setAttribute(BUTTON_TEXT, "Do Action");
  }

  public static String getConfigureTypeName() {
    return "Action Button";
  }

  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      BUTTON_TEXT,
      TOOLTIP,
      ICON,
      HOTKEY,
      DO_REPORT,
      REPORT_FORMAT,
      DO_SOUND,
      SOUND_CLIP,
      DO_HOTKEY,
      HOTKEYS
    };
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Description:  ",
      "Button text:  ",
      "Button Tooltip text:  ",
      "Button icon:  ",
      "Hot key:  ",
      "Display Message?",
      "Report Format:  ",
      "Play a sound?",
      "Sound Clip:  ",
      "Send Hotkeys?",
      "Hot Keys:  ",
    };
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
  
  public static class HotkeyConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new HotkeyListConfigurer(key, name, ((DoActionButton) c).hotkeys);
    }
  }
  
  public static class HotkeyListConfigurer extends ListConfigurer {

    public HotkeyListConfigurer(String key, String name, List<KeyStroke> list) {
      super(key, name, list);
    }

    protected Configurer buildChildConfigurer() {
      return new HotKeyConfigurer(null, "  HotKey:  ");
    }
    
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class,
      IconConfig.class,
      KeyStroke.class,
      Boolean.class,
      ReportFormatConfig.class,
      Boolean.class,
      SoundConfig.class,
      Boolean.class,
      HotkeyConfig.class
    };
  }

  public void addTo(Buildable parent) {
    GameModule.getGameModule().getToolBar().add(getComponent());
  }

  /**
   * The component to be added to the control window toolbar
   */
  protected Component getComponent() {
    return launch;
  }

  @SuppressWarnings("unchecked")
  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (DO_REPORT.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      doReport = ((Boolean) o).booleanValue();
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) o);
    }
    else if (DO_SOUND.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      doSound = ((Boolean) o).booleanValue();
    }
    if (SOUND_CLIP.equals(key)) {
      if (o instanceof File) {
        o = ((File) o).getName();
      }
      soundClip = (String) o;
    }
    else if (DO_HOTKEY.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      doHotkey = ((Boolean) o).booleanValue();
    }
    else if (HOTKEYS.equals(key)) {
      if (o instanceof String) {
        o = decodeHotkeys((String) o);
      }
      hotkeys = (List<KeyStroke>) o;
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DO_REPORT.equals(key)) {
      return String.valueOf(doReport);
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else if (DO_SOUND.equals(key)) {
      return String.valueOf(doSound);
    }
    else if (SOUND_CLIP.equals(key)) {
      return soundClip;
    }
    else if (DO_HOTKEY.equals(key)) {
      return String.valueOf(doHotkey);
    }
    else if (HOTKEYS.equals(key)) {
      return encodeHotkeys();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    if (REPORT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return doReport;
        }};
    }
    else if (SOUND_CLIP.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return doSound;
        }};
    }
    else if (HOTKEYS.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return doHotkey;
        }};
    }
    else {
      return null;
    }
  }
  
  protected String encodeHotkeys() {
    final SequenceEncoder se = new SequenceEncoder(',');
    for (KeyStroke key : hotkeys) {
      se.append(HotKeyConfigurer.encode(key));
    }

    final String val = se.getValue();
    return val == null ? "" : val;
  }

  protected List<KeyStroke> decodeHotkeys(String s) {
    List<KeyStroke> list = new ArrayList<KeyStroke>();
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    while (sd.hasMoreTokens()) {
      KeyStroke key = HotKeyConfigurer.decode(sd.nextToken());
      list.add(key);
    }
    return list;    
  }
  
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getToolBar().revalidate();
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MessageButton.htm"); //$NON-NLS-1$
  }

  protected void execute() throws InfiniteLoopException {
    try {
      if (++recursionDepth > RECURSION_LIMIT) {
        throw new InfiniteLoopException();
      }

      final Command c = new NullCommand();
      if (doReport) {
        final String report = "* " + reportFormat.getLocalizedText();

        c.append(new Chatter.DisplayText(
          GameModule.getGameModule().getChatter(), report)); 
      }

      if (doSound) {
        final String clipName =
          new FormattedString(soundClip).getText(GameModule.getGameModule());

        c.append(new PlayAudioClipCommand(clipName));
      }

      if (doHotkey) {
        for (KeyStroke key : hotkeys) {
          GameModule.getGameModule().fireKeyStroke(key);
        }
      }

      c.execute();
      GameModule.getGameModule().sendAndLog(c);
    }
    finally {
      --recursionDepth;
    }
  }
  
  private static class InfiniteLoopException extends Exception {
    private static final long serialVersionUID = 1L;
  }
}
