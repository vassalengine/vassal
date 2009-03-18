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
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.ListConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button displays a message, plays a sound and/or sends hotkeys */
public class DoActionButton extends AbstractConfigurable
                            implements RecursionLimiter.Loopable {

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
  protected FormattedString reportFormat =
    new FormattedString(GameModule.getGameModule());
  protected boolean doSound = false;
  protected String soundClip = ""; //$NON-NLS-1$
  protected boolean doHotkey = false;
  protected List<NamedKeyStroke> hotkeys = new ArrayList<NamedKeyStroke>();
  
  public DoActionButton() {
    ActionListener rollAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          execute();
        }
        catch (RecursionLimitException ex) {
          ErrorDialog.infiniteLoop(ex);
        }        
      }
    };

	  final String description = Resources.getString("Editor.DoAction.component_type"); //$NON-NLS-1$ 
    launch = new LaunchButton(
      description, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, rollAction);
    setAttribute(NAME, description);
    setAttribute(TOOLTIP, description);
    launch.setAttribute(BUTTON_TEXT, description);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DoAction.component_type"); //$NON-NLS-1$
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
    	Resources.getString(Resources.DESCRIPTION),
    	Resources.getString(Resources.BUTTON_TEXT),
    	Resources.getString(Resources.TOOLTIP_TEXT),
    	Resources.getString(Resources.BUTTON_ICON),
    	Resources.getString(Resources.HOTKEY_LABEL),
    	Resources.getString("Editor.DoAction.display_message"), //$NON-NLS-1$
    	Resources.getString("Editor.DoAction.report_format"), //$NON-NLS-1$
    	Resources.getString("Editor.DoAction.play_sound"), //$NON-NLS-1$
    	Resources.getString("Editor.DoAction.sound_clip"), //$NON-NLS-1$
    	Resources.getString("Editor.DoAction.send_hotkeys"), //$NON-NLS-1$
    	Resources.getString("Editor.DoAction.hotkeys") //$NON-NLS-1$
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, null); 
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
  
  public static class HotkeyConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new NamedHotkeyListConfigurer(key, name, ((DoActionButton) c).hotkeys);
    }
  }
  
  public static class NamedHotkeyListConfigurer extends ListConfigurer {

    public NamedHotkeyListConfigurer(String key, String name, List<NamedKeyStroke> list) {
      super(key, name, list);
    }

    protected Configurer buildChildConfigurer() {
      return new NamedHotKeyConfigurer(null, Resources.getString(Resources.HOTKEY_LABEL));
    }
    
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class,
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
      hotkeys = (List<NamedKeyStroke>) o;
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
    for (NamedKeyStroke key : hotkeys) {
      se.append(NamedHotKeyConfigurer.encode(key));
    }

    final String val = se.getValue();
    return val == null ? "" : val; //$NON-NLS-1$
  }

  protected List<NamedKeyStroke> decodeHotkeys(String s) {
    List<NamedKeyStroke> list = new ArrayList<NamedKeyStroke>();
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    while (sd.hasMoreTokens()) {
      NamedKeyStroke key = NamedHotKeyConfigurer.decode(sd.nextToken());
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

  protected void execute() throws RecursionLimitException {
    try {
      RecursionLimiter.startExecution(this);

      final Command c = new NullCommand();
      if (doReport) {
        final String report = "* " + reportFormat.getLocalizedText(); //$NON-NLS-1$

        c.append(new Chatter.DisplayText(
          GameModule.getGameModule().getChatter(), report)); 
      }

      if (doSound) {
        final String clipName =
          new FormattedString(soundClip).getText(GameModule.getGameModule());

        c.append(new PlayAudioClipCommand(clipName));
      }

      if (doHotkey) {
        for (NamedKeyStroke key : hotkeys) {
          GameModule.getGameModule().fireKeyStroke(key);
        }
      }

      c.execute();
      GameModule.getGameModule().sendAndLog(c);
    }
    finally {
      RecursionLimiter.endExecution();
    }
  }

  // Implement Loopable
  public String getComponentTypeName () {
    return getConfigureTypeName();
  }
  
  public String getComponentName() {
    return getConfigureName();
  }
}
