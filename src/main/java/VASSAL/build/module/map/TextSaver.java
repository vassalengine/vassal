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
package VASSAL.build.module.map;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;

import javax.swing.JOptionPane;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;

public class TextSaver extends AbstractConfigurable {

  protected static final String HOTKEY = "hotkey";
  protected static final String BUTTON_TEXT = "buttonText";
  protected static final String TOOLTIP = "tooltip";
  protected static final String ICON_NAME = "icon";

  protected Map map;
  protected LaunchButton launch;

  public TextSaver() {
    ActionListener al = new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        apply();
      }
    };

    launch = new LaunchButton("Save Text", TOOLTIP, BUTTON_TEXT,
                              HOTKEY, ICON_NAME, al);
    launch.setAttribute(TOOLTIP, "Save map contents as plain text file");
  }


  @Override
  public String[] getAttributeNames() {
    return new String[] {
      BUTTON_TEXT,
      TOOLTIP,
      ICON_NAME,
      HOTKEY
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((TextSaver) c).launch.getAttributeValueString(ICON_NAME));
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    launch.setAttribute(key, value);
  }

  @Override
  public String getAttributeValueString(String key) {
    return launch.getAttributeValueString(key);
  }

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getToolBar().add(launch);
  }

  @Override
  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.getToolBar().remove(launch);
    map.getToolBar().revalidate();
  }

  public void apply() {

    switch (JOptionPane.showConfirmDialog
        (null, "Write contents as seen by opponents?", "", JOptionPane.YES_NO_OPTION)) {
    case JOptionPane.NO_OPTION:
      writeMapAsText();
      break;
    case JOptionPane.YES_OPTION:
      String myId = GameModule.getUserId();
      GameModule.setUserId("yendoR117");
      writeMapAsText();
      GameModule.setUserId(myId);
      break;
    }
  }

  protected void writeMapAsText() {
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showSaveDialog(map.getView()) != FileChooser.APPROVE_OPTION) return;

    final File file =  fc.getSelectedFile();
    try (Writer fw = new FileWriter(file);
         BufferedWriter bw = new BufferedWriter(fw);
         PrintWriter p = new PrintWriter(bw)) {
      for (GamePiece gp : map.getPieces()) {
        final String s = gp.getName();
        if (s.length() > 0) {
          p.println(map.locationName(gp.getPosition()) + ": " + s);
        }
      }
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, file);
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "TextCapture");
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.TextCapture.component_type"); //$NON-NLS-1$
  }

  /**
   * @return an array of Configurer objects representing
   * all possible classes of Buildable children of this Configurable object
   */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }
}
