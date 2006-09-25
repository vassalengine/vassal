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
package VASSAL.build.module.map;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.MalformedURLException;

import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

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
import VASSAL.tools.LaunchButton;

public class TextSaver extends AbstractConfigurable {
  
  protected static final String HOTKEY = "hotkey";
  protected static final String BUTTON_TEXT = "buttonText";
  protected static final String TOOLTIP = "tooltip";
  protected static final String ICON_NAME = "icon";
  
  protected Map map;
  protected LaunchButton launch;

  public TextSaver() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        apply();
      }
    };
    launch = new LaunchButton("Save Text", TOOLTIP, BUTTON_TEXT, HOTKEY, ICON_NAME, al);
    launch.setAttribute(TOOLTIP, "Save map contents as plain text file");
  }
  

  public String[] getAttributeNames() {
    return new String[] {TOOLTIP, BUTTON_TEXT, ICON_NAME, HOTKEY};
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Tooltip Text", "Button Text", "Toolbar button icon", "Hotkey"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, IconConfig.class, KeyStroke.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((TextSaver) c).launch.getAttributeValueString(ICON_NAME));
    }
  }
  
  public void setAttribute(String key, Object value) {
    launch.setAttribute(key, value);
  }

  public String getAttributeValueString(String key) {
    return launch.getAttributeValueString(key);
  }
  
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getToolBar().add(launch);
  }

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
    javax.swing.JFileChooser fc
        = GameModule.getGameModule().getFileChooser();
    if (fc.showSaveDialog(null) == javax.swing.JFileChooser.CANCEL_OPTION) {
      return;
    }
    try {
      PrintWriter p = new PrintWriter
          (new FileOutputStream(fc.getSelectedFile().getPath()));
      GamePiece stack[] = map.getPieces();
      for (int i = 0; i < stack.length; ++i) {
        String s = stack[i].getName();
        if (s.length() > 0) {
          p.println(map.locationName(stack[i].getPosition()) + ": " + s);
        }
      }
      p.close();
    }
    catch (IOException e) {
      JOptionPane.showMessageDialog(null, e.getMessage());
    }
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#TextCapture");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static String getConfigureTypeName() {
    return "Text Capture Tool";
  }

  /** @return an array of Configurer objects representing
   * all possible classes of Buildable children of this Configurable object
   */
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

}
