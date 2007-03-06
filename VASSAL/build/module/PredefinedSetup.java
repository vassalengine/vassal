/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.VisibilityCondition;

/**
 * Defines a saved game that is accessible from the File menu.
 * The game will be loaded in place of a normal New Game
 */
public class PredefinedSetup extends AbstractConfigurable {
  public static final String NAME = "name";
  public static final String FILE = "file";
  public static final String USE_FILE = "useFile";
  public static final String IS_MENU = "isMenu";
  protected boolean isMenu;
  protected boolean useFile=true;
  protected String fileName;
  protected JMenuItem menuItem;
  protected JMenu menu;
  protected JMenuItem originalItem;
  protected VisibilityCondition showFile;
  protected VisibilityCondition showUseFile;

  public PredefinedSetup() {
    menuItem = new JMenuItem();
    menu = new JMenu();
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
    showFile = new VisibilityCondition() {
      public boolean shouldBeVisible() {
        return !isMenu && useFile;
      }
    };
    showUseFile = new VisibilityCondition() {
      public boolean shouldBeVisible() {
        return !isMenu;
      }
    };
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  ", "Contains sub-menus?", "Use pre-defined file?", "Saved Game:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, Boolean.class, Boolean.class, File.class};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, IS_MENU, USE_FILE, FILE};
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (FILE.equals(key)) {
      return fileName;
    }
    else if (USE_FILE.equals(key)) {
      return String.valueOf(useFile);
    }
    else if (IS_MENU.equals(key)) {
      return String.valueOf(isMenu);
    }
    else {
      return null;
    }
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      menuItem.setText((String) value);
      menu.setText((String) value);
    }
    else if (USE_FILE.equals(key)) {
      useFile = "true".equals(value) || Boolean.TRUE.equals(value);
    }
    else if (FILE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      fileName = (String) value;
    }
    else if (IS_MENU.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      setMenu(((Boolean) value).booleanValue());
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (FILE.equals(name)) {
      return showFile;
    }
    else if (USE_FILE.equals(name)) {
      return showUseFile;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public void launch() {
    if (useFile
      && fileName != null) {
      try {
        int index = fileName.indexOf(".");
        String prefix = index > 3 ? fileName.substring(0, index) : "VSL";
        String suffix = index >= 0 ? fileName.substring(index) : ".sav";
        File tmp = File.createTempFile(prefix, suffix);
        BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(tmp));
        BufferedInputStream in = new BufferedInputStream(GameModule.getGameModule().getDataArchive().getFileStream(fileName));
        int len = 0;
        byte[] b = new byte[in.available()];
        while ((len = in.read(b)) > 0) {
          out.write(b, 0, len);
        }
        in.close();
        out.close();
        File renamed = new File(tmp.getParent(), fileName);
        if (tmp.renameTo(renamed)) {
          tmp = renamed;
        }
        GameModule.getGameModule().getGameState().loadGame(tmp);
      }
      catch (IOException e1) {
        e1.printStackTrace();
        String msg = "Unable to start game";
        if (e1.getMessage() != null) {
          msg += ":  " + e1.getMessage();
        }
        GameModule.getGameModule().warn(msg);
      }
    }
    else {
      GameModule.getGameModule().getGameState().setup(false);
      GameModule.getGameModule().getGameState().setup(true);
    }
  }

  private JMenuItem getMenuInUse() {
    return isMenu ? menu : menuItem;
  }

  private void setMenu(boolean isMenu) {
    if (isMenu != this.isMenu
        && getMenuInUse().getParent() != null) {
      JMenuItem inUse = getMenuInUse();
      int index = -1;
      for (int i = 0,n = inUse.getParent().getComponentCount(); i < n; ++i) {
        if (inUse.getParent().getComponent(i) == inUse) {
          index = i;
          break;
        }
      }
      if (index >= 0) {
        JMenuItem notInUse = this.isMenu ? menuItem : menu;
        inUse.getParent().add(notInUse, index);
        inUse.getParent().remove(inUse);
      }
    }
    this.isMenu = isMenu;
  }

  public void addTo(Buildable parent) {
    if (parent instanceof GameModule) {
      JMenu fileMenu = GameModule.getGameModule().getFileMenu();
      originalItem = (JMenuItem) fileMenu.getMenuComponent(0);
      fileMenu.insert(getMenuInUse(), 0);
      fileMenu.remove(originalItem);
    }
    else if (parent instanceof PredefinedSetup) {
      PredefinedSetup setup = (PredefinedSetup) parent;
      setup.menu.add(getMenuInUse());
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return isMenu ? new Class[]{PredefinedSetup.class} : new Class[0];
  }

  public static String getConfigureTypeName() {
    return "Pre-defined setup";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm", "PredefinedSetup");
  }

  public void removeFrom(Buildable parent) {
    if (parent instanceof GameModule) {
      JMenu fileMenu = GameModule.getGameModule().getFileMenu();
      fileMenu.insert(originalItem, 0);
      fileMenu.remove(getMenuInUse());
    }
    else if (parent instanceof PredefinedSetup) {
      PredefinedSetup setup = (PredefinedSetup) parent;
      setup.menu.remove(getMenuInUse());
    }
  }
}
