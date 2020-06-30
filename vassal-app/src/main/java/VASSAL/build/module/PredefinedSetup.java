/*
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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.swing.AbstractAction;
import javax.swing.Action;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.menu.ChildProxy;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;
import VASSAL.tools.menu.ParentProxy;

/**
 * Defines a saved game that is accessible from the File menu.
 * The game will be loaded in place of a normal New Game
 */
public class PredefinedSetup extends AbstractConfigurable implements GameComponent {
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String FILE = "file"; //$NON-NLS-1$
  public static final String USE_FILE = "useFile"; //$NON-NLS-1$
  public static final String IS_MENU = "isMenu"; //$NON-NLS-1$

  protected boolean isMenu;
  protected boolean useFile = true;
  protected String fileName;

  protected MenuItemProxy menuItem;
  protected MenuProxy menu;

  protected VisibilityCondition showFile;
  protected VisibilityCondition showUseFile;
  protected AbstractAction launchAction;

  public PredefinedSetup() {
    launchAction = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
    menuItem = new MenuItemProxy(launchAction);

    menu = new MenuProxy();

    showFile = new VisibilityCondition() {
      @Override
      public boolean shouldBeVisible() {
        return !isMenu && useFile;
      }
    };

    showUseFile = new VisibilityCondition() {
      @Override
      public boolean shouldBeVisible() {
        return !isMenu;
      }
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString(Resources.NAME_LABEL),
        Resources.getString("Editor.PredefinedSetup.parent_menu"), //$NON-NLS-1$
        Resources.getString("Editor.PredefinedSetup.predefined_file"), //$NON-NLS-1$
        Resources.getString("Editor.PredefinedSetup.saved_game") //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Boolean.class,
      Boolean.class,
      File.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      IS_MENU,
      USE_FILE,
      FILE
    };
  }

  @Override
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

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      menuItem.getAction().putValue(Action.NAME, value);
      menu.setText((String) value);
    }
    else if (USE_FILE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      useFile = (Boolean) value;
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
      setMenu((Boolean) value);
    }
  }

  @Override
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
    if (useFile && fileName != null) {
      try {
        GameModule.getGameModule()
                  .getGameState()
                  .loadGameInBackground(fileName, getSavedGameContents());
      }
      catch (IOException e) {
        ErrorDialog.dataError(new BadDataReport(this, Resources.getString("Error.not_found", "Setup"),fileName,e)); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    else {
      GameModule.getGameModule().getGameState().setup(false);
      GameModule.getGameModule().getGameState().setup(true);
    }
  }

  public InputStream getSavedGameContents() throws IOException {
    return GameModule.getGameModule().getDataArchive().getInputStream(fileName);
  }

  private ChildProxy<?> getMenuInUse() {
    return isMenu ? menu : menuItem;
  }

  private void setMenu(boolean isMenu) {
    if (isMenu == this.isMenu) return;

    final ChildProxy<?> inUse = getMenuInUse();
    final ParentProxy parent = inUse.getParent();

    if (parent != null) {
      // swap our items if one is already in the menu
      final ChildProxy<?> notInUse = this.isMenu ? menuItem : menu;

      parent.insert(notInUse, parent.getIndex(inUse));
      parent.remove(inUse);
    }

    this.isMenu = isMenu;
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof GameModule) {
      MenuManager.getInstance().addToSection("PredefinedSetup", getMenuInUse()); //$NON-NLS-1$
    }
    else if (parent instanceof PredefinedSetup) {
      final PredefinedSetup setup = (PredefinedSetup) parent;
      setup.menu.add(getMenuInUse());
    }
    MenuManager.getInstance().removeAction("GameState.new_game"); //$NON-NLS-1$
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().getWizardSupport().addPredefinedSetup(this);
  }

  @Override
  public void removeFrom(Buildable parent) {
    if (parent instanceof GameModule) {
      MenuManager.getInstance()
                 .removeFromSection("PredefinedSetup", getMenuInUse()); //$NON-NLS-1$
    }
    else if (parent instanceof PredefinedSetup) {
      final PredefinedSetup setup = (PredefinedSetup) parent;
      setup.menu.remove(getMenuInUse());
    }
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().getWizardSupport().removePredefinedSetup(this);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return isMenu ? new Class<?>[]{PredefinedSetup.class} : new Class<?>[0];
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PredefinedSetup.component_type"); //$NON-NLS-1$
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm", "PredefinedSetup"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public boolean isMenu() {
    return isMenu;
  }

  public boolean isUseFile() {
    return useFile;
  }

  public String getFileName() {
    return fileName;
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  @Override
  public void setup(boolean gameStarting) {
    launchAction.setEnabled(!gameStarting);
  }

}
