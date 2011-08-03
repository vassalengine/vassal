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

import java.io.File;
import java.net.MalformedURLException;

import javax.swing.JMenu;
import javax.swing.JMenuBar;

import org.w3c.dom.Element;

import VASSAL.Info;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.AboutScreen;
import VASSAL.build.module.documentation.BrowserHelpFile;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.Tutorial;
import VASSAL.configure.Configurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.menu.MenuManager;

/**
 * Represents the <code>Help</code> menu of the controls window
 */
public class Documentation extends AbstractConfigurable {
  public Documentation() {
  }

  public JMenu getHelpMenu() {
    final JMenuBar mb = MenuManager.getInstance().getMenuBarFor(
      GameModule.getGameModule().getFrame());

    return mb.getMenu(mb.getMenuCount()-1);
  }

  public void build(Element el) {
    if (el == null) {
      final AboutScreen about = new AboutScreen();
      about.setAttribute(AboutScreen.TITLE,
        Resources.getString("Documentation.about_module")); //$NON-NLS-1$
      about.setAttribute(AboutScreen.FILE, "/images/Splash.png"); //$NON-NLS-1$
      about.addTo(this);
      add(about);

      final HelpFile intro = new HelpFile();
      intro.setAttribute(HelpFile.TITLE,
        Resources.getString("Documentation.quick_start")); //$NON-NLS-1$
      intro.setAttribute(HelpFile.FILE, "/help/Intro.html"); //$NON-NLS-1$
      intro.setAttribute(HelpFile.TYPE, HelpFile.RESOURCE);
      intro.addTo(this);
      add(intro);
    }
    else {
      super.build(el);
    }
  }

  public static File getDocumentationBaseDir() {
    return Info.getDocDir();
  }

  public void addTo(Buildable b) {
    validator =
      new SingleChildInstance(GameModule.getGameModule(), getClass());
  }

  public void removeFrom(Buildable b) {
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  public Configurer getConfigurer() {
    return null;
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{
      BrowserHelpFile.class,
      AboutScreen.class,
      Tutorial.class,
      HelpFile.class
    };
  }

  public static String getConfigureTypeName() {
    return Resources.getString(
      "Editor.Documentation.component_type"); //$NON-NLS-1$
  }

  public String getConfigureName() {
    return null;
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
    try {
      return new HelpFile(null, new File(dir, "HelpMenu.htm")); //$NON-NLS-1$
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
      return null;
    }
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }
}
