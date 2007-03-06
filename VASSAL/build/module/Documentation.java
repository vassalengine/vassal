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
import org.w3c.dom.Element;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.AboutScreen;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.Tutorial;
import VASSAL.configure.CompoundValidityChecker;
import VASSAL.configure.Configurer;
import VASSAL.configure.MandatoryComponent;
import VASSAL.configure.SingleChildInstance;
import VASSAL.i18n.Resources;

/**
 * Represents the <code>Help</code> menu of the controls window
 */
//I18n: Complete
public class Documentation extends AbstractConfigurable {
  /** Preferences key for the directory where VASSAL documentation is stored */
  public static final String DOCS_DIR = "docsDirectory"; 

  private javax.swing.JMenu controls;

  public Documentation() {
    controls = new javax.swing.JMenu(Resources.getString(Resources.HELP));
  }

  public javax.swing.JMenu getHelpMenu() {
    return controls;
  }

  public void build(Element el) {
    if (el == null) {
      try {
        AboutScreen about = new AboutScreen();
        about.setAttribute(AboutScreen.TITLE, Resources.getString("Documentation.about_vassal")); 
        about.setAttribute(AboutScreen.FILE, "/images/Splash.gif"); 
        about.addTo(this);
        add(about);
      }
      catch (Exception err) {
        err.printStackTrace();
      }

      HelpFile intro = new HelpFile();
      intro.setAttribute(HelpFile.TITLE, Resources.getString("Documentation.quick_start")); 
      intro.setAttribute(HelpFile.FILE, "/help/Intro.html"); 
      intro.setAttribute(HelpFile.TYPE, HelpFile.RESOURCE);
      intro.addTo(this);
      add(intro);
    }
    else {
      super.build(el);
    }
  }

  public static File getDocumentationBaseDir() {
    File f = null;
    if (GameModule.getGameModule() != null
        && GameModule.getGameModule().getGlobalPrefs() != null) {
      f = (File) GameModule.getGameModule().getGlobalPrefs().getValue(DOCS_DIR);
    }
    return f;
  }

  public void addTo(Buildable b) {
    // Moved following line to Resources.getString() as it is needed earlier
    // GameModule.getGameModule().getGlobalPrefs().addOption(null, new DirectoryConfigurer(DOCS_DIR, null));   
    GameModule.getGameModule().getFrame().getJMenuBar().add(controls);
    validator = new CompoundValidityChecker
        (new MandatoryComponent(this, AboutScreen.class),
         new SingleChildInstance(GameModule.getGameModule(), getClass()));
  }

  public void removeFrom(Buildable b) {
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public Configurer getConfigurer() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{HelpFile.class, AboutScreen.class, Tutorial.class};
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Documentation.component_type"); 
  }

  public String getConfigureName() {
    return null;
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual"); 
    try {
      return new HelpFile(null, new File(dir, "HelpMenu.htm")); 
    }
    catch (MalformedURLException ex) {
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
