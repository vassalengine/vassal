/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.launch.install;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Properties;
import java.util.ResourceBundle;

import VASSAL.i18n.BundleHelper;


/**
 * Walks the user through a wizard interface. The user may choose between an auto-updating (networked jnlp) or purely
 * local installation (jnlp on local filesystem) installations, and can also select the particular version of VASSAL to
 * install
 * 
 * @author rkinney
 */
public class InstallWizard implements Constants {
  private static BundleHelper bundle = new BundleHelper(ResourceBundle.getBundle("VASSAL.i18n.VASSAL", Locale.getDefault()));
  private WizardDialog dialog;
  private Properties properties;
  public void start() throws IOException, InstantiationException, IllegalAccessException, ClassNotFoundException {
    Properties p = new Properties();
    InputStream in = getClass().getResourceAsStream("/"+INSTALL_PROPERTIES);
    if (in != null) {
      p.load(in);
    }
    properties = new Properties(p);
    dialog = new WizardDialog(this);
    dialog.setTitle(properties.getProperty(TITLE,InstallWizard.getResources().getString("Install.install_vassal"))); //$NON-NLS-1$ //$NON-NLS-2$
    dialog.setScreen((Screen) Class.forName(p.getProperty(INITIAL_SCREEN, ChooseVersionScreen.class.getName())).newInstance());
    dialog.setVisible(true);
  }

  public void put(String key, String value) {
    properties.put(key, value);
  }

  public String get(String key) {
    return properties.getProperty(key);
  }

  /** Sets the next screen to an instance specified by the named key 
   * @param defaultClass TODO*/
  public Screen next(String screenKey, Class defaultClass) {
    Screen screen;
    try {
      String screenClass = properties.getProperty(screenKey);
      if (screenClass != null) {
        screen = (Screen) Class.forName(screenClass).newInstance();
      }
      else {
        if (defaultClass != null) {
          screen = (Screen) defaultClass.newInstance();
        }
        else {
          throw new NullPointerException("Screen "+screenKey+" not specified"); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
      dialog.setScreen(screen);
    }
    catch (Exception e) {
      screen = new FailureScreen(e);
      e.printStackTrace();
      dialog.setScreen(screen);
    }
    return screen;
  }

  public static void main(String[] args) throws Exception {
    InstallWizard wiz = new InstallWizard();
    wiz.start();
  }

  public WizardDialog getDialog() {
    return dialog;
  }
  
  public static BundleHelper getResources() {
    return bundle;
  }
}
