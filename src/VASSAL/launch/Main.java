/*
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
package VASSAL.launch;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Properties;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.JarArchive;

/*
import javax.swing.RepaintManager;
import org.jdesktop.swinghelper.debug.CheckThreadViolationRepaintManager;
import org.jdesktop.swinghelper.debug.EventDispatchThreadHangMonitor;
*/

/** 
 * @deprecated Use {@link Editor}, {@link Player}, and {@link ModuleManager}
 *  as entry points for VASSAL instead.
 */
@Deprecated
public class Main {
  protected boolean isFirstTime;
  protected boolean builtInModule;
  protected File moduleFile;
  protected boolean editMode;
  protected File savedGame;
  protected List<String> extractTargets = new ArrayList<String>();
  protected List<String> autoExtensions = new ArrayList<String>();

  public Main(final String[] args) {
    initSystemProperties();

    // begin the error log
    System.err.println("-- OS " + System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$
    System.err.println("-- Java version " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
    System.err.println("-- VASSAL version " + Info.getVersion()); //$NON-NLS-1$

    new Thread(new ErrorLog.Group(), "Main Thread") { //$NON-NLS-1$
      public void run() {
        Runnable runnable = new Runnable() {
          public void run() {
            try {
              Main.this.configure(args);
              Main.this.extractResourcesAndLaunch(0);
            }
            catch (IOException e) {
              reportError(e);
            }
          }
        };
        SwingUtilities.invokeLater(runnable);
      }
    }.start();
  }

  protected void extractResourcesAndLaunch(final int resourceIndex) throws IOException {
    if (resourceIndex >= extractTargets.size()) {
      launch();
    }
    else {
      final Properties props = new Properties();
      final InputStream in =
        Main.class.getResourceAsStream(extractTargets.get(resourceIndex));
      if (in != null) {
        try {
          props.load(in);
        }
        finally {
          try {
            in.close();
          }
          catch (IOException e) {
            ErrorLog.log(e);
          }
        }
      }

      new ResourceExtracter(Prefs.getGlobalPrefs(), props, new Observer() {
        public void update(Observable o, Object arg) {
          try {
            extractResourcesAndLaunch(resourceIndex + 1);
          }
          catch (IOException e) {
            reportError(e);
          }
        }
      }).install();
    }
  }

  protected void reportError(Exception e) {
    ErrorLog.log(e);
    String msg = e.getMessage();
    if (msg == null) {
      msg = e.getClass().getSimpleName();
    }
    JOptionPane.showMessageDialog(null, msg, Resources.getString("ResourceExtracter.install_failed"), JOptionPane.ERROR_MESSAGE);
  }

  protected void initSystemProperties() {
    //
    // Error log setup
    //
    String stderr = "stderr";           //$NON-NLS-1$
    final String errorLog = "errorLog"; //$NON-NLS-1$

    // redirect stderr to errorLog by default
    if (System.getProperty(stderr) == null) {
      System.setProperty(stderr,
        new File(Info.getHomeDir(), errorLog).getPath());
    }

    // write no log if stderr is set to null
    stderr = System.getProperty(stderr); 
    if (!"null".equals(stderr)) { //$NON-NLS-1$
      try {
        System.setErr(new PrintStream(new FileOutputStream(stderr)));
      }
      catch (IOException ex) {
        System.err.println(
          "Unable to redirect stderr to " + stderr); //$NON-NLS-1$
      }
    }

    //
    // HTTP proxying setup
    //
    final String httpProxyHost = "http.proxyHost";  //$NON-NLS-1$
    final String proxyHost = "proxyHost";           //$NON-NLS-1$

    if (System.getProperty(httpProxyHost) == null && 
        System.getProperty(proxyHost) != null) {
      System.setProperty(httpProxyHost, System.getProperty(proxyHost));
    }

    final String httpProxyPort = "http.proxyPort"; //$NON-NLS-1$
    final String proxyPort = "proxyPort";          //$NON-NLS-1$

    if (System.getProperty(httpProxyPort) == null &&
        System.getProperty(proxyPort) != null) {
      System.setProperty(httpProxyPort, System.getProperty(proxyPort));
    }

    //
    // Miscellaneous setup
    //
    System.setProperty("swing.aatext", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("swing.boldMetal", "false"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("awt.useSystemAAFontSettings", "on"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  protected void launch() throws IOException {
    if (isFirstTime) {
      new FirstTimeDialog(null).setVisible(true);
    }
    else if (builtInModule) {
      GameModule.init(createModule(createDataArchive()));
      for (String ext : autoExtensions) {
        createExtension(ext).build();
      }
      createExtensionsLoader().addTo(GameModule.getGameModule());
      Localization.getInstance().translate();
      GameModule.getGameModule().getWizardSupport().showWelcomeWizard();
    }
    else if (moduleFile == null) {
      ModuleManagerWindow.getInstance().setVisible(true);
    }
    else if (editMode) {
      new EditModuleAction(moduleFile).loadModule(moduleFile);
    }
    else {
      GameModule.init(createModule(createDataArchive()));
      createExtensionsLoader().addTo(GameModule.getGameModule());
      Localization.getInstance().translate();
      if (savedGame != null) {
        GameModule.getGameModule().getFrame().setVisible(true);
        GameModule.getGameModule().getGameState().loadGameInBackground(savedGame);
      }
      else {
        GameModule.getGameModule().getWizardSupport().showWelcomeWizard();
      }
    }
  }

  protected ExtensionsLoader createExtensionsLoader() {
    return new ExtensionsLoader();
  }

  protected ModuleExtension createExtension(String name) {
    return new ModuleExtension(new JarArchive(name));
  }

  protected DataArchive createDataArchive() throws IOException {
    if (builtInModule) {
      return new JarArchive();
    }
    else {
      return new DataArchive(moduleFile.getPath());
    }
  }

  protected GameModule createModule(DataArchive archive) {
    return new BasicModule(archive);
  }

  /** @deprecated Use {@link Info.getVersion()} instead. */
  @Deprecated
  protected String getVersion() {
    return VASSAL.Info.getVersion();
  }

  protected void configure(final String[] args) {
    File prefsFile = new File(Info.getHomeDir(), "Preferences");
    isFirstTime = !prefsFile.exists();
    int n = -1;
    while (++n < args.length) {
      String arg = args[n];
      if ("-auto".equals(arg)) {
        builtInModule = true;
      }
      else if ("-extract".equals(arg)) {
        extractTargets.add(args[++n]);
      }
      else if ("-autoextensions".equals(arg)) {
        for (String ext : args[++n].split(",")) {
          autoExtensions.add(ext.replace("_"," "));
        }
      }
      else if ("-edit".equals(arg)) {
        editMode = true;
      }
      else if ("-load".equals(arg)) {
        savedGame = new File(args[++n]);
      }
      else if (!arg.startsWith("-")) {
        moduleFile = new File(arg);
      }
    }
  }

  public static void main(String[] args) {
//    EventDispatchThreadHangMonitor.initMonitoring();
//    RepaintManager.setCurrentManager(new CheckThreadViolationRepaintManager());
    new Main(args);
  }
}
