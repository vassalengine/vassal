/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman 
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

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Observable;
import java.util.Observer;
import java.util.Properties;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.menu.MenuManager;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class Launcher {
  protected CommandClient cmdC = null;
  protected CommandServer cmdS = null;

  protected final LaunchRequest lr;

  private static Launcher instance = null;

  public static Launcher getInstance() {
    return instance;
  }

  public Launcher(LaunchRequest lr) {
    if (instance != null) throw new IllegalStateException();
    instance = this;

    this.lr = lr;

    if (!lr.standalone) {
      try {
        // set up our command listener
        final ServerSocket serverSocket = new ServerSocket(0);
        cmdS = createCommandServer(serverSocket);
        new Thread(cmdS).start();

        // write our socket port out to the module manager
        final DataOutputStream out = new DataOutputStream(System.out);
        out.writeInt(serverSocket.getLocalPort());
        out.flush();

        // read the module manager's socket port from stdin
        final int port = new DataInputStream(System.in).readInt();

        // set up our command client
        cmdC = new CommandClient(new Socket((String) null, port));
      }
      catch (IOException e) {
        ErrorLog.log(e);
      }
  
      if (cmdC == null || cmdS == null) System.exit(1);
    } 

    final StartUp start = Info.isMacOSX() ? new MacOSXStartUp() : new StartUp();
    if (lr.standalone) start.setupErrorLog();
    start.startErrorLog();
    Thread.setDefaultUncaughtExceptionHandler(new ErrorLog());

    start.initSystemProperties();

    createMenuManager();

    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        try {
          Launcher.this.extractResourcesAndLaunch(0);
        }
        catch (IOException e) {
          reportError(e);
        }
      }
    });
  }

  protected void extractResourcesAndLaunch(final int resourceIndex)
                                                          throws IOException {
    if (lr.extract == null || resourceIndex >= lr.extract.size()) {
      launch();
    }
    else {
      final Properties props = new Properties();
      final InputStream in =
        Launcher.class.getResourceAsStream(lr.extract.get(resourceIndex));
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

    JOptionPane.showMessageDialog(
      null,
      msg,
      Resources.getString("ResourceExtracter.install_failed"),
      JOptionPane.ERROR_MESSAGE
    );
  }

  protected abstract void launch() throws IOException;

  protected abstract MenuManager createMenuManager();

  protected abstract CommandServer createCommandServer(ServerSocket s);
  
  /**
   * Send a message to the ModuleManager that a file has been saved by the
   * Editor or the Player
   * @param f
   */
  public void sendSaveCmd(File f) {
    if (cmdC != null) {
      try {
        cmdC.request(new SaveFileCmd(f));
      }
      catch (IOException e) {
        //
      }
    }
  }
  
  public static class SaveFileCmd implements Serializable {

    private static final long serialVersionUID = 1L;
    protected File file;
    
    public SaveFileCmd(File f) {
      file = f;
    }
    
    public File getFile() {
      return file;
    }
  }
}
