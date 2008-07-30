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

import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.net.ServerSocket;
import java.net.Socket;

import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
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

  protected Launcher(String[] args) {
    if (instance != null) throw new IllegalStateException();
    instance = this;

    final boolean standalone = args.length > 0;

    // parse the command line args now if we're standalone, since they
    // could be messed up and so we'll bail before setup
    LaunchRequest lr = null; 
    if (standalone) {
      // Note: We could do more sanity checking of the launch request
      // in standalone mode, but we don't bother because this is meant
      // only for debugging, not for normal use. If you pass nonsense
      // arguments (e.g., '-e' to the Player), don't expect it to work.
      try {
        lr = LaunchRequest.parseArgs(args);
      }
      catch (LaunchRequestException e) {
        System.err.println("VASSAL: " + e.getMessage());
        System.exit(1);
      }
    }

    // start the error log and setup system properties
    final StartUp start = Info.isMacOSX() ? new MacOSXStartUp() : new StartUp();
    if (standalone) start.setupErrorLog();
    start.startErrorLog();
    System.err.println("-- " + getClass().getSimpleName());
    Thread.setDefaultUncaughtExceptionHandler(new ErrorLog());
    start.initSystemProperties();

    // if we're not standalone, contact the module manager for instructions 
    if (!standalone) {
      try {
        // set up our command listener
        final ServerSocket serverSocket = new ServerSocket(0);
        cmdS = createCommandServer(serverSocket);
        new Thread(cmdS).start();

        // write our socket port out to the module manager
        final DataOutputStream out = new DataOutputStream(System.out);
        out.writeInt(serverSocket.getLocalPort());
        out.flush();

        // read the module manager's socket port and launch request from stdin
        final ObjectInputStream in = new ObjectInputStream(System.in);
        final int port = in.readInt();
        
        lr = (LaunchRequest) in.readObject();

        // set up our command client
        cmdC = new CommandClient(new Socket((String) null, port));
      }
      catch (ClassNotFoundException e) {
        ErrorDialog.bug(e);
        System.exit(1);
      }
      catch (IOException e) {
        // What we've got here is a failure to communicate.
        ErrorDialog.error(
          Resources.getString("Error.communication_error"),
          Resources.getString("Error.communication_error"),
          e,
          Resources.getString("Error.communication_error_message",
            Resources.getString(getClass().getSimpleName() + ".app_name"))
        );         
        System.exit(1);
      }
    }

    this.lr = lr;

    createMenuManager();

    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        try {
          launch();
        }
        catch (IOException e1) {
          if (cmdC == null) {
            // we are standalone, so warn the user directly
            ErrorDialog.error(
              Resources.getString("Launcher.load_error"),
              Resources.getString("Launcher.load_error"),
              e1,
              Resources.getString("Launcher.load_error_message"),
              e1.getMessage()
            );
          }
          else {
            // we have a manager, so pass the load failure back to it
            try {
              cmdC.request(new LoadFailedCmd(e1));
            }
            catch (IOException e2) {
              // warn the user directly as a last resort 
              ErrorDialog.error(
                Resources.getString("Launcher.load_error"),
                Resources.getString("Launcher.load_error"),
                e1,
                Resources.getString("Launcher.load_error_message"),
                e1.getMessage()
              );

              ErrorDialog.error(
                Resources.getString("Launcher.communication_error"),
                Resources.getString("Launcher.communication_error"),
                e2,
                Resources.getString("Launcher.communication_error_message",
                  Resources.getString(getClass().getSimpleName() + ".app_name"))
              );
            }
          }

          System.exit(1);
        }
      }
    });
  }

  protected abstract void launch() throws IOException;

  protected abstract MenuManager createMenuManager();

  protected abstract CommandServer createCommandServer(ServerSocket s);
 
  public static class LoadFailedCmd implements Serializable {
    private static final long serialVersionUID = 1L;

    protected final Throwable t;

    public LoadFailedCmd(Throwable throwable) {
      t = throwable;
    }

    public Throwable getThrowable() {
      return t;
    }
  }
 
  /**
   * Send a message to the ModuleManager that a file has been saved by the
   * Editor or the Player
   * @param f
   */
// FIXME: this isn't called from anywhere yet!
  public void sendSaveCmd(File f) {
    if (cmdC != null) {
      try {
        cmdC.request(new SaveFileCmd(f));
      }
      // FIXME: review error message
      catch (IOException e) {
      }
    }
  }
  
  public static class SaveFileCmd implements Serializable {
    private static final long serialVersionUID = 1L;

    protected final File file;
    
    public SaveFileCmd(File f) {
      file = f;
    }
    
    public File getFile() {
      return file;
    }
  }
}
