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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.File;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Serializable;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;

import VASSAL.Info;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.IOUtils;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MacOSXMenuManager;

/**
 * Tracks recently-used modules and builds the main GUI window for 
 * interacting with modules.
 * 
 * @author rodneykinney
 * @since 3.1.0
 */
public class ModuleManager {

  private static final int port = 8904;

  public static void main(String[] args) {
    // parse command-line arguments
    final LaunchRequest lr = LaunchRequest.parseArgs(args); 
   
    // try to create ModuleManager
    try {
      new ModuleManager();
    }
    catch (BindException e) {
      // if this fails, a ModuleManager is already listening
    }
    catch (IOException e) {
      // should not happen
      e.printStackTrace();
      System.exit(1);
    }  
    
    // pass launch parameters on to the ModuleManager via the socket
    Socket clientSocket = null;
    ObjectOutputStream out = null;
    InputStream in = null;

    try {
      try {
        clientSocket = new Socket((String) null, port);
      }
      catch (IOException e) {
        System.err.println("Couldn't open socket.");
        System.exit(1);
      }

      try {
        out = new ObjectOutputStream(
                new BufferedOutputStream(clientSocket.getOutputStream()));
        out.writeObject(lr);
        out.flush();
      }
      catch (IOException e) {
        System.err.println("Couldn't write to socket.");
        System.exit(1);
      }

      try {
        in = clientSocket.getInputStream();
        IOUtils.copy(in, System.err);
      }
      catch (IOException e) {
        System.err.println("Couldn't read from socket.");
        System.exit(1);
      }
    }
    finally {
      IOUtils.closeQuietly(in);
      IOUtils.closeQuietly(out);
      IOUtils.closeQuietly(clientSocket);
    }
  }

  private final ServerSocket serverSocket;

  private static ModuleManager instance = null;

  public static ModuleManager getInstance() {
    return instance;
  }

  public ModuleManager() throws IOException {
    if (instance != null) throw new IllegalStateException();
    instance = this;

    serverSocket = new ServerSocket(port); 

    final StartUp start = StartUp.getInstance();
    start.setupErrorLog();
    start.startErrorLog();
    Thread.setDefaultUncaughtExceptionHandler(new ErrorLog());

    start.initSystemProperties();

    if (Info.isMacOSX()) new MacOSXMenuManager();
    else new ModuleManagerMenuManager();

    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        launch();
      }
    });

    // ModuleManagerWindow.getInstance() != null now, so listen on the socket
    new Thread(new SocketListener(serverSocket)).start();
  }

  private class SocketListener implements Runnable {
    private final ServerSocket serverSocket;

    public SocketListener(ServerSocket serverSocket) {
      this.serverSocket = serverSocket;
    }

    public void run() {
      try {
        ObjectInputStream in = null;
        PrintStream out = null;
        Socket clientSocket = null;
        while (true) {
          try {
            clientSocket = serverSocket.accept();
            in = new ObjectInputStream(
                  new BufferedInputStream(clientSocket.getInputStream()));
            
            final String message = execute(in.readObject());

            if (message == null) continue;
            
            out = new PrintStream(
                    new BufferedOutputStream(clientSocket.getOutputStream()));
            out.println(message);
            out.flush();
          }
          catch (IOException e) {
            ErrorLog.warn(e);
          }
          catch (ClassNotFoundException e) {
            ErrorLog.warn(e);
          }
          finally {
            IOUtils.closeQuietly(in);
            IOUtils.closeQuietly(out);
            IOUtils.closeQuietly(clientSocket);
          }
        }
      }
      finally {
        IOUtils.closeQuietly(serverSocket);
      }
    }
  }

  protected void launch() {
    final ModuleManagerWindow window = ModuleManagerWindow.getInstance();
    window.setVisible(true);

    final File prefsFile = new File(Info.getHomeDir(), "Preferences");
    final boolean isFirstTime = !prefsFile.exists();

    if (isFirstTime) new FirstTimeDialog(window).setVisible(true);
  }

  protected String execute(Object req) {
    if (req instanceof LaunchRequest) { 
      final LaunchRequest lr = (LaunchRequest) req;
      final ModuleManagerWindow window = ModuleManagerWindow.getInstance();

      switch (lr.mode) {
      case MANAGE:  
        window.toFront();
        break;
      case LOAD:
        if (Player.LaunchAction.isEditing(lr.module))
          return "module open for editing";
    
        if (lr.game == null) {
          new Player.LaunchAction(window, lr.module).actionPerformed(null);
        }
        else {
          new Player.LaunchAction(
            window, lr.module, lr.game).actionPerformed(null);
        }
        break;
      case EDIT:
        if (Editor.LaunchAction.isInUse(lr.module))
          return "module open for play";
        if (Editor.LaunchAction.isEditing(lr.module))
          return "module open for editing";
  
        new Editor.LaunchAction(window, lr.module).actionPerformed(null);
        break;
      case IMPORT:
        new Editor.ImportLaunchAction(window, lr.module).actionPerformed(null);
        break;
      case NEW:
        new Editor.NewModuleLaunchAction(window).actionPerformed(null);
        break;
      case EDIT_EXT:
        return "not yet implemented";   // FIXME
      case NEW_EXT:
        return "not yet implemented";   // FIXME
      }
    }
    else {
      return "unrecognized command";
    }
  
    return null;
  }

  private static class ModuleManagerMenuManager extends MenuManager {
    private final MenuBarProxy menuBar = new MenuBarProxy();

    @Override
    public JMenuBar getMenuBarFor(JFrame fc) {
      return (fc instanceof ModuleManagerWindow) ? menuBar.createPeer() : null;
    }

    @Override
    public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
      return (fc instanceof ModuleManagerWindow) ? menuBar : null;
    }
  }
}
