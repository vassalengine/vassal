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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintStream;
import java.net.BindException;
import java.net.ConnectException;
import java.net.ServerSocket;
import java.net.Socket;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.build.module.AbstractMetaData;
import VASSAL.build.module.SaveMetaData;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.LongConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.IOUtils;
import VASSAL.tools.menu.MacOSXMenuManager;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;

/**
 * Tracks recently-used modules and builds the main GUI window for 
 * interacting with modules.
 * 
 * @author rodneykinney
 * @since 3.1.0
 */
public class ModuleManager {

  private static final String MODULE_MANAGER_PORT = "moduleManagerPort";
  private static final String MODULE_MANAGER_KEY = "moduleManagerKey";

  private static int port;
  private static long key;

  public static void main(String[] args) {
    // parse command-line arguments
    final LaunchRequest lr = LaunchRequest.parseArgs(args); 

    // set up security key so other users can't talk with our socket
    final LongConfigurer keyConfig =
      new LongConfigurer(MODULE_MANAGER_KEY, null, -1L);
    Prefs.getGlobalPrefs().addOption(keyConfig);
 
    key = keyConfig.getLongValue(-1);
    if (key == -1) {
      key = (long) (Math.random() * Long.MAX_VALUE);
      keyConfig.setValue(key);
      try {
        Prefs.getGlobalPrefs().write();
      }
      catch (IOException e) {
        ErrorLog.log(e);
      }
    }

    lr.key = key;

    // set up prefs for port to listen on
    final IntConfigurer portConfig =
      new IntConfigurer(MODULE_MANAGER_PORT, null, -1); 
    Prefs.getGlobalPrefs().addOption(portConfig);
 
    // set port from command-line if specified; else try the prefs 
    if (lr.port >= 0) {
      port = lr.port;

      // we have a port, write it to the prefs
      portConfig.setValue(port);
      try {
        Prefs.getGlobalPrefs().write();
      }
      catch (IOException e) {
        ErrorLog.log(e);
      }
    }
    else {
      port = portConfig.getIntValue(-1);
    }

    // try to create ModuleManager
    try {
      new ModuleManager();
    }
    catch (BindException e) {
      // if this fails, a ModuleManager is already listening
    }
    catch (IOException e) {
      // should not happen
      ErrorLog.log(e);
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

    // if the port is bad, try a random port
    if (port < 0 || port > 65535) {
      ServerSocket socket = null;

      while (socket == null) {
        // check a random port in the range [49152,65535]
        port = (int)(Math.random() * 16384) + 49152;
        try {
          socket = new ServerSocket(port);

          // we have a port, write it to the prefs
          final IntConfigurer portConfig = (IntConfigurer)
            Prefs.getGlobalPrefs().getOption(MODULE_MANAGER_PORT);
          portConfig.setValue(port);
          try {
            Prefs.getGlobalPrefs().write();
          }
          catch (IOException e) {
            ErrorLog.log(e);
          }
        }
        catch (ConnectException e) {
          // we can't connect, try another port
          IOUtils.closeQuietly(socket);
        }
      }

      serverSocket = socket;
    }
    else {
      serverSocket = new ServerSocket(port);
    }

    final StartUp start = Info.isMacOSX() ?
      new ModuleManagerMacOSXStartUp() : new StartUp();
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
    System.err.println("-- Manager");
    final ModuleManagerWindow window = ModuleManagerWindow.getInstance();
    window.setVisible(true);

    final File prefsFile = new File(Info.getHomeDir(), "Preferences");
    final boolean isFirstTime = !prefsFile.exists();

    if (isFirstTime) new FirstTimeDialog(window).setVisible(true);
  }

  protected String execute(Object req) {
    if (req instanceof LaunchRequest) { 
      final LaunchRequest lr = (LaunchRequest) req;

      if (lr.key != key) {
// FIXME: translate
        return "incorrect key";
      }

      final ModuleManagerWindow window = ModuleManagerWindow.getInstance();

      switch (lr.mode) {
      case MANAGE:  
        window.toFront();
        break;
      case LOAD:
        if (Player.LaunchAction.isEditing(lr.module))
          return "module open for editing";   // FIXME
    
        if (lr.module == null && lr.game != null) {
          // attempt to find the module for the saved game or log
          final AbstractMetaData data = AbstractMetaData.buildMetaData(lr.game);
          if (data != null && data instanceof SaveMetaData) {
            // we found save metadata
            final String moduleName = ((SaveMetaData) data).getModuleName();
            if (moduleName != null && moduleName.length() > 0) {
              // get the module file by module name 
              lr.module = window.getModuleByName(moduleName);
            }
            else {
              // this is a pre 3.1 save file, can't tell the module name
// FIXME: show some error here
              return "cannot find module";
            }
          }
        }

        if (lr.module == null) {
          return "cannot find module";
// FIXME: show some error here
        }
        else if (lr.game == null) {
          new Player.LaunchAction(window, lr.module).actionPerformed(null);
        }
        else { 
          new Player.LaunchAction(
            window, lr.module, lr.game).actionPerformed(null);
        }
        break;
      case EDIT:
        if (Editor.LaunchAction.isInUse(lr.module))
          return "module open for play";      // FIXME
        if (Editor.LaunchAction.isEditing(lr.module))
          return "module open for editing";   // FIXME
  
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
      return "unrecognized command";  // FIXME
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
