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
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;

import com.apple.eawt.Application;
import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationEvent;

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

  private static enum Mode {
    MANAGE,
    LOAD,
    EDIT,
    IMPORT,
    NEW,
    EDIT_EXT,
    NEW_EXT
  }

  protected static class LaunchRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    public Mode mode;
    public File module;
    public File game;
    public File extension;
  }

  public static void main(String[] args) {
    // parse command-line arguments
    final LaunchRequest lr = parseArgs(args); 
   
    // try to create ModuleManager
    try {
      new ModuleManager();
    }
    catch (IOException e) {
      // if this fails, a ModuleManager is already listening
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

  protected ModuleManagerWindow window;

  private final ServerSocket serverSocket;

  public ModuleManager() throws IOException {
    serverSocket = new ServerSocket(port); 

    StartUp.setupErrorLog();
    StartUp.startErrorLog();
    Thread.setDefaultUncaughtExceptionHandler(new ErrorLog());

    StartUp.initSystemProperties();

    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        launch();
      }
    });
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
    if (Info.isMacOSX()) new MacOSXMenuManager();
    else new ModuleManagerMenuManager();

    final File prefsFile = new File(Info.getHomeDir(), "Preferences");
    final boolean isFirstTime = !prefsFile.exists();

    window = ModuleManagerWindow.getInstance();

    if (Info.isMacOSX()) {
      final Application app = Application.getApplication();
      app.addApplicationListener(new ApplicationAdapter() {
        @Override
        public void handleOpenFile(ApplicationEvent e) {
          final String filename = e.getFilename();
          if (filename.endsWith(".vmod")) {
            final LaunchRequest lr = new LaunchRequest();
            lr.mode = Mode.LOAD;
            lr.module = new File(filename);
            execute(lr);
            e.setHandled(true); 
          }
          else {
            e.setHandled(false);
          }
        }

        @Override
        public void handleReOpenApplication(ApplicationEvent e) {
          final LaunchRequest lr = new LaunchRequest();
          lr.mode = Mode.MANAGE; 
          execute(lr);
          e.setHandled(true);
        }
      }); 
    }

    window.setVisible(true);
    if (isFirstTime) new FirstTimeDialog(window).setVisible(true);

    new Thread(new SocketListener(serverSocket)).start();
  }

  protected String execute(Object req) {
    if (req instanceof LaunchRequest) { 
      final LaunchRequest lr = (LaunchRequest) req;

      System.out.println("lr.mode = " + lr.mode + 
                       ", lr.module = " + lr.module + 
                       ", lr.game = " + lr.game);

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

    return null;
  }


  protected static LaunchRequest parseArgs(String[] args) {
    /*
      VASSAL -e module [log/save]
      VASSAL -i module
      VASSAL -l module [log/save]
      VASSAL -n

      Operation:
        -e, --edit
        -i, --import
        -l, --load
        -n, --new

      Options:
        -a, --auto
        -h, --help
        -x, --extract
            --
    */

    final LaunchRequest lr = new LaunchRequest();

    // parse the options
    int i = -1;
    while (++i < args.length) {
      if (!args[i].startsWith("-") || "--".equals(args[i])) {
        // end of options
        break;
      }
      else if ("-a".equals(args[i]) || "--auto".equals(args[i])) {
        throw new UnsupportedOperationException();  // FIXME
      }
      else if ("-e".equals(args[i]) || "--edit".equals(args[i])) {
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.EDIT;
      }
      else if ("-h".equals(args[i]) || "--help".equals(args[i])) {
        System.err.println("TODO: Command-line help.");
        System.exit(0);
      }
      else if ("-i".equals(args[i]) || "--import".equals(args[i])) {
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.IMPORT;
      }
      else if ("-l".equals(args[i]) || "--load".equals(args[i])) {
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.LOAD;
      }
      else if ("-n".equals(args[i]) || "--new".equals(args[i])) {
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.NEW;
      }
      else if ("-x".equals(args[i]) || "--extract".equals(args[i])) {
        throw new UnsupportedOperationException();  // FIXME
      }
      else if ("--auto-extensions".equals(args[i])) {
        throw new UnsupportedOperationException();  // FIXME
      }
      else if ("--new-extension".equals(args[i])) {
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.NEW_EXT;
      }
      else if ("--edit-extension".equals(args[i])) {
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.EDIT_EXT; 
      }
      else {
        throw new IllegalArgumentException("unrecognized option: " + args[i]);
      }
    }

    // load by default if an argument is given; otherwise, manage
    if (lr.mode == null) {
      lr.mode = i < args.length ? Mode.LOAD : Mode.MANAGE;
    }

    // get the module and game, if specified
    switch (lr.mode) {
    case MANAGE:
      break;
    case LOAD:
    case EDIT:
      if (i < args.length) {
        lr.module = new File(args[i++]);
        if (i < args.length) {
          lr.game = new File(args[i++]);
        }
      }
      else {
        throw new IllegalArgumentException("too few arguments");
      }
      break;
    case IMPORT:
    case NEW_EXT:
      if (i < args.length) {
        lr.module = new File(args[i++]);
      }
      else {
        throw new IllegalArgumentException("too few arguments");
      }
      break;
    case EDIT_EXT:
      if (i + 1 < args.length) {
        lr.module = new File(args[i++]);
        lr.extension = new File(args[i++]);
      }
      else {
        throw new IllegalArgumentException("too few arguments");
      }
      break;
    case NEW:
      break;
    }

    if (i < args.length) {
      throw new IllegalArgumentException("too many arguments");
    }   
 
    return lr;
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
