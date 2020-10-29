/*
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;

import org.apache.commons.lang3.SystemUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.LongConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateVassalWindow;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.logging.LoggedOutputStream;
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
  private static final Logger logger =
    LoggerFactory.getLogger(ModuleManager.class);

  private static final String NEXT_VERSION_CHECK = "nextVersionCheck"; //NON-NLS

  public static final String MAXIMUM_HEAP = "maximumHeap"; //$NON-NLS-1$

  @Deprecated(since = "2020-10-21", forRemoval = true)
  public static final String INITIAL_HEAP = "initialHeap"; //$NON-NLS-1$

  private static FileLock acquireLock(FileChannel fc) throws IOException {
    try {
      return fc.lock();
    }
    catch (OverlappingFileLockException e) {
      throw new IOException(e);
    }
  }

  private static FileLock tryLock(FileChannel fc) throws IOException {
    try {
      return fc.tryLock();
    }
    catch (OverlappingFileLockException e) {
      throw new IOException(e);
    }
  }

  public static void main(String[] args) {
    // do this before the graphics subsystem fires up or it won't stick
    System.setProperty("swing.boldMetal", "false");

    try {
      Info.setConfig(new StandardConfig());
    }
    catch (IOException e) {
// FIXME: should be a dialog...
      System.err.println("VASSAL: " + e.getMessage());  //NON-NLS
      System.exit(1);
    }

// FIXME: We need to catch more exceptions in main() and then exit in
// order to avoid situations where the main thread ends due to an uncaught
// exception, but there are other threads still running, and so VASSAL
// does not quit. For example, this can happen if an IllegalArgumentException
// is thrown here...

    // parse command-line arguments
    LaunchRequest lr = null;
    try {
      lr = LaunchRequest.parseArgs(args);
    }
    catch (LaunchRequestException e) {
// FIXME: should be a dialog...
      System.err.println("VASSAL: " + e.getMessage()); //NON-NLS
      System.exit(1);
    }

    if (lr.mode == LaunchRequest.Mode.TRANSLATE) {
      // show the translation window in translation mode
      SwingUtilities.invokeLater(() -> {
        // FIXME: does this window exit on close?
        new TranslateVassalWindow(null).setVisible(true);
      });
      return;
    }

    //
    // How we start exactly one request server:
    //
    // To ensure that exactly one process acts as the request server, we
    // acquire a lock on the ~/VASSAL/key file, and then attempt to acquire
    // a lock on the ~/VASSAL/lock file. If we cannot lock ~/VASSAL/lock,
    // then there is already a server running; in that case, we read the
    // port number and security key from ~/VASSAL/key. If we can lock
    // ~/VASSAL/lock, then we start the server, write the port number and
    // key to ~/VASSAL/key, and continue to hold the lock on ~/VASSAL/lock.
    // Finally, we unlock ~/VASSAL/key and proceed to act as a client,
    // sending requests over localhost:port using the security key.
    //
    // The advantages of this method are:
    //
    // (1) No race conditions between processes started at the same time.
    // (2) No port collisions, because we don't use a predetermined port.
    //

    // Different versions of VASSAL can all co-exist, each with own
    // Module Manager
    final String ver = Info.getReportableVersion();

    final File keyfile = new File(Info.getConfDir(), "key-" + ver);
    final File lockfile = new File(Info.getConfDir(), "lock-" + ver);

    int port = 0;
    long key = 0;

    try (RandomAccessFile kraf = new RandomAccessFile(keyfile, "rw")) {
      // acquire an exclusive lock on the key file
      try (FileLock klock = acquireLock(kraf.getChannel())) {
        // determine whether we are the server or a client

        // Note: We purposely keep lout open in the case where we are the
        // server, because closing lout will release the lock.
        final FileOutputStream lout = new FileOutputStream(lockfile);
        final FileLock lock = tryLock(lout.getChannel());

        if (lock != null) {
          // we have the lock, so we will be the request server

          // bind to an available port on the loopback device
          final ServerSocket serverSocket =
            new ServerSocket(0, 0, InetAddress.getByName(null));

          // write the port number where we listen to the key file
          port = serverSocket.getLocalPort();
          kraf.writeInt(port);

          // create new security key and write it to the key file
          key = (long) (Math.random() * Long.MAX_VALUE);
          kraf.writeLong(key);

          // create a new Module Manager
          new ModuleManager(serverSocket, key, lout, lock);
        }
        else {
          // we do not have the lock, so we will be a request client
          lout.close();

          // read the port number we will connect to from the key file
          port = kraf.readInt();

          // read the security key from the key file
          key = kraf.readLong();
        }
      }
    }
    catch (IOException e) {
// FIXME: should be a dialog...
      System.err.println("VASSAL: IO error"); //NON-NLS
      e.printStackTrace();
      System.exit(1);
    }
    // lock on the key file is released

    lr.key = key;

    // pass launch parameters on to the ModuleManager via the socket
    try (Socket clientSocket = new Socket((String) null, port);
         ObjectOutputStream out = new ObjectOutputStream(
           new BufferedOutputStream(clientSocket.getOutputStream()))) {

      out.writeObject(lr);
      out.flush();

      try (InputStream in = clientSocket.getInputStream()) {
        IOUtils.copy(in, System.err);
      }
    }
    catch (UnknownHostException e) {
      logger.error("Unable to open socket for loopback device", e); //NON-NLS
      System.exit(1);
    }
    catch (IOException e) {
// FIXME: should be a dialog...
      logger.error("VASSAL: Problem with socket on port {}", port, e); //NON-NLS
      System.exit(1);
    }
  }

  private static ModuleManager instance = null;

  public static ModuleManager getInstance() {
    return instance;
  }

  private final long key;

  private final FileOutputStream lout;
  private final FileLock lock;

  public ModuleManager(ServerSocket serverSocket, long key,
                       FileOutputStream lout, FileLock lock)
                                                           throws IOException {

    if (instance != null) throw new IllegalStateException();
    instance = this;

    this.key = key;

    // we hang on to these to prevent the lock from being lost
    this.lout = lout;
    this.lock = lock;

    // truncate the errorLog
    final File errorLog = Info.getErrorLogPath();
    new FileOutputStream(errorLog).close();

    final StartUp start = SystemUtils.IS_OS_MAC_OSX ?
      new ModuleManagerMacOSXStartUp() : new StartUp();

    start.startErrorLog();

    // log everything which comes across our stderr
    System.setErr(new PrintStream(new LoggedOutputStream(), true, Charset.defaultCharset()));

    Thread.setDefaultUncaughtExceptionHandler(new ExceptionHandler());

    start.initSystemProperties();

    if (SystemUtils.IS_OS_MAC_OSX) new MacOSXMenuManager();
    else new ModuleManagerMenuManager();

    new CustomVmOptions().ensureCustomVmOptionsFileExistsInConfDir();

    SwingUtilities.invokeLater(this::launch);

    // ModuleManagerWindow.getInstance() != null now, so listen on the socket
    final Thread socketListener = new Thread(
      new SocketListener(serverSocket), "socket listener");
    socketListener.setDaemon(true);
    socketListener.start();

    final Prefs globalPrefs = Prefs.getGlobalPrefs();

    // determine when we should next check on the current version of VASSAL
    final LongConfigurer nextVersionCheckConfig =
      new LongConfigurer(NEXT_VERSION_CHECK, null, -1L);
    globalPrefs.addOption(null, nextVersionCheckConfig);

    long nextVersionCheck = nextVersionCheckConfig.getLongValue(-1L);
    if (nextVersionCheck < System.currentTimeMillis()) {
      new UpdateCheckRequest().execute();
    }

    // set the time for the next version check
    if (nextVersionCheck == -1L) {
      // this was our first check; randomly check after 0-10 days to
      // to spread version checks evenly over a 10-day period
      nextVersionCheck = System.currentTimeMillis() +
                         (long) (Math.random() * 10 * 86400000);
    }
    else {
      // check again in 10 days
      nextVersionCheck += 10 * 86400000;
    }

    nextVersionCheckConfig.setValue(nextVersionCheck);

// FIXME: the importer heap size configurer doesn't belong here
    // the maximum heap size for the module importer
    final IntConfigurer maxHeapConf = new IntConfigurer(
      MAXIMUM_HEAP,
      Resources.getString("GlobalOptions.maximum_heap"),  //$NON-NLS-1$
      512
    );
    globalPrefs.addOption("Importer", maxHeapConf); //NON-NLS
  }

  public void shutDown() throws IOException {
    lock.release();
    lout.close();
  }

  private class SocketListener implements Runnable {
    private final ServerSocket serverSocket;

    public SocketListener(ServerSocket serverSocket) {
      this.serverSocket = serverSocket;
    }

    @Override
    public void run() {
      try {
        Socket clientSocket = null;

        // TODO while can only complete by throwing, do not use exceptions for ordinary control flow
        while (true) {
          try {
            clientSocket = serverSocket.accept();
            final String message;

            try (ObjectInputStream in = new ObjectInputStream(
              new BufferedInputStream(clientSocket.getInputStream()))) {

              message = execute(in.readObject());
            }
            clientSocket.close();

            if (message == null || clientSocket.isClosed()) continue;

            try (PrintStream out = new PrintStream(
              new BufferedOutputStream(clientSocket.getOutputStream()), true, StandardCharsets.UTF_8)) {

              out.println(message);
            }
          }
          catch (IOException e) {
            ErrorDialog.showDetails(
              e,
              ThrowableUtils.getStackTrace(e),
              "Error.socket_error" //NON-NLS
            );
          }
          catch (ClassNotFoundException e) {
            ErrorDialog.bug(e);
          }
          finally {
            if (clientSocket != null) {
              try {
                clientSocket.close();
              }
              catch (IOException e) {
                logger.error("Error while closing client socket", e); //NON-NLS
              }
            }
          }
        }
      }
      finally {
        if (serverSocket != null) {
          try {
            serverSocket.close();
          }
          catch (IOException e) {
            logger.error("Error while closing server socket", e); //NON-NLS
          }
        }
      }
    }
  }

  protected void launch() {
    logger.info("Manager"); //NON-NLS
    final ModuleManagerWindow window = ModuleManagerWindow.getInstance();
    window.setVisible(true);

    final boolean isFirstTime = !Info.getPrefsDir().exists();

    if (isFirstTime) new FirstTimeDialog(window).setVisible(true);
  }

  protected String execute(Object req) {
    if (req instanceof LaunchRequest) {
      final LaunchRequest lr = (LaunchRequest) req;

      if (lr.key != key) {
// FIXME: translate
        return "incorrect key"; //NON-NLS
      }

      final LaunchRequestHandler handler = new LaunchRequestHandler(lr);
      try {
        SwingUtilities.invokeAndWait(handler);
      }
      catch (InterruptedException e) {
        return "interrupted";   // FIXME //NON-NLS
      }
      catch (InvocationTargetException e) {
        ErrorDialog.bug(e);
        return null;
      }

      return handler.getResult();
    }
    else {
      return "unrecognized command";  // FIXME //NON-NLS
    }
  }

  private static class LaunchRequestHandler implements Runnable {
    private final LaunchRequest lr;
    private String result;

    public LaunchRequestHandler(LaunchRequest lr) {
      this.lr = lr;
    }

    @Override
    public void run() {
      result = handle();
    }

    public String getResult() {
      return result;
    }

    private String handle() {
      final ModuleManagerWindow window = ModuleManagerWindow.getInstance();

      switch (lr.mode) {
      case MANAGE:
        window.toFront();
        break;
      case LOAD:
        if (Player.LaunchAction.isEditing(lr.module))
          return "module open for editing";   // FIXME //NON-NLS

        if (lr.module == null && lr.game != null) {
          // attempt to find the module for the saved game or log
          final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.game);
          if (data instanceof SaveMetaData) {
            // we found save metadata
            final String moduleName = ((SaveMetaData) data).getModuleName();
            if (moduleName != null && moduleName.length() > 0) {
              // get the module file by module name
              lr.module = window.getModuleByName(moduleName);
            }
            else {
              // this is a pre 3.1 save file, can't tell the module name
// FIXME: show some error here
              return "cannot find module"; //NON-NLS
            }
          }
        }

        if (lr.module == null) {
          return "cannot find module"; //NON-NLS
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
          return "module open for play";      // FIXME //NON-NLS
        if (Editor.LaunchAction.isEditing(lr.module))
          return "module open for editing";   // FIXME //NON-NLS

        new Editor.LaunchAction(window, lr.module).actionPerformed(null);
        break;
      case IMPORT:
        new Editor.ImportLaunchAction(
          window, lr.importFile).actionPerformed(null);
        break;
      case NEW:
        new Editor.NewModuleLaunchAction(window).actionPerformed(null);
        break;
      case EDIT_EXT:
        return "not yet implemented";   // FIXME //NON-NLS
      case NEW_EXT:
        return "not yet implemented";   // FIXME //NON-NLS
      default:
        return "unrecognized mode";     // FIXME //NON-NLS
      }

      return null;
    }
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
