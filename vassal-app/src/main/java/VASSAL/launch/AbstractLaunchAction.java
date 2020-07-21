/*
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.ZipFile;

import javax.swing.AbstractAction;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.build.module.ExtensionsManager;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.ReadOnlyPrefs;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.concurrent.FutureUtils;
import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ModuleFileFilter;
import VASSAL.tools.io.ProcessLauncher;
import VASSAL.tools.io.ProcessWrapper;
import VASSAL.tools.ipc.IPCMessage;
import VASSAL.tools.ipc.IPCMessenger;
import VASSAL.tools.ipc.SimpleIPCMessage;
import VASSAL.tools.lang.MemoryUtils;

/**
 *
 * The base class for {@link javax.swing.Action}s which launch processes from the
 * {@link ModuleManagerWindow}.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class AbstractLaunchAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private static final Logger logger =
    LoggerFactory.getLogger(AbstractLaunchAction.class);

  //
  // memory-related constants
  //
  protected static final int PHYS_MEMORY;
  protected static final int DEFAULT_INITIAL_HEAP = 256;
  protected static final int DEFAULT_MAXIMUM_HEAP = 512;
  protected static final int FAILSAFE_INITIAL_HEAP = 64;
  protected static final int FAILSAFE_MAXIMUM_HEAP = 128;

  static {
    // Determine how much physical RAM this machine has
    // Assume 4GB if we can't determine how much RAM there is
    final long physMemoryBytes = MemoryUtils.getPhysicalMemory();
    PHYS_MEMORY = physMemoryBytes < 0 ? 4096 : (int)(physMemoryBytes >> 20);
  }

  protected final Window window;
  protected final String entryPoint;
  protected final LaunchRequest lr;

  protected static final Set<File> editing =
    Collections.synchronizedSet(new HashSet<>());
  protected static final Map<File,Integer> using =
    Collections.synchronizedMap(new HashMap<>());

/*
  protected static final List<ObjectOutputStream> children =
    Collections.synchronizedList(new ArrayList<ObjectOutputStream>());
*/

  protected static final List<IPCMessenger> children =
    Collections.synchronizedList(new ArrayList<>());

  protected static final AtomicInteger nextId = new AtomicInteger(1);

  public AbstractLaunchAction(String name, Window window,
                              String entryPoint, LaunchRequest lr) {
    super(name);

    this.window = window;
    this.entryPoint = entryPoint;
    this.lr = lr;
  }

  /**
   * @param file the file to check
   * @return <code>true</code> iff the file is in use
   */
  public static boolean isInUse(File file) {
    return using.containsKey(file);
  }

  /**
   * @param file the file to check
   * @return <code>true</code> iff the file is being edited
   */
  public static boolean isEditing(File file) {
    return editing.contains(file);
  }

  /**
   * Ask child processes to close.
   *
   * @return <code>true</code> iff all child processes will terminate
   */
  public static boolean shutDown() {
    ModuleManagerWindow.getInstance().toBack();

    final List<Future<IPCMessage>> futures = new ArrayList<>();

    // must synchronize when iterating over a Collections.synchronizedList()
    synchronized (children) {
      for (IPCMessenger ipc : children) {
        try {
          futures.add(ipc.send(new Launcher.CloseRequest()));
        }
        catch (IOException e) {
          // FIXME
          e.printStackTrace();
        }
      }
    }

    // FIXME: not working!
    for (Future<IPCMessage> f : futures) {
      try {
        if (f.get() instanceof Launcher.CloseReject) {
          System.out.println("rejected!");
          return false;
        }
      }
      catch (ExecutionException | InterruptedException e) {
        e.printStackTrace();
      }
    }

    return true;
  }

  /** {@inheritDoc} */
  @Override
  public void actionPerformed(ActionEvent e) {
    ModuleManagerWindow.getInstance().setWaitCursor(true);
    getLaunchTask().execute();
  }

  protected abstract LaunchTask getLaunchTask();

  protected File promptForFile() {
    // prompt the user to pick a file
    final FileChooser fc = FileChooser.createFileChooser(window,
      (DirectoryConfigurer)
        Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));

    addFileFilters(fc);

    // loop until cancellation or we get an existing file
    if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
      lr.module = fc.getSelectedFile();
      if (lr.module != null) {
        if (lr.module.exists()) {
          final AbstractMetaData metadata =
            MetaDataFactory.buildMetaData(lr.module);
          if (metadata == null || ! (metadata instanceof ModuleMetaData)) {
            ErrorDialog.show(
              "Error.invalid_vassal_module", lr.module.getAbsolutePath());
            logger.error(
              "-- Load of {} failed: Not a Vassal module",
              lr.module.getAbsolutePath()
            );
            lr.module = null;
          }
        }
        else {
          lr.module = null;
        }
// FIXME: do something to warn about nonexistant file
//        FileNotFoundDialog.warning(window, lr.module);
      }
    }

    return lr.module;
  }

  protected void addFileFilters(FileChooser fc) {
    fc.addChoosableFileFilter(new ModuleFileFilter());
  }

  protected class LaunchTask extends SwingWorker<Void,Void> {
    protected final int id = nextId.getAndIncrement();

    // lr might be modified before the task is over, keep a local copy
    protected final LaunchRequest lr =
      new LaunchRequest(AbstractLaunchAction.this.lr);

    protected ServerSocket serverSocket = null;
    protected Socket clientSocket = null;

    protected IPCMessenger ipc = null;

    @Override
    public Void doInBackground() throws InterruptedException,
                                        IOException {
// FIXME: this should be in an abstract method and farmed out to subclasses
      // send some basic information to the log
      if (lr.module != null) {
        logger.info("Loading module file {}", lr.module.getAbsolutePath());

        // slice tiles for module
        final String aname = lr.module.getAbsolutePath();
        final ModuleMetaData meta = new ModuleMetaData(new ZipFile(aname));
        final String hstr =
          DigestUtils.sha1Hex(meta.getName() + "_" + meta.getVersion());

        final File cdir = new File(Info.getConfDir(), "tiles/" + hstr);

        final TilingHandler th = new TilingHandler(
          aname,
          cdir,
          new Dimension(256, 256),
          PHYS_MEMORY,
          nextId.getAndIncrement()
        );

        try {
          th.sliceTiles();
        }
        catch (CancellationException e) {
          cancel(true);
          return null;
        }

        // slice tiles for extensions
        final ExtensionsManager mgr = new ExtensionsManager(lr.module);
        for (File ext : mgr.getActiveExtensions()) {
          final TilingHandler eth = new TilingHandler(
            ext.getAbsolutePath(),
            cdir,
            new Dimension(256, 256),
            PHYS_MEMORY,
            nextId.getAndIncrement()
          );

          try {
            eth.sliceTiles();
          }
          catch (CancellationException e) {
            cancel(true);
            return null;
          }
        }
      }

      if (lr.game != null) {
        logger.info("Loading game file {}", lr.game.getAbsolutePath());
      }

      if (lr.importFile != null) {
        logger.info(
          "Importing module file {}",
          lr.importFile.getAbsolutePath()
        );
      }
// end FIXME

      // set default heap sizes
      int initialHeap = DEFAULT_INITIAL_HEAP;
      int maximumHeap = DEFAULT_MAXIMUM_HEAP;

      String moduleName = null;

// FIXME: this should be in an abstract method and farmed out to subclasses,
// rather than a case structure for each kind of thing which may be loaded.
      // find module-specific heap settings, if any
      if (lr.module != null) {
        final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.module);

        if (data == null) {
          ErrorDialog.show(
            "Error.invalid_vassal_file", lr.module.getAbsolutePath());
          ModuleManagerWindow.getInstance().setWaitCursor(false);
          return null;
        }

        if (data instanceof ModuleMetaData) {
          moduleName = ((ModuleMetaData) data).getName();

          // log the module name
          logger.info("Loading module {}", moduleName);

          // read module prefs
          final ReadOnlyPrefs p = new ReadOnlyPrefs(moduleName);

          // read initial heap size
          initialHeap = getHeapSize(
            p, GlobalOptions.INITIAL_HEAP, DEFAULT_INITIAL_HEAP);

          // read maximum heap size
          maximumHeap = getHeapSize(
            p, GlobalOptions.MAXIMUM_HEAP, DEFAULT_MAXIMUM_HEAP);
        }
      }
      else if (lr.importFile != null) {
        final Prefs p = Prefs.getGlobalPrefs();

        // read initial heap size
        initialHeap = getHeapSize(
          p, GlobalOptions.INITIAL_HEAP, DEFAULT_INITIAL_HEAP);

        // read maximum heap size
        maximumHeap = getHeapSize(
          p, GlobalOptions.MAXIMUM_HEAP, DEFAULT_MAXIMUM_HEAP);
      }
// end FIXME

      //
      // Heap size sanity checks: fall back to failsafe heap sizes in
      // case the given initial or maximum heap is not usable.
      //

// FIXME: The heap size messages are too nonspecific. They should
// differientiate between loading a module and importing a module,
// since the heap sizes are set in different places for those two
// actions.
      // maximum heap must fit in physical RAM
      if (maximumHeap > PHYS_MEMORY) {
        initialHeap = FAILSAFE_INITIAL_HEAP;
        maximumHeap = FAILSAFE_MAXIMUM_HEAP;

        FutureUtils.wait(WarningDialog.show(
          "Warning.maximum_heap_too_large",
          FAILSAFE_MAXIMUM_HEAP
        ));
      }
      // maximum heap must be at least the failsafe size
      else if (maximumHeap < FAILSAFE_MAXIMUM_HEAP) {
        initialHeap = FAILSAFE_INITIAL_HEAP;
        maximumHeap = FAILSAFE_MAXIMUM_HEAP;

        FutureUtils.wait(WarningDialog.show(
          "Warning.maximum_heap_too_small",
          FAILSAFE_MAXIMUM_HEAP
        ));
      }
      // initial heap must be at least the failsafe size
      else if (initialHeap < FAILSAFE_INITIAL_HEAP) {
        initialHeap = FAILSAFE_INITIAL_HEAP;
        maximumHeap = FAILSAFE_MAXIMUM_HEAP;

        FutureUtils.wait(WarningDialog.show(
          "Warning.initial_heap_too_small",
          FAILSAFE_INITIAL_HEAP
        ));
      }
      // initial heap must be less than or equal to maximum heap
      else if (initialHeap > maximumHeap) {
        initialHeap = FAILSAFE_INITIAL_HEAP;
        maximumHeap = FAILSAFE_MAXIMUM_HEAP;

        FutureUtils.wait(WarningDialog.show(
          "Warning.initial_heap_too_large",
          FAILSAFE_INITIAL_HEAP
        ));
      }

      // create a socket for communicating which the child process
      final InetAddress lo = InetAddress.getByName(null);
      serverSocket = new ServerSocket(0, 0, lo);

      final int port = serverSocket.getLocalPort();

      // build the argument list
      final ArrayList<String> al = new ArrayList<>();
      al.add(Info.javaBinPath);
      al.add("");   // reserved for initial heap
      al.add("");   // reserved for maximum heap
      al.add("-DVASSAL.id=" + id);  // instance id
      al.add("-DVASSAL.port=" + port); // MM socket port

      // pass on the user's home, if it's set
      final String userHome = System.getProperty("user.home");
      if (userHome != null) al.add("-Duser.home=" + userHome);

      // pass on the user's working dir, if it's set
      final String userDir = System.getProperty("user.dir");
      if (userDir != null) al.add("-Duser.dir=" + userDir);

      // pass on VASSAL's home dir, if it's set
      final String vHome = System.getProperty("VASSAL.home");
      if (vHome != null) al.add("-DVASSAL.home=" + vHome);

      // set the classpath
      al.add("-cp");
      al.add(System.getProperty("java.class.path"));

      if (SystemUtils.IS_OS_MAC_OSX) {
        // set the MacOS X dock parameters

        // use the module name for the dock if we found a module name
// FIXME: should "Unnamed module" be localized?
        final String d_name = moduleName != null && moduleName.length() > 0
          ? moduleName : "Unnamed module";

        // get the path to the app icon
        final String d_icon = new File(Info.getBaseDir(),
          "Contents/Resources/VASSAL.icns").getAbsolutePath();

        al.add("-Xdock:name=" + d_name);
        al.add("-Xdock:icon=" + d_icon);
      }
      else if (SystemUtils.IS_OS_WINDOWS) {
        // Disable the 2D to Direct3D pipeline?
        final Boolean disableD3d =
          (Boolean) Prefs.getGlobalPrefs().getValue(Prefs.DISABLE_D3D);
        if (Boolean.TRUE.equals(disableD3d)) {
          al.add("-Dsun.java2d.d3d=false");
        }
      }

      al.add(entryPoint);

      al.addAll(Arrays.asList(lr.toArgs()));

      final String[] args = al.toArray(new String[0]);

      // try to start a child process with the given heap sizes
      args[1] = "-Xms" + initialHeap + "M";
      args[2] = "-Xmx" + maximumHeap + "M";


      ProcessWrapper proc = new ProcessLauncher().launch(args);

      try {
        proc.future.get(1000L, TimeUnit.MILLISECONDS);
      }
      catch (CancellationException e) {
        cancel(true);
        return null;
      }
      catch (ExecutionException e) {
        logger.error("", e);
      }
      catch (TimeoutException e) {
        // this is expected
      }

      // if launch failed, use conservative heap sizes
      if (proc.future.isDone()) {
        args[1] = "-Xms" + FAILSAFE_INITIAL_HEAP + "M";
        args[2] = "-Xmx" + FAILSAFE_MAXIMUM_HEAP + "M";
        proc = new ProcessLauncher().launch(args);

        try {
          proc.future.get(1000L, TimeUnit.MILLISECONDS);
        }
        catch (ExecutionException e) {
          logger.error("", e);
        }
        catch (TimeoutException e) {
          // this is expected
        }

        if (proc.future.isDone()) {
          throw new IOException("failed to start child process");
        }
        else {
          FutureUtils.wait(WarningDialog.show(
            "Warning.maximum_heap_too_large",
            FAILSAFE_MAXIMUM_HEAP
          ));
        }
      }

      clientSocket = serverSocket.accept();
      ipc = new IPCMessenger(clientSocket);

      ipc.addEventListener(
        NotifyOpenModuleOk.class,
        new NotifyOpenModuleOkListener()
      );

      ipc.addEventListener(
        NotifyNewModuleOk.class,
        new NotifyNewModuleOkListener()
      );

      ipc.addEventListener(
        NotifyImportModuleOk.class,
        new NotifyImportModuleOkListener()
      );

      ipc.addEventListener(
        NotifyOpenModuleFailed.class,
        new NotifyOpenModuleFailedListener()
      );

      ipc.addEventListener(
        NotifySaveFileOk.class,
        new NotifySaveFileOkListener()
      );

      ipc.start();

      children.add(ipc);

      // block until the process ends
      try {
        proc.future.get();
      }
      catch (ExecutionException e) {
        logger.error("", e);
      }

      return null;
    }

    protected int getHeapSize(ReadOnlyPrefs p, String key, int defaultHeap) {
      // read heap size, if it exists
      final String val = p.getStoredValue(key);
      if (val == null) return defaultHeap;

      try {
        return Integer.parseInt(val);
      }
      catch (NumberFormatException ex) {
        return -1;
      }
    }

    protected int getHeapSize(Prefs p, String key, int defaultHeap) {
      // read heap size, if it exists
      final Object val = p.getValue(key);
      if (val == null) return defaultHeap;

      try {
        return Integer.parseInt(val.toString());
      }
      catch (NumberFormatException ex) {
        return -1;
      }
    }

    @Override
    protected void done() {
      try {
        get();
      }
      catch (CancellationException e) {
        // this means that loading was cancelled
        ModuleManagerWindow.getInstance().setWaitCursor(false);
      }
      catch (InterruptedException e) {
        ErrorDialog.bug(e);
      }
      catch (ExecutionException e) {
        // determine what kind of exception occurred
        final Throwable c = e.getCause();
        if (c instanceof IOException) {
          ErrorDialog.showDetails(
            e,
            ThrowableUtils.getStackTrace(e),
            "Error.socket_error"
          );
        }
        else {
          ErrorDialog.bug(e);
        }
      }
      finally {
        if (clientSocket != null) {
          try {
            clientSocket.close();
          }
          catch (IOException e) {
            logger.error("Error while closing clientSocket", e);
          }
        }

        if (serverSocket != null) {
          try {
            serverSocket.close();
          }
          catch (IOException e) {
            logger.error("Error while closing serverSocket", e);
          }
        }
        children.remove(ipc);
      }
    }
  }

  //
  // Commands
  //

  protected abstract static class LaunchRequestMessage
                                                     extends SimpleIPCMessage {
    private static final long serialVersionUID = 1L;

    protected final LaunchRequest lr;

    public LaunchRequestMessage(LaunchRequest lr) {
      this.lr = lr;
    }
  }

  public static class NotifyOpenModuleOk extends LaunchRequestMessage {
    private static final long serialVersionUID = 1L;

    public NotifyOpenModuleOk(LaunchRequest lr) {
      super(lr);
    }
  }

  public static class NotifyNewModuleOk extends LaunchRequestMessage {
    private static final long serialVersionUID = 1L;

    public NotifyNewModuleOk(LaunchRequest lr) {
      super(lr);
    }
  }

  public static class NotifyImportModuleOk extends LaunchRequestMessage {
    private static final long serialVersionUID = 1L;

    public NotifyImportModuleOk(LaunchRequest lr) {
      super(lr);
    }
  }

  public static class NotifyOpenModuleFailed extends LaunchRequestMessage {
    private static final long serialVersionUID = 1L;

    public final Throwable thrown;

    public NotifyOpenModuleFailed(LaunchRequest lr, Throwable thrown) {
      super(lr);
      this.thrown = thrown;
    }
  }

  public static class NotifySaveFileOk extends SimpleIPCMessage {
    private static final long serialVersionUID = 1L;

    public final File file;

    public NotifySaveFileOk(File file) {
      this.file = file;
    }
  }

  //
  // Listeners
  //

  protected static class NotifyOpenModuleOkListener
                                 implements EventListener<NotifyOpenModuleOk> {
    @Override
    public void receive(Object src, final NotifyOpenModuleOk msg) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          final ModuleManagerWindow mmw = ModuleManagerWindow.getInstance();
          mmw.addModule(msg.lr.module);
          mmw.setWaitCursor(false);
        }
      });
    }
  }

  protected static class NotifyNewModuleOkListener
                                 implements EventListener<NotifyNewModuleOk> {
    @Override
    public void receive(Object src, NotifyNewModuleOk msg) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          ModuleManagerWindow.getInstance().setWaitCursor(false);
        }
      });
    }
  }

  protected static class NotifyImportModuleOkListener
                               implements EventListener<NotifyImportModuleOk> {
    @Override
    public void receive(Object src, NotifyImportModuleOk msg) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          ModuleManagerWindow.getInstance().setWaitCursor(false);
        }
      });
    }
  }

  protected static class NotifyOpenModuleFailedListener
                             implements EventListener<NotifyOpenModuleFailed> {
    @Override
    public void receive(Object src, NotifyOpenModuleFailed msg) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          ModuleManagerWindow.getInstance().setWaitCursor(false);
        }
      });

      ErrorDialog.showDetails(
        msg.thrown,
        ThrowableUtils.getStackTrace(msg.thrown),
        "Error.module_load_failed",
        msg.thrown.getMessage()
      );
    }
  }

  protected static class NotifySaveFileOkListener
                                   implements EventListener<NotifySaveFileOk> {
    @Override
    public void receive(Object rc, final NotifySaveFileOk msg) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          ModuleManagerWindow.getInstance().update(msg.file);
        }
      });
    }
  }
}
