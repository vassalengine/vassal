/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman 
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

import java.awt.Cursor;
import java.awt.Window;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import javax.swing.AbstractAction;

// FIXME: switch back to javax.swing.SwingWorker on move to Java 1.6
//import javax.swing.SwingWorker;
import org.jdesktop.swingworker.SwingWorker;

import VASSAL.Info;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.ReadOnlyPrefs;
import VASSAL.tools.CommunicationErrorDialog;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.HeapFinder;
import VASSAL.tools.StringUtils;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ModuleFileFilter;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.logging.Logger;
import VASSAL.tools.logging.LogEntry;
import VASSAL.tools.logging.LogManager;

/**
 * 
 * The base class for {@link Action}s which launch processes from the
 * {@link ModuleManagerWindow}.
 *
 * @author Joel Uckelman
 * @since 3.1.0 
 */
public abstract class AbstractLaunchAction extends AbstractAction {
  private static final long serialVersionUID = 1L;
  
  public static final int DEFAULT_INITIAL_HEAP = 256;
  public static final int DEFAULT_MAXIMUM_HEAP = 512;

  protected final Window window; 
  protected final String entryPoint;
  protected final LaunchRequest lr;

  protected static final Set<File> editing =
    Collections.synchronizedSet(new HashSet<File>());
  protected static final Map<File,Integer> using =
    Collections.synchronizedMap(new HashMap<File,Integer>());

  protected static final List<CommandClient> children =
    Collections.synchronizedList(new ArrayList<CommandClient>());

  
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

    // must synchronize when iterating over a Collections.synchronizedList()
    synchronized (children) {
      for (CommandClient child : children) {
        try {
          if ("NOK".equals(child.request("REQUEST_CLOSE"))) return false;
        }
        catch (EOFException ignore) {
          // Normal. Child closed.
        }
        catch (SocketException ignore) {
          // Normal. Child closed.
        }
        catch (IOException e) {
          CommunicationErrorDialog.error(e);
        }
      }
    }

    return true;
  }

  /** {@inheritDoc} */  
  public void actionPerformed(ActionEvent e) {
    setWaitCursor(true);
    getLaunchTask().execute();
  }

  protected void setWaitCursor(boolean wait) {
    window.setCursor(Cursor.getPredefinedCursor(
      wait ? Cursor.WAIT_CURSOR : Cursor.DEFAULT_CURSOR
    ));
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
            Logger.log("-- Load of " + lr.module.getAbsolutePath() +
                       " failed: Not a Vassal module");
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
    // lr might be modified before the task is over, keep a local copy
    protected final LaunchRequest lr =
      new LaunchRequest(AbstractLaunchAction.this.lr); 

    protected ServerSocket serverSocket = null;
    protected Socket clientSocket = null;
  
    protected CommandClient cmdC = null;
    protected CommandServer cmdS = null;

    @Override
    public Void doInBackground() throws InterruptedException, IOException {
// FIXME: this should be in an abstract method and farmed out to subclasses
      // send some basic information to the log
      if (lr.module != null) {
        Logger.log("-- Loading module file " + lr.module.getAbsolutePath());
      }

      if (lr.game != null) {
        Logger.log("-- Loading game file " + lr.game.getAbsolutePath());
      }

      if (lr.importFile != null) {
        Logger.log("-- Importing module file " +
          lr.importFile.getAbsolutePath());
      }
// end FIXME


      // set default heap setttings
      int initialHeap = DEFAULT_INITIAL_HEAP;
      int maximumHeap = DEFAULT_MAXIMUM_HEAP;

      String moduleName = null;

// FIXME: this should be in an abstract method and farmed out to subclasses
      // find module-specific heap settings, if any
      if (lr.module != null) {
        final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.module);
        
        if (data == null) {
          ErrorDialog.show(
            "Error.invalid_vassal_file", lr.module.getAbsolutePath());
          setWaitCursor(false);
          return null;
        }
        
        if (data instanceof ModuleMetaData) {
          moduleName = ((ModuleMetaData) data).getName();

          // log the module name
          Logger.log("-- Loading module " + moduleName);

          // read module prefs
          final ReadOnlyPrefs p = new ReadOnlyPrefs(moduleName);

          // read initial heap size, if it exists
          final String iheap = p.getStoredValue(GlobalOptions.INITIAL_HEAP);
          if (iheap != null) {
            try {
              initialHeap = Integer.parseInt(iheap.toString());
              if (initialHeap <= 0) throw new NumberFormatException();
            }
            catch (NumberFormatException ex) {
              WarningDialog.show(
                "Warning.bad_initial_heap", DEFAULT_INITIAL_HEAP);
 
              initialHeap = DEFAULT_INITIAL_HEAP;
            }
          }

          // read maximum heap size, if it exists
          final String mheap = p.getStoredValue(GlobalOptions.MAXIMUM_HEAP);
          if (mheap != null) {
            try {
              maximumHeap = Integer.parseInt(mheap.toString());
              if (maximumHeap <= 0) throw new NumberFormatException();
            }
            catch (NumberFormatException ex) {
              WarningDialog.show(
                "Warning.bad_maximum_heap", DEFAULT_MAXIMUM_HEAP);

              maximumHeap = DEFAULT_MAXIMUM_HEAP;
            }
          }
        }
      }
      else if (lr.importFile != null) {
        final Prefs p = Prefs.getGlobalPrefs();

        // read initial heap size, if it exists
        final Object iheap = p.getValue(GlobalOptions.INITIAL_HEAP);
        if (iheap != null) {
          try {
            initialHeap = Integer.parseInt(iheap.toString());
            if (initialHeap <= 0) throw new NumberFormatException();
          }
          catch (NumberFormatException ex) {
            WarningDialog.show(
              "Warning.bad_initial_heap", DEFAULT_INITIAL_HEAP);
 
            initialHeap = DEFAULT_INITIAL_HEAP;
          }
        }

        // read maximum heap size, if it exists
        final Object mheap = p.getStoredValue(GlobalOptions.MAXIMUM_HEAP);
        if (mheap != null) {
          try {
            maximumHeap = Integer.parseInt(mheap.toString());
            if (maximumHeap <= 0) throw new NumberFormatException();
          }
          catch (NumberFormatException ex) {
            WarningDialog.show(
              "Warning.bad_maximum_heap", DEFAULT_MAXIMUM_HEAP);

            maximumHeap = DEFAULT_MAXIMUM_HEAP;
          }
        }
      }
// end FIXME

      // make sure that the initial heap is sane
      if (initialHeap > maximumHeap) {
        WarningDialog.show("Warning.initial_gt_maximum_heap",
           DEFAULT_INITIAL_HEAP, DEFAULT_MAXIMUM_HEAP
        );

        initialHeap = maximumHeap / 2;
      }

      // create a socket for communicating which the child process
      serverSocket = new ServerSocket(0);
      cmdS = new LaunchCommandServer(serverSocket);
      new Thread(cmdS).start();

      // build the argument list
      final ArrayList<String> al = new ArrayList<String>();
      al.add(Info.javaBinPath);
      al.add("");   // reserved for initial heap 
      al.add("");   // reserved for maximum heap 
      al.add("-cp");
      al.add(System.getProperty("java.class.path"));

      if (Info.isMacOSX()) {
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
      else if (Info.isWindows()) {
        // Disable the 2D to Direct3D pipeline?
        final Boolean disableD3d =
          (Boolean) Prefs.getGlobalPrefs().getValue(Prefs.DISABLE_D3D);
        if (Boolean.TRUE.equals(disableD3d)) {
          al.add("-Dsun.java2d.d3d=false");
        }
      }
      
      al.add(entryPoint);

      final String[] args = al.toArray(new String[al.size()]);
     
      Process p = null;
      boolean maxHeapWarning = false;
      while (true) {
        args[1] = "-Xms" + initialHeap + "M";
        args[2] = "-Xmx" + maximumHeap + "M";

        Logger.log(StringUtils.join(" ", args));

        // set up and start the child process
        final ProcessBuilder pb = new ProcessBuilder(args);
        pb.directory(Info.getBinDir());
        p = pb.start();

        // write the port for this socket to child's stdin and close
        ObjectOutputStream oout = null;
        try {
          oout = new ObjectOutputStream(p.getOutputStream());
          oout.writeInt(serverSocket.getLocalPort());
          oout.writeObject(lr);
          oout.close();
        }
        finally {
          IOUtils.closeQuietly(oout);
        }

// FIXME: this is probably too long
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
        }

        // check whether the child is still alive
        try {
          // If this doesn't throw, our baby is dead.
          p.exitValue();
          Logger.log(IOUtils.toString(p.getErrorStream()));

          // Try to determine heuristically what the max max heap is.
          final int maxMaximumHeap = HeapFinder.getMaxMaxHeap();
          if (maximumHeap < maxMaximumHeap) {
            // Either we're on a 64-bit machine, so we can't have
            // an overlarge maximum heap size problem, or something
            // else weird happened. We give up.
            throw new IllegalStateException();
          }
          else {
            // We had an infeasibly large maximum heap setting, try again
            // with something smaller and warn the user to lower it.
            maximumHeap = (int) (0.75 * maxMaximumHeap);
            initialHeap = Math.min(DEFAULT_INITIAL_HEAP, maximumHeap);
            maxHeapWarning = true;
            continue;
          }
        }
        catch (IllegalThreadStateException e) {
          // It's alive! It's ALIIIIIIVE!!!
          break;
        }
      }

      // read the port for the child's socket from its stdout
      final DataInputStream din = new DataInputStream(p.getInputStream());
      final int childPort = din.readInt();

      // pump child's stderr to our own stderr
      new Thread(new StreamPump(p.getErrorStream(), System.err)).start();

      // pump child's stdout to our own stdout
      new Thread(new StreamPump(p.getInputStream(), System.out)).start();

      // Check that the child's port is sane. Reading stdout from a
      // failed launch tends to give impossible port numbers.
      if (childPort < 0 || childPort > 65535) {
        throw new IOException("port out of range: " + childPort);
      }

      // create the client for the child's socket
      clientSocket = new Socket((String) null, childPort);
      cmdC = new CommandClient(clientSocket);
      children.add(cmdC);

      if (maxHeapWarning) {
        WarningDialog.show("Error.set_lower_maximum_heap");
      }

      // block until the process ends
      p.waitFor();
      return null;
    }

    @Override
    protected void done() {
      try {
        get();
      }
      catch (CancellationException e) {
        // FIXME: bug until we enable cancellation of loading
        ErrorDialog.bug(e);
      }
      catch (InterruptedException e) {
        ErrorDialog.bug(e);
      }
      catch (ExecutionException e) {
        // determine what kind of exception occurred
        final Throwable c = e.getCause();
        if (c instanceof IOException) {
          CommunicationErrorDialog.error(e, (IOException) c);
        }
        else {
          ErrorDialog.bug(e);
        }
      }
      finally {
        IOUtils.closeQuietly(clientSocket);
        IOUtils.closeQuietly(serverSocket);
        children.remove(cmdC);
      }    
    }
  }

  protected class LaunchCommandServer extends CommandServer {
    public LaunchCommandServer(ServerSocket serverSocket) {
      super(serverSocket);
    }

    @Override
    protected Object reply(Object cmd) {
      if (cmd instanceof LogEntry) {
        LogManager.enqueue((LogEntry) cmd);
        return "OK";
      }
      else if (cmd instanceof Launcher.SaveFileCmd) {
        return ModuleManagerWindow.getInstance().update(
          ((Launcher.SaveFileCmd) cmd).getFile());
      }
      else if ("NOTIFY_OPEN_OK".equals(cmd)) {
        ModuleManagerWindow.getInstance().addModule(lr.module);
        setWaitCursor(false);
        return "OK";
      }
      else if ("NOTIFY_NEW_OK".equals(cmd)) {
        setWaitCursor(false);
        return "OK";
      }
      else if ("NOTIFY_IMPORT_OK".equals(cmd)) {
        setWaitCursor(false);
        return "OK";
      }
      else if (cmd instanceof Launcher.LoadFailedCmd) {
        setWaitCursor(false);

        final Throwable thrown = ((Launcher.LoadFailedCmd) cmd).getThrowable();

        ErrorDialog.showDetails(
          thrown,
          ThrowableUtils.getStackTrace(thrown),
          "Error.module_load_failed",
          thrown.getMessage()
        );

        return "OK";
      }
      else {
        return "UNRECOGNIZED_COMMAND";
      }
    }
  }

  private static class StreamPump implements Runnable {
    private final InputStream in;
    private final OutputStream out;

    public StreamPump(InputStream in, OutputStream out) {
      this.in = in;
      this.out = out;
    }

    public void run() {
      try {
        IOUtils.copy(in, out);
      }
      catch (IOException e) {
        CommunicationErrorDialog.error(e);
      }
    }
  }
}
