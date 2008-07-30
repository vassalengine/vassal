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
import VASSAL.build.module.AbstractMetaData;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.ModuleMetaData;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.ReadOnlyPrefs;
import VASSAL.tools.CommunicationErrorDialog;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.IOUtils;
import VASSAL.tools.WinRegUtils;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ModuleFileFilter;

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
    for (CommandClient child : children) {
      try {
        if ("NOK".equals(child.request("REQUEST_CLOSE"))) return false;
      }
      catch (EOFException e) {
        // Normal. Child closed.
      }
      catch (IOException e) {
        CommunicationErrorDialog.error(e);
      }
    }
    return true;
  }

  /** {@inheritDoc} */  
  public void actionPerformed(ActionEvent e) {
    window.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
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
    while (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
      lr.module = fc.getSelectedFile();
      if (lr.module != null) {
        if (lr.module.exists()) break;
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
      // set default heap setttings
      int initialHeap = DEFAULT_INITIAL_HEAP;
      int maximumHeap = DEFAULT_MAXIMUM_HEAP;

      String moduleName = null;

      // find module-specific heap settings, if any
      if (lr.module != null) {
        final AbstractMetaData data = AbstractMetaData.buildMetaData(lr.module);
        if (data != null && data instanceof ModuleMetaData) {
          moduleName = ((ModuleMetaData) data).getName();

          // read module prefs
          final ReadOnlyPrefs p = new ReadOnlyPrefs(moduleName);

          // read initial heap size, if it exists
          final String iheap = p.getStoredValue(GlobalOptions.INITIAL_HEAP);
          if (iheap != null) {
            try {
              initialHeap = Integer.parseInt(iheap);
            }
            catch (NumberFormatException ex) {
              ErrorDialog.warning(
                Resources.getString("Error.bad_initial_heap"),
                Resources.getString("Error.bad_initial_heap"),
                Resources.getString("Error.bad_initial_heap_message",
                  DEFAULT_INITIAL_HEAP)
              );
 
              initialHeap = DEFAULT_INITIAL_HEAP;
            }
          }

          // read maximum heap size, if it exists
          final String mheap = p.getStoredValue(GlobalOptions.MAXIMUM_HEAP);
          if (mheap != null) {
            try {
              maximumHeap = Integer.parseInt(mheap);
            }
            catch (NumberFormatException ex) {
              ErrorDialog.warning(
                Resources.getString("Error.bad_maximum_heap"),
                Resources.getString("Error.bad_maximum_heap"),
                Resources.getString("Error.bad_maximum_heap_message",
                  DEFAULT_MAXIMUM_HEAP)
              );

              maximumHeap = DEFAULT_MAXIMUM_HEAP;
            }
          }
        }
      }

      // create a socket for communicating which the child process
      serverSocket = new ServerSocket(0);
      cmdS = new LaunchCommandServer(serverSocket);
      new Thread(cmdS).start();

      // build the child process
      final ArrayList<String> al = new ArrayList<String>();
      
      if (Info.isWindows()) {
        // check the registry to find java.exe
        final String java = WinRegUtils.getJavaPath();
        al.add(java == null ? "java" : java);
      }
      else {
        al.add("java");
      }

      al.add("-Xms" + initialHeap + "M");
      al.add("-Xmx" + maximumHeap + "M");
      al.add("-cp");
      al.add(System.getProperty("java.class.path"));

      if (Info.isMacOSX()) {
        // use the module name for the dock if we found a module name
        al.add("-Xdock:name=" + 
          (moduleName != null && moduleName.length() > 0 ? moduleName :
// FIXME: should be localized?
          "Unnamed module"));  
//        al.add("-Xdock:icon=" + );
      }

      al.add(entryPoint);

      final String[] args = al.toArray(new String[al.size()]);

      final ProcessBuilder pb = new ProcessBuilder(args);
      pb.directory(Info.getBinDir());

      final Process p = pb.start();

      // pump child's stderr to our own stderr
      new Thread(new StreamPump(p.getErrorStream(), System.err)).start();

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

      // read the port for the child's socket from its stdout
      final DataInputStream din = new DataInputStream(p.getInputStream());
      final int childPort = din.readInt();

      // pump child's stdout to our own stdout
      new Thread(new StreamPump(p.getInputStream(), System.out)).start();

      // create the client for the child's socket
      clientSocket = new Socket((String) null, childPort);
      cmdC = new CommandClient(clientSocket);
      children.add(cmdC);

      // block until the process ends
      p.waitFor();
      return null;
    }

    @Override
    protected void done() {
      try {
        get();
        clientSocket.close();
        serverSocket.close();
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
      catch (IOException e) {
        CommunicationErrorDialog.error(e);
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
      if (cmd instanceof Launcher.SaveFileCmd) {
        return ModuleManagerWindow.getInstance().update(
          ((Launcher.SaveFileCmd) cmd).getFile());
      }
      else if ("NOTIFY_OPEN_OK".equals(cmd)) {
        ModuleManagerWindow.getInstance().addModule(lr.module);
        window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        return "OK";
      }
      else if ("NOTIFY_NEW_OK".equals(cmd)) {
        window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        return "OK";
      }
      else if ("NOTIFY_IMPORT_OK".equals(cmd)) {
        window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        return "OK";
      }
      else if (cmd instanceof Launcher.LoadFailedCmd) {
        window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

        final Throwable thrown = ((Launcher.LoadFailedCmd) cmd).getThrowable();

        ErrorDialog.error(
          Resources.getString("Error.load_error"),
          Resources.getString("Error.load_error"),
          thrown,
          Resources.getString("Error.load_error_message"),
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
