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
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
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
import VASSAL.preferences.Prefs;
import VASSAL.preferences.ReadOnlyPrefs;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.IOUtils;
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
      catch (IOException e) {
        ErrorLog.warn(e);
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

    if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
      lr.module = fc.getSelectedFile();
      if (lr.module != null && !lr.module.exists()) lr.module = null;
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

    protected ServerSocket serverSocket;
    protected Socket clientSocket;
  
    protected CommandClient cmdC;
    protected CommandServer cmdS;

    @Override
    public Void doInBackground() throws Exception {
      // get heap setttings
      int initialHeap = DEFAULT_INITIAL_HEAP;
      int maximumHeap = DEFAULT_MAXIMUM_HEAP;

      // get module name
      final AbstractMetaData data = AbstractMetaData.buildMetaData(lr.module);
      if (data != null && data instanceof ModuleMetaData) {
        final String moduleName = ((ModuleMetaData) data).getName();

        // read module prefs
        final ReadOnlyPrefs p = new ReadOnlyPrefs(moduleName);

        // read initial heap size, if it exists
        final String iheap = p.getStoredValue(GlobalOptions.INITIAL_HEAP);
        if (iheap != null) {
          try {
            initialHeap = Integer.parseInt(iheap);
          }
          catch (NumberFormatException ex) {
// FIXME: warn the user the prefs are corrupt
            ErrorLog.warn(ex);
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
// FIXME: warn the user the prefs are corrupt
            ErrorLog.warn(ex);
            maximumHeap = DEFAULT_MAXIMUM_HEAP;
          }
        }
      }

      // create a socket for communicating which the child process
      final ServerSocket serverSocket = new ServerSocket(0);
      cmdS = new LaunchCommandServer(serverSocket);
      new Thread(cmdS).start();

      // build the child process
      final String[] args = lr.toArgs();      
      final String[] pa = new String[6 + args.length];
      pa[0] = "java";
      pa[1] = "-Xms" + initialHeap + "M";
      pa[2] = "-Xmx" + maximumHeap + "M";
      pa[3] = "-cp"; 
      pa[4] = System.getProperty("java.class.path");
      pa[5] = entryPoint; 
      System.arraycopy(args, 0, pa, 6, args.length);

      final ProcessBuilder pb = new ProcessBuilder(pa);
      pb.directory(Info.getBinDir());

      final Process p = pb.start();

      // pump child's stderr to our own stderr
      new Thread(new StreamPump(p.getErrorStream(), System.err)).start();

      // write the port for this socket to child's stdin and close
      DataOutputStream dout = null;
      try {
        dout = new DataOutputStream(p.getOutputStream());
        dout.writeInt(serverSocket.getLocalPort());
      }
      finally {
        IOUtils.closeQuietly(dout);
      }

      // read the port for the child's socket from its stdout
      final DataInputStream din = new DataInputStream(p.getInputStream());
      final int childPort = din.readInt();

      // pump child's stdout to our own stdout
      new Thread(new StreamPump(p.getInputStream(), System.out)).start();

      // create the client for the child's socket
      cmdC = new CommandClient(new Socket((String) null, childPort));
      children.add(cmdC);

      // trigger any process() methods belonging to subclasses
      publish((Void) null);

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
      }
      catch (InterruptedException e) {
        ErrorLog.warn(e);
      }
      catch (ExecutionException e) {
        ErrorLog.warn(e);
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
      else if ("NOTIFY_OPEN".equals(cmd)) {
        window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
        ErrorLog.warn(e);
      }
    }
  }
}
