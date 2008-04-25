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
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.awt.event.ActionEvent;
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
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.FileChooser;
import VASSAL.tools.IOUtils;

public abstract class AbstractLaunchAction extends AbstractAction {
  private static final long serialVersionUID = 1L;
  
  public static final int DEFAULT_INITIAL_HEAP = 256;
  public static final int DEFAULT_MAXIMUM_HEAP = 512;

  protected final Window window; 
  protected File module;
  protected final String entryPoint;
  protected final String[] args;

  protected static final Set<File> editing = new HashSet<File>();
  protected static final Map<File,Integer> using =
    new HashMap<File,Integer>();

  public AbstractLaunchAction(String name, Window window, String entryPoint,
                              String[] args, File module) {
    super(name);
    this.window = window;
    this.entryPoint = entryPoint;
    this.args = args;
    this.module = module;
  }
  
  public static boolean isInUse(File f) {
    return using.containsKey(f);
  }
  
  public static boolean isEditing(File f) {
    return editing.contains(f);
  }

  public void actionPerformed(ActionEvent e) {
    window.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    getLaunchTask().execute();
  }

  protected abstract LaunchTask getLaunchTask(); 

  protected File promptForModule() {
    // prompt the use to pick a module
    final FileChooser fc = FileChooser.createFileChooser(window,
      (DirectoryConfigurer)
        Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));

    if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
      module = fc.getSelectedFile();
      if (module != null && !module.exists()) module = null;
    }
    
    return module;
  }

  protected class LaunchTask extends SwingWorker<Void,Void> {
    // module might be reassigned before the task is over, keep a local copy
    protected final File mod = AbstractLaunchAction.this.module; 

    @Override
    public Void doInBackground() throws Exception {
      int initialHeap; 
      try {
        initialHeap = Integer.parseInt(Prefs.getGlobalPrefs()
          .getStoredValue(GlobalOptions.INITIAL_HEAP));
      }
      catch (NumberFormatException ex) {
        // don't show warning dialog, since this isn't fatal,
        // or even abnormal, e.g., in the case where this is
        // a new copy of VASSAL
        ex.printStackTrace();
        initialHeap = DEFAULT_INITIAL_HEAP;
      }

      int maximumHeap;
      try {
        maximumHeap = Integer.parseInt(Prefs.getGlobalPrefs()
          .getStoredValue(GlobalOptions.MAXIMUM_HEAP));
      }
      catch (NumberFormatException ex) {
        // don't show warning dialog, since this isn't fatal,
        // or even abnormal, e.g., in the case where this is
        // a new copy of VASSAL
        ex.printStackTrace();
        maximumHeap = DEFAULT_MAXIMUM_HEAP;
      }

      final String[] pa =
        new String[6 + args.length + (mod == null ? 0 : 1)];
      pa[0] = "java";
      pa[1] = "-Xms" + initialHeap + "M";
      pa[2] = "-Xmx" + maximumHeap + "M";
      pa[3] = "-cp"; 
      pa[4] = System.getProperty("java.class.path");
      pa[5] = entryPoint; 
      System.arraycopy(args, 0, pa, 6, args.length);
      if (mod != null) pa[pa.length-1] = mod.getPath();

      final ProcessBuilder pb = new ProcessBuilder(pa);
      pb.directory(Info.getBinDir());

      final Process p = pb.start();

      // close child's stdin because we won't write to it 
      p.getOutputStream().close();

      // pump child's stderr to our own stderr
      new StreamPump(p.getErrorStream(), System.err).start();

      // child writes a char to stdout to signal end of loading
      p.getInputStream().read();
      publish((Void) null);

      // pump child's stdout to our own stdout
      new StreamPump(p.getInputStream(), System.out).start();

      // block until the process ends
      p.waitFor();
      return null;
    }

    @Override
    protected void process(List<Void> chunks) {
      window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
    }
  }

  private static class StreamPump extends Thread {
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
