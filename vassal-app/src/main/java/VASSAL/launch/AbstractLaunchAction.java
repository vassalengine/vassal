/*
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.zip.ZipFile;

import javax.swing.AbstractAction;
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
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ModuleFileFilter;
import VASSAL.tools.io.ProcessLauncher;
import VASSAL.tools.io.ProcessWrapper;
import VASSAL.tools.lang.MemoryUtils;
import VASSAL.i18n.Resources;

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
  protected static final int DEFAULT_MAXIMUM_HEAP = 512;
  protected static final int FAILSAFE_MAXIMUM_HEAP = 128;

  static {
    // Determine how much physical RAM this machine has
    // Assume 4GB if we can't determine how much RAM there is
    final long physMemoryBytes = MemoryUtils.getPhysicalMemory();
    PHYS_MEMORY = physMemoryBytes <= 0 ? 4096 : (int)(physMemoryBytes >> 20);
  }

  protected final Window window;
  protected final String entryPoint;
  protected final LaunchRequest lr;

  protected static final Map<File, Integer> using =
    Collections.synchronizedMap(new HashMap<>());

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
    return Integer.valueOf(-1).equals(using.get(file));
  }

  protected static void incrementUsed(File file) {
    using.merge(file, 1, Integer::sum);
  }

  protected static void decrementUsed(File file) {
    using.merge(file, 0, (v, n) -> v == 1 ? null : v - 1);
  }

  protected static void markEditing(File file) {
    using.put(file, -1);
  }

  protected static void unmarkEditing(File file) {
    using.remove(file);
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
          if (!(metadata instanceof ModuleMetaData)) {
            ErrorDialog.show(
              "Error.invalid_vassal_module", lr.module.getAbsolutePath()); //NON-NLS
            logger.error(
              "-- Load of {} failed: Not a Vassal module", //NON-NLS
              lr.module.getAbsolutePath()
            );
            lr.module = null;
          }
        }
        else {
          lr.module = null;
        }
// FIXME: do something to warn about nonexistent file
//        FileNotFoundDialog.warning(window, lr.module);
      }
    }

    return lr.module;
  }

  protected void addFileFilters(FileChooser fc) {
    fc.addChoosableFileFilter(new ModuleFileFilter());
  }

  protected class LaunchTask extends SwingWorker<Void, Void> {
    // lr might be modified before the task is over, keep a local copy
    protected final LaunchRequest lr =
      new LaunchRequest(AbstractLaunchAction.this.lr);

    @Override
    public Void doInBackground() throws InterruptedException,
                                        IOException {
// FIXME: this should be in an abstract method and farmed out to subclasses
      // send some basic information to the log
      if (lr.module != null) {
        logger.info("Loading module file {}", lr.module.getAbsolutePath()); //NON-NLS

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
          PHYS_MEMORY
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
        for (final File ext : mgr.getActiveExtensions()) {
          final TilingHandler eth = new TilingHandler(
            ext.getAbsolutePath(),
            cdir,
            new Dimension(256, 256),
            PHYS_MEMORY
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
        logger.info("Loading game file {}", lr.game.getAbsolutePath()); //NON-NLS
      }

      if (lr.importFile != null) {
        logger.info(
          "Importing module file {}", //NON-NLS
          lr.importFile.getAbsolutePath()
        );
      }
// end FIXME

      // set default heap size
      int maximumHeap = DEFAULT_MAXIMUM_HEAP;

      String moduleName = null;

// FIXME: this should be in an abstract method and farmed out to subclasses,
// rather than a case structure for each kind of thing which may be loaded.
      // find module-specific heap settings, if any
      if (lr.module != null) {
        final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.module);

        if (data == null) {
          ErrorDialog.show(
            "Error.invalid_vassal_file", lr.module.getAbsolutePath()); //NON-NLS
          return null;
        }

        if (data instanceof ModuleMetaData) {
          moduleName = ((ModuleMetaData) data).getName();

          // log the module name
          logger.info("Loading module {}", moduleName); //NON-NLS

          // read module prefs
          final ReadOnlyPrefs p = new ReadOnlyPrefs(moduleName);

          // read maximum heap size
          maximumHeap = getHeapSize(
            p, GlobalOptions.MAXIMUM_HEAP, DEFAULT_MAXIMUM_HEAP);
        }
      }
      else if (lr.importFile != null) {
        final Prefs p = Prefs.getGlobalPrefs();

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
// differentiate between loading a module and importing a module,
// since the heap sizes are set in different places for those two
// actions.
      // maximum heap must fit in physical RAM
      if (maximumHeap > PHYS_MEMORY) {
        maximumHeap = FAILSAFE_MAXIMUM_HEAP;

        FutureUtils.wait(WarningDialog.show(
          "Warning.maximum_heap_too_large", //NON-NLS
          FAILSAFE_MAXIMUM_HEAP
        ));
      }
      // maximum heap must be at least the failsafe size
      else if (maximumHeap < FAILSAFE_MAXIMUM_HEAP) {
        maximumHeap = FAILSAFE_MAXIMUM_HEAP;

        FutureUtils.wait(WarningDialog.show(
          "Warning.maximum_heap_too_small", //NON-NLS
          FAILSAFE_MAXIMUM_HEAP
        ));
      }

      final int initialHeap = maximumHeap;

      final List<String> argumentList = buildArgumentList(moduleName);
      final String[] args = argumentList.toArray(new String[0]);

      // try to start a child process with the given heap sizes
      args[1] = "-Xms" + initialHeap + "M"; //NON-NLS
      args[2] = "-Xmx" + maximumHeap + "M"; //NON-NLS

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
        args[1] = "-Xms" + FAILSAFE_MAXIMUM_HEAP + "M"; //NON-NLS
        args[2] = "-Xmx" + FAILSAFE_MAXIMUM_HEAP + "M"; //NON-NLS
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
            "Warning.maximum_heap_too_large", //NON-NLS
            FAILSAFE_MAXIMUM_HEAP
          ));
        }
      }

      final ModuleManagerWindow mmw = ModuleManagerWindow.getInstance();
      if (lr.module != null) {
        mmw.addModule(lr.module);
      }
      mmw.setWaitCursor(false);

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
      }
      catch (ExecutionException e) {
        if (SystemUtils.IS_OS_WINDOWS &&
            ThrowableUtils.getAncestor(IOException.class, e) != null) {
          final String msg = e.getMessage();
          if (msg.contains("jre\\bin\\java") && msg.contains("CreateProcess")) {
            ErrorDialog.showDetails(
              e,
              ThrowableUtils.getStackTrace(e),
              "Error.possible_windows_av_interference",
              msg
            );
            return;
          }
        }
        ErrorDialog.bug(e);
      }
      catch (InterruptedException e) {
        ErrorDialog.bug(e);
      }
      finally {
        ModuleManagerWindow.getInstance().setWaitCursor(false);
      }
    }

    private List<String> buildArgumentList(String moduleName) {
      final List<String> result = new ArrayList<>();

      result.add(Info.getJavaBinPath().getAbsolutePath());
      result.add("");   // reserved for initial heap
      result.add("");   // reserved for maximum heap

      result.addAll(new CustomVmOptions().getCustomVmOptions());

      // pass on the user's home, if it's set
      final String userHome = System.getProperty("user.home");
      if (userHome != null) result.add("-Duser.home=" + userHome); //NON-NLS

      // pass on the user's working dir, if it's set
      final String userDir = System.getProperty("user.dir");
      if (userDir != null) result.add("-Duser.dir=" + userDir); //NON-NLS

      // pass on VASSAL's conf dir, if it's set
      final String vConf = System.getProperty("VASSAL.conf");
      if (vConf != null) result.add("-DVASSAL.conf=" + vConf); //NON-NLS

      // set the classpath
      result.add("-cp"); //NON-NLS
      result.add(System.getProperty("java.class.path"));

      if (SystemUtils.IS_OS_MAC) {
        // set the MacOS dock parameters

        // use the module name for the dock if we found a module name
// FIXME: should "Unnamed module" be localized?
        final String d_name = moduleName != null && moduleName.length() > 0
          ? moduleName : Resources.getString("Editor.AbstractLaunchAction.unnamed_module"); //NON-NLS

        // get the path to the app icon
        final String d_icon = new File(Info.getBaseDir(),
          "Contents/Resources/VASSAL.icns").getAbsolutePath();

        result.add("-Xdock:name=" + d_name); //NON-NLS
        result.add("-Xdock:icon=" + d_icon); //NON-NLS
      }
      else if (SystemUtils.IS_OS_WINDOWS) {
        // Disable the 2D to Direct3D pipeline?
        final Boolean disableD3d =
          (Boolean) Prefs.getGlobalPrefs().getValue(Prefs.DISABLE_D3D);
        if (Boolean.TRUE.equals(disableD3d)) {
          result.add("-Dsun.java2d.d3d=false"); //NON-NLS
        }
      }

      result.add(entryPoint);
      result.addAll(Arrays.asList(lr.toArgs()));
      return result;
    }
  }
}
