/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Joel Uckelman
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

import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.tools.version.VersionUtils;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.configure.IntConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.concurrent.FutureUtils;
import VASSAL.tools.concurrent.SimpleRunnableFuture;
import VASSAL.tools.logging.LoggedOutputStream;
import VASSAL.tools.menu.MenuManager;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class Launcher {
  private static final Logger logger = LoggerFactory.getLogger(Launcher.class);

  protected final LaunchRequest lr;

  private static Launcher instance = null;

  public static Launcher getInstance() {
    return instance;
  }

  protected Launcher(String[] args) {
    if (instance != null) throw new IllegalStateException();
    instance = this;

    LaunchRequest lreq = null;
    try {
      lreq = LaunchRequest.parseArgs(args);
    }
    catch (LaunchRequestException e) {
      System.err.println("VASSAL: " + e.getMessage()); //NON-NLS
      System.exit(1);
    }

    lr = lreq;

    // start the error log and setup system properties
    final StartUp start = SystemUtils.IS_OS_MAC ?
      new MacOSXStartUp() : new StartUp();

    start.startErrorLog();

    // log everything which comes across our stderr
    System.setErr(new PrintStream(new LoggedOutputStream(), true, Charset.defaultCharset()));

    logger.info(getClass().getSimpleName());
    Thread.setDefaultUncaughtExceptionHandler(new ExceptionHandler());
    start.initSystemProperties();

    createMenuManager();


    final SimpleRunnableFuture<Void> fut = new SimpleRunnableFuture() {
      @Override
      public void run() {
        try {
          launch();
          set(null);
        }
        catch (Throwable t) {
          setException(t);
        }
      }
    };

    SwingUtilities.invokeLater(fut);

    try {
      fut.get();
    }
    catch (CancellationException | InterruptedException e) {
      // this should be impossible
      FutureUtils.wait(
        ErrorDialog.showDetails(
          e,
          ThrowableUtils.getStackTrace(e),
          "Error.module_load_failed", //NON-NLS
          e.getMessage()
        )
      );
      System.exit(1);
    }
    catch (ExecutionException e) {
      final Throwable t = e.getCause();

      if (t instanceof OutOfMemoryError) {
        // The module ran out of memory while loading. The user has no way
        // of setting the max heap if the module won't load. Double the max
        // heap and ask the user to try again.
        logger.error("Exiting: ", t);
        FutureUtils.wait(ErrorDialog.show("Error.out_of_memory_reload"));

        final String gname = GameModule.getGameModule().getGameName();
        try (Prefs p = new Prefs(Prefs.getGlobalPrefs().getEditor(), gname)) {
          final IntConfigurer maxHeapConf = new IntConfigurer(
            GlobalOptions.MAXIMUM_HEAP, "", 512
          );
          p.addOption(maxHeapConf);

          Integer h = (Integer) p.getValue(GlobalOptions.MAXIMUM_HEAP);
          if (h == null) {
            h = 512;
          }
          p.setValue(GlobalOptions.MAXIMUM_HEAP, 2*h);
        }
        catch (IOException ioe) {
          logger.error("", ioe);
        }

        System.exit(2);
      }
      else if (t instanceof ExtensionsLoader.LoadExtensionException ||
               t instanceof IOException) {
        FutureUtils.wait(
          ErrorDialog.showDetails(
            e,
            ThrowableUtils.getStackTrace(e),
            "Error.module_load_failed", //NON-NLS
            e.getMessage()
          )
        );
        System.exit(1);
      }
    }
  }


  private static class LoadConstraint {
    public String moduleName;
    public String minVassalVersion;
    public String minModuleVersion;
    public String newVersionLink;

    public LoadConstraint(String moduleName, String minVassalVersion, String minModuleVersion, String newVersionLink) {
      this.moduleName = moduleName;
      this.minVassalVersion =  minVassalVersion;
      this.minModuleVersion =  minModuleVersion;
      this.newVersionLink   =  newVersionLink;
    }
  }

  private static final List<LoadConstraint> constraints = List.of(
    new LoadConstraint("VASL", "3.4", "", "<a href=\"https://vasl.info\">vasl.info</a>"),                                                    // NON-NLS
    new LoadConstraint("VSQL", "3.4", "", "<a href=\"https://vassalengine.org/wiki/Module:Squad_Leader\">vassalengine.org</a>"),             // NON-NLS
    new LoadConstraint("Paths of Glory", "", "9.9", "<a href=\"https://vassalengine.org/wiki/Module:Paths_of_Glory\">vassalengine.org</a>")  // NON-NLS
  );

  public static boolean checkModuleLoadable(ModuleMetaData md) {
    final String name = md.getName();
    for (final LoadConstraint rule : constraints) {
      if (rule.moduleName.equals(name)) {
        if (!rule.minVassalVersion.isEmpty() && (VersionUtils.compareVersions(md.getVassalVersion(), rule.minVassalVersion) < 0)) {
          ErrorDialog.show("Error.module_too_old_vassal_version", name, Info.getVersion(), rule.newVersionLink); //NON-NLS
          return false;
        }
        if (!rule.minModuleVersion.isEmpty() && (VersionUtils.compareVersions(md.getVersion(), rule.minModuleVersion) < 0)) {
          ErrorDialog.show("Error.module_too_old_module_version", name, Info.getVersion(), rule.newVersionLink);  //NON-NLS
          return false;
        }
      }
    }

    return true;
  }

  protected abstract void launch() throws IOException;

  protected abstract MenuManager createMenuManager();
}
