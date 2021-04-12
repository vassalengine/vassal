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

import java.awt.Font;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.module.ExtensionsLoader;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
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

    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        try {
          launch();
        }
        catch (ExtensionsLoader.LoadExtensionException | IOException e2) {
          warn(e2);
        }
      }

      private void warn(Exception e1) {
        ErrorDialog.showDetails(
          e1,
          ThrowableUtils.getStackTrace(e1),
          "Error.module_load_failed", //NON-NLS
          e1.getMessage()
        );
        System.exit(1);
      }
    });
  }

  protected abstract void launch() throws IOException;

  protected abstract MenuManager createMenuManager();
}
