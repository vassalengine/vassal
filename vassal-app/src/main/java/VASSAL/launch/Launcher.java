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

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.net.Socket;

import javax.swing.SwingUtilities;

import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.ipc.IPCMessenger;
import VASSAL.tools.ipc.SimpleIPCMessage;
import VASSAL.tools.logging.LoggedOutputStream;
import VASSAL.tools.menu.MenuManager;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class Launcher {
  private static final Logger logger = LoggerFactory.getLogger(Launcher.class);

  protected IPCMessenger ipc = null;

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
      System.err.println("VASSAL: " + e.getMessage());
      System.exit(1);
    }

    lr = lreq;

    // Note: We could do more sanity checking of the launch request
    // in standalone mode, but we don't bother because this is meant
    // only for debugging, not for normal use. If you pass nonsense
    // arguments (e.g., '-e' to the Player), don't expect it to work.
    final boolean standalone = lr.standalone;

/*
    // parse the command line args now if we're standalone, since they
    // could be messed up and so we'll bail before setup
    LaunchRequest lr = null;
    if (standalone) {
      // Note: We could do more sanity checking of the launch request
      // in standalone mode, but we don't bother because this is meant
      // only for debugging, not for normal use. If you pass nonsense
      // arguments (e.g., '-e' to the Player), don't expect it to work.
      try {
        lr = LaunchRequest.parseArgs(args);
      }
      catch (LaunchRequestException e) {
        System.err.println("VASSAL: " + e.getMessage());
        System.exit(1);
      }
    }
*/

    // start the error log and setup system properties
    final StartUp start = SystemUtils.IS_OS_MAC_OSX ?
      new MacOSXStartUp() : new StartUp();

    start.startErrorLog();

    // log everything which comes across our stderr
    System.setErr(new PrintStream(new LoggedOutputStream(), true));

    logger.info(getClass().getSimpleName());
    Thread.setDefaultUncaughtExceptionHandler(new ExceptionHandler());
    start.initSystemProperties();

    // if we're not standalone, contact the module manager for instructions
    if (!standalone) {
      try {
        final int port = Integer.parseInt(System.getProperty("VASSAL.port"));

        final InetAddress lo = InetAddress.getByName(null);
        final Socket cs = new Socket(lo, port);

        ipc = new IPCMessenger(cs);

        ipc.addEventListener(CloseRequest.class, new CloseRequestListener());

        ipc.start();

        ipc.send(new StartedNotice(Info.getInstanceID()));
      }
      catch (IOException e) {
        // What we've got here is failure to communicate.
        ErrorDialog.show(
          e,
          "Error.communication_error",
          Resources.getString(getClass().getSimpleName() + ".app_name")
        );
        System.exit(1);
      }
    }

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
        if (ipc == null) {
          // we are standalone, so warn the user directly
          ErrorDialog.showDetails(
            e1,
            ThrowableUtils.getStackTrace(e1),
            "Error.module_load_failed",
            e1.getMessage()
          );
        }
        else {
          // we have a manager, so pass the load failure back to it
          try {
            ipc.send(new AbstractLaunchAction.NotifyOpenModuleFailed(lr, e1));
          }
          catch (IOException e2) {
            // warn the user directly as a last resort
            ErrorDialog.showDetails(
              e1,
              ThrowableUtils.getStackTrace(e1),
              "Error.module_load_failed",
              e1.getMessage()
            );

            ErrorDialog.show(
              e2,
              "Error.communication_error",
              Resources.getString(getClass().getSimpleName() + ".app_name")
            );
          }
        }

        System.exit(1);
      }
    });
  }

  protected abstract void launch() throws IOException;

  protected abstract MenuManager createMenuManager();

  public static class CloseRequest extends SimpleIPCMessage {
    private static final long serialVersionUID = 1L;
  }

  public static class CloseAccept extends SimpleIPCMessage {
    private static final long serialVersionUID = 1L;

    public final long pid;

    public CloseAccept(long pid) {
      this.pid = pid;
    }
  }

  public static class CloseReject extends SimpleIPCMessage {
    private static final long serialVersionUID = 1L;

    public final long pid;

    public CloseReject(long pid) {
      this.pid = pid;
    }
  }

  public static class StartedNotice extends SimpleIPCMessage {
    private static final long serialVersionUID = 1L;

    public final long pid;

    public StartedNotice(long pid) {
      this.pid = pid;
    }
  }

  protected class CloseRequestListener implements EventListener<CloseRequest> {
    private boolean shutdown;

    @Override
    public void receive(Object src, CloseRequest msg) {
      final GameModule module = GameModule.getGameModule();
      if (module != null) {
        try {
          SwingUtilities.invokeAndWait(new Runnable() {
            @Override
            public void run() {
              module.getFrame().toFront();
              shutdown = module.shutDown();
            }
          });
        }
        catch (InterruptedException e) {
          logger.error("", e);
          shutdown = false;
        }
        catch (InvocationTargetException e) {
          ErrorDialog.bug(e);
          shutdown = false;
        }
      }

      if (shutdown) {
        if (ipc != null) {
          try {
            ipc.send(new CloseAccept(-msg.getId()));
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
        System.exit(0);
      }
      else {
        if (ipc != null) {
          try {
            ipc.send(new CloseReject(-msg.getId()));
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
      }
    }
  }

  public void sendSaveCmd(File f) {
    if (ipc != null) {
      try {
        ipc.send(new AbstractLaunchAction.NotifySaveFileOk(f));
      }
      // FIXME: review error message
      catch (IOException e) {
      }
    }
  }
}
