/*
 * $Id$
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

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Properties;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.FileChooser;
import VASSAL.tools.imports.ImportAction;
import VASSAL.tools.menu.MacOSXMenuManager;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;

public class Editor {
  protected File moduleFile;
  protected File extensionFile;
  protected List<String> extractTargets = new ArrayList<String>();

  private boolean newModule = false;
  private boolean newExtension = false;
  private boolean importModule = false;

  protected CommandClient cmdC = null;
  protected CommandServer cmdS = null;

  public Editor(final String[] args) {
    StartUp.initSystemProperties();
//    StartUp.setupErrorLog();
    StartUp.startErrorLog();

    Thread.setDefaultUncaughtExceptionHandler(new ErrorLog());

//////
    try {
      // set up our command listener
      final ServerSocket serverSocket = new ServerSocket(0);
      cmdS = new EditorCommandServer(serverSocket);
      new Thread(cmdS).start();

      // write our socket port out to the module manager
      new DataOutputStream(System.out).writeInt(serverSocket.getLocalPort());

      // read the module manager's socket port from stdin
      final int port = new DataInputStream(System.in).readInt();
 
      // set up our command client
      cmdC = new CommandClient(new Socket((String) null, port));
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  
    if (cmdC == null || cmdS == null) System.exit(1);
//////
 
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        try {
          Editor.this.configure(args);
          Editor.this.extractResourcesAndLaunch(0);
        }
        catch (IOException e) {
          reportError(e);
        }
      }
    });
  }

  protected static class EditorCommandServer extends CommandServer {
    public EditorCommandServer(ServerSocket serverSocket) {
      super(serverSocket);
    }

    @Override
    protected Object reply(Object cmd) {
      if ("REQUEST_CLOSE".equals(cmd)) {
        final GameModule module = GameModule.getGameModule();
        module.getFrame().toFront();
        final boolean shutDown = module.shutDown();
        try {
          return shutDown ? "OK" : "NOK";
        }
        finally {
          if (shutDown) System.exit(0);
        }
      }
      else {
        return "UNRECOGNIZED_COMMAND";
      } 
    }
  }

  protected void extractResourcesAndLaunch(final int resourceIndex) throws IOException {
    if (resourceIndex >= extractTargets.size()) {
      launch();
    }
    else {
      final Properties props = new Properties();
      final InputStream in =
        Editor.class.getResourceAsStream(extractTargets.get(resourceIndex));
      if (in != null) {
        try {
          props.load(in);
        }
        finally {
          try {
            in.close();
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
      }

      new ResourceExtracter(Prefs.getGlobalPrefs(), props, new Observer() {
        public void update(Observable o, Object arg) {
          try {
            extractResourcesAndLaunch(resourceIndex + 1);
          }
          catch (IOException e) {
            reportError(e);
          }
        }
      }).install();
    }
  }

  protected void reportError(Exception e) {
    e.printStackTrace();
    String msg = e.getMessage();
    if (msg == null) {
      msg = e.getClass().getSimpleName();
    }
    JOptionPane.showMessageDialog(null, msg, Resources.getString("ResourceExtracter.install_failed"), JOptionPane.ERROR_MESSAGE);
  }

  protected void launch() throws IOException {
    if (Info.isMacOSX()) new MacOSXMenuManager();
    else new EditorMenuManager();

    try {
      if (newModule) new CreateModuleAction(null).performAction(null);

      if (moduleFile == null) return;

      if (newExtension) {
        GameModule.init(new BasicModule(new DataArchive(moduleFile.getPath())));
        final JFrame f = GameModule.getGameModule().getFrame();
        f.setVisible(true);
        new NewExtensionAction(f).performAction(null);
      }
      else if (extensionFile != null) {
        GameModule.init(new BasicModule(new DataArchive(moduleFile.getPath())));
        final JFrame f = GameModule.getGameModule().getFrame();
        f.setVisible(true);
        new EditExtensionAction(extensionFile).performAction(null);
      }
      else if (importModule) new ImportAction(null).loadModule(moduleFile); 
      else new EditModuleAction(moduleFile).loadModule(moduleFile);
    }
    finally {
      final Object reply = cmdC.request("NOTIFY_OPEN");
      System.out.println("Reply: " + reply);
    }
  }

  protected void configure(final String[] args) {
    int n = -1;
    while (++n < args.length) {
      final String arg = args[n];
      if ("-extract".equals(arg)) {
        extractTargets.add(args[++n]);
      }
      else if ("-import".equals(arg)) {
        importModule = true; 
      }
      else if ("-new".equals(arg)) {
        newModule = true; 
      }
      else if ("-newext".equals(arg)) {
        newExtension = true;
      }
      else if ("-edext".equals(arg)) {
        extensionFile = new File(args[++n]); 
      }
      else if (!arg.startsWith("-")) {
        moduleFile = new File(arg);
      }
    }
  }

  public static class NewModuleLaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public NewModuleLaunchAction(ModuleManagerWindow mm) {
      super(Resources.getString("Main.new_module"), mm, 
            Editor.class.getName(), new String[]{ "-new" }, null);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask();
    }
  }

  public static class ImportLaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public ImportLaunchAction(ModuleManagerWindow mm, File module) {
      super(Resources.getString("Editor.import_module"), mm,
            Editor.class.getName(), new String[]{ "-import" }, module);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask();
    }
  }

  public static class PromptImportLaunchAction extends ImportLaunchAction {
    private static final long serialVersionUID = 1L;

    public PromptImportLaunchAction(ModuleManagerWindow mm) {
      super(mm, null);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // prompt the user to pick a module
      if (promptForModule() == null) return;

      super.actionPerformed(e);
    }

    @Override
    protected File promptForModule() {
      // prompt the use to pick a module
      final FileChooser fc = ImportAction.getFileChooser(window);

      if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
        module = fc.getSelectedFile();
        if (module != null && !module.exists()) module = null;
      } 
    
      return module;
    }
  }

  public static class LaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public LaunchAction(ModuleManagerWindow mm, File module) {
      super(Resources.getString("Main.edit_module"), mm,
            Editor.class.getName(), new String[0], module);
      setEnabled(!editing.contains(module) && !using.containsKey(module));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // register that this module is being edited
      if (editing.contains(module) || using.containsKey(module)) return;
      editing.add(module);

      super.actionPerformed(e);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // register that this module is no longer being edited
          editing.remove(mod);
          setEnabled(true);
        }

        @Override
        protected void process(List<Void> chunks) {
          super.process(chunks);
          ((ModuleManagerWindow) window).addModule(mod);
        }
      };
    }
  }

  public static class ListLaunchAction extends LaunchAction {
    private static final long serialVersionUID = 1L;

    public ListLaunchAction(ModuleManagerWindow mm, File module) {
      super(mm, module);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      super.actionPerformed(e);
      setEnabled(false);
    }
  }

  public static class PromptLaunchAction extends LaunchAction {
    private static final long serialVersionUID = 1L;

    public PromptLaunchAction(ModuleManagerWindow mm) {
      super(mm, null);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // prompt the user to pick a module
      if (promptForModule() == null) return;

      super.actionPerformed(e);
      module = null;
    }
  }

  private static class EditorMenuManager extends MenuManager {
    private final MenuBarProxy editorBar = new MenuBarProxy();
    private final MenuBarProxy playerBar = new MenuBarProxy();

    @Override
    public JMenuBar getMenuBarFor(JFrame fc) {
      if (fc instanceof PlayerWindow) return playerBar.createPeer();
      else if (fc instanceof EditorWindow) return editorBar.createPeer();
      else return null;
    }

    @Override
    public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
      if (fc instanceof PlayerWindow) return playerBar;
      else if (fc instanceof EditorWindow) return editorBar;
      else return null;
    }
  }

  public static void main(String[] args) {
    new Editor(args);
  }
}
