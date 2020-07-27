/*
 *
 * Copyright (c) 2000-2008 by Brent Easton, Rodney Kinney, Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.launch;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.ModuleUpdaterDialog;
import VASSAL.configure.SaveAction;
import VASSAL.configure.SaveAsAction;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidationReportDialog;
import VASSAL.i18n.Resources;
import VASSAL.tools.ApplicationIcons;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.menu.ChildProxy;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;

/**
 * EditorWindow is the base class for the three top-level component
 * editors :- ModuleEditorWindow, ExtensionEditorWindow, PluginEditorWindow
 *
 * @author Brent Easton
 */
public abstract class EditorWindow extends JFrame {
  private static final long serialVersionUID = 1L;

  protected SaveAction saveAction;
  protected SaveAsAction saveAsAction;
  protected JMenuItem componentHelpItem;
  protected Action createUpdater;

  protected final HelpWindow helpWindow = new HelpWindow(
    Resources.getString("Editor.ModuleEditor.reference_manual"), //$NON-NLS-1$
    null
  );

  protected ConfigureTree tree;

  public abstract String getEditorType();

  protected final JToolBar toolBar = new JToolBar();

  protected final JScrollPane scrollPane;

  protected EditorWindow() {
    setTitle("VASSAL " + getEditorType() + " Editor");
    setLayout(new BorderLayout());

    ApplicationIcons.setFor(this);

    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        close();
      }
    });

    toolBar.setFloatable(false);
    add(toolBar, BorderLayout.NORTH);

    // setup menubar and actions
    final MenuManager mm = MenuManager.getInstance();
    final MenuBarProxy mb = mm.getMenuBarProxyFor(this);

    // file menu
    if (SystemUtils.IS_OS_MAC_OSX) {
      mm.addToSection("Editor.File", mm.addKey("Editor.save"));
      mm.addToSection("Editor.File", mm.addKey("Editor.save_as"));
    }
    else {
      final MenuProxy fileMenu =
        new MenuProxy(Resources.getString("General.file"));

      // FIMXE: setting nmemonic from first letter could cause collisions in
      // some languages
      fileMenu.setMnemonic(Resources.getString("General.file.shortcut").charAt(0));

      fileMenu.add(mm.addKey("Editor.save"));
      fileMenu.add(mm.addKey("Editor.save_as"));
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("General.quit"));
      mb.add(fileMenu);
    }

    // edit menu
    final MenuProxy editMenu =
      new MenuProxy(Resources.getString("General.edit"));
    editMenu.setMnemonic(Resources.getString("General.edit.shortcut").charAt(0));

    editMenu.add(mm.addKey("Editor.cut"));
    editMenu.add(mm.addKey("Editor.copy"));
    editMenu.add(mm.addKey("Editor.paste"));
    editMenu.add(mm.addKey("Editor.move"));
    editMenu.addSeparator();
    editMenu.add(mm.addKey("Editor.search"));
    editMenu.addSeparator();
    editMenu.add(mm.addKey("Editor.ModuleEditor.properties"));
    editMenu.add(mm.addKey("Editor.ModuleEditor.translate"));

    // tools menu
    final MenuProxy toolsMenu =
      new MenuProxy(Resources.getString("General.tools"));
    toolsMenu.setMnemonic(Resources.getString("General.tools.shortcut").charAt(0));

    toolsMenu.add(mm.addKey("create_module_updater"));
    toolsMenu.add(mm.addKey("Editor.ModuleEditor.update_saved"));

    if (SystemUtils.IS_OS_MAC_OSX) {
      mm.addToSection("Editor.MenuBar", editMenu);
      mm.addToSection("Editor.MenuBar", toolsMenu);
    }
    else {
      mb.add(editMenu);
      mb.add(toolsMenu);
    }

    // help menu
    if (SystemUtils.IS_OS_MAC_OSX) {
      mm.addToSection("Documentation.VASSAL",
                      mm.addKey("Editor.ModuleEditor.reference_manual"));
    }
    else {
      final MenuProxy helpMenu =
        new MenuProxy(Resources.getString("General.help"));

      // FIMXE: setting nmemonic from first letter could cause collisions in
      // some languages
      helpMenu.setMnemonic(Resources.getString("General.help.shortcut").charAt(0));

      helpMenu.add(mm.addKey("General.help"));
      helpMenu.add(mm.addKey("Help.user_guide"));
      helpMenu.add(mm.addKey("Editor.ModuleEditor.reference_manual"));
      helpMenu.addSeparator();
      helpMenu.add(mm.addKey("AboutScreen.about_vassal"));
      mb.add(helpMenu);
    }

    final int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx();

    saveAction = new SaveAction() {
      private static final long serialVersionUID = 1L;
      @Override
      public void actionPerformed(ActionEvent e) {
        save();
        treeStateChanged(false);
      }
    };

    saveAction.setEnabled(false);
    saveAction.putValue(Action.ACCELERATOR_KEY,
      KeyStroke.getKeyStroke(KeyEvent.VK_S, mask));
    mm.addAction("Editor.save", saveAction);
    toolBar.add(saveAction);

    saveAsAction = new SaveAsAction() {
      private static final long serialVersionUID = 1L;
      @Override
      public void actionPerformed(ActionEvent e) {
        saveAs();
        treeStateChanged(false);
      }
    };

    saveAsAction.setEnabled(false);
    saveAsAction.putValue(Action.ACCELERATOR_KEY,
      KeyStroke.getKeyStroke(KeyEvent.VK_A, mask));
    mm.addAction("Editor.save_as", saveAsAction);
    toolBar.add(saveAsAction);

    mm.addAction("General.quit", new ShutDownAction());
// FXIME: mnemonics should be language-dependant
//    mm.getAction("General.quit").setMnemonic('Q');

    createUpdater = new AbstractAction(
                      "Create " + getEditorType() + " updater") {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        new ModuleUpdaterDialog(EditorWindow.this).setVisible(true);
      }
    };
    createUpdater.setEnabled(false);
    mm.addAction("create_module_updater", createUpdater);

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "README.html").toURI().toURL();
      mm.addAction("General.help", new ShowHelpAction(url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "userguide/userguide.pdf").toURI().toURL();
      mm.addAction("Help.user_guide",
        new ShowHelpAction("Help.user_guide", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "ReferenceManual/index.htm").toURI().toURL();

      final ShowHelpAction helpAction = new ShowHelpAction(
        url,
        helpWindow.getClass().getResource("/images/Help16.gif")
      );

      helpAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString(
        "Editor.ModuleEditor.reference_manual")); //$NON-NLS-1$
      toolBar.add(helpAction);
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    mm.addAction("AboutScreen.about_vassal", new AboutVASSALAction(this));

    setJMenuBar(mm.getMenuBarFor(this));

    // the presence of the panel prevents a NullPointerException on packing
    final JPanel panel = new JPanel();
    panel.setPreferredSize(new Dimension(250,400));

    scrollPane = new JScrollPane(
      panel,
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    add(scrollPane, BorderLayout.CENTER);
    pack();
  }

  protected MenuProxy findMenuProxy(String name, MenuBarProxy mb) {
    for (ChildProxy<?> c : mb.getChildren()) {
      if (c instanceof MenuProxy) {
        final MenuProxy m = (MenuProxy) c;
        if (name.equals(m.getText())) return m;
      }
    }
    return null;
  }

  /*
   * Each component must Save, SaveAs and close itself
   */
  protected abstract void save();
  protected abstract void saveAs();

  protected void close() {
    GameModule.getGameModule().quit();
  }

  protected void saver(final Runnable save) {
    final ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(), report);
    if (report.getWarnings().size() == 0) {
      save.run();
    }
    else {
      new ValidationReportDialog(report,
        new ValidationReportDialog.CallBack() {
          @Override
          public void ok() {
            save.run();
          }

          @Override
          public void cancel() {
          }
        }
      ).setVisible(true);
    }
  }

  /**
   * Called by the enclosed ConfigureTree or ExtensionTree when it's dirty
   * state is changed. The implementing class should override this if they
   * need to take action like changing menu availability.
   *
   * @param changed true if the tree is in a changed (dirty) state
   */
  public void treeStateChanged(boolean changed) { }
}
