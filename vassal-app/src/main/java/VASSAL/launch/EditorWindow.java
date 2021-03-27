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

import VASSAL.build.module.GameState;
import VASSAL.command.AlertCommand;
import VASSAL.command.Command;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.RefreshPredefinedSetupsDialog;
import VASSAL.configure.RemoveUnusedImagesDialog;
import VASSAL.configure.SaveAction;
import VASSAL.configure.SaveAsAction;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidationReportDialog;
import org.apache.commons.lang3.SystemUtils;

import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.i18n.Resources;
import VASSAL.tools.ApplicationIcons;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.menu.ChildProxy;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;

/**
 * EditorWindow is the base class for the three top-level component editors :-
 * ModuleEditorWindow, ExtensionEditorWindow, PluginEditorWindow
 *
 * @author Brent Easton
 */
public abstract class EditorWindow extends JFrame {
  private static final long serialVersionUID = 1L;

  protected SaveAction saveAction;
  protected SaveAsAction saveAsAction;
  protected JMenuItem componentHelpItem;

  protected final HelpWindow helpWindow = new HelpWindow(Resources.getString("Editor.ModuleEditor.reference_manual"), //$NON-NLS-1$
    null);

  protected ConfigureTree tree;        // The Configure Tree we are editing

  protected String moduleName = "";    // Current module file if any
  protected String extensionName = ""; // Current extension file if any

  public abstract String getEditorType();

  protected final JToolBar toolBar = new JToolBar();

  protected final JScrollPane scrollPane;

  protected EditorWindow() {
    updateWindowTitle();
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
    if (SystemUtils.IS_OS_MAC) {
      mm.addToSection("Editor.File", mm.addKey("Editor.save"));  //NON-NLS
      mm.addToSection("Editor.File", mm.addKey("Editor.save_as"));  //NON-NLS
    }
    else {
      final MenuProxy fileMenu = new MenuProxy(Resources.getString("General.file"));

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
    final MenuProxy editMenu = new MenuProxy(Resources.getString("General.edit"));
    editMenu.setMnemonic(Resources.getString("General.edit.shortcut").charAt(0));

    editMenu.add(mm.addKey("Editor.cut"));
    editMenu.add(mm.addKey("Editor.copy"));
    editMenu.add(mm.addKey("Editor.paste"));
    editMenu.add(mm.addKey("Editor.move"));
    editMenu.addSeparator();
    editMenu.add(mm.addKey("Editor.search"));
    editMenu.addSeparator();
    editMenu.add(mm.addKey("Editor.properties"));
    editMenu.add(mm.addKey("Editor.ModuleEditor.translate"));

    // tools menu
    final MenuProxy toolsMenu = new MenuProxy(Resources.getString("General.tools"));
    toolsMenu.setMnemonic(Resources.getString("General.tools.shortcut").charAt(0));

    // Remove old updater from menu
    // toolsMenu.add(mm.addKey("Editor.ModuleUpdaterDialog.create_updater"));  //NON-NLS
    // toolsMenu.add(mm.addKey("Editor.ModuleEditor.update_saved"));

    toolsMenu.add(mm.addKey("Editor.ModuleEditor.refresh_predefined"));
    toolsMenu.add(mm.addKey("Editor.UnusedImages.remove_unused_images"));

    if (SystemUtils.IS_OS_MAC) {
      mm.addToSection("Editor.MenuBar", editMenu);  //NON-NLS
      mm.addToSection("Editor.MenuBar", toolsMenu);  //NON-NLS
    }
    else {
      mb.add(editMenu);
      mb.add(toolsMenu);
    }

    // help menu
    if (SystemUtils.IS_OS_MAC) {
      mm.addToSection("Documentation.VASSAL", mm.addKey("Editor.ModuleEditor.reference_manual"));  //NON-NLS
    }
    else {
      final MenuProxy helpMenu = new MenuProxy(Resources.getString("General.help"));

      // FIMXE: setting nmemonic from first letter could cause collisions in
      // some languages
      helpMenu.setMnemonic(Resources.getString("General.help.shortcut").charAt(0));

      //BR// So here I "think"  I'm just adding my happy little help entries.
      helpMenu.add(mm.addKey("Editor.ModuleEditor.table_of_contents"));
      helpMenu.add(mm.addKey("Editor.ModuleEditor.using_the_editor"));
      helpMenu.add(mm.addKey("Editor.ModuleEditor.designer_guide"));
      helpMenu.addSeparator();
      helpMenu.add(mm.addKey("Editor.ModuleEditor.module_components"));
      helpMenu.add(mm.addKey("Editor.ModuleEditor.map_components"));
      helpMenu.add(mm.addKey("Editor.ModuleEditor.toolbar_help"));
      helpMenu.addSeparator();
      helpMenu.add(mm.addKey("Editor.ModuleEditor.game_pieces_and_traits"));
      helpMenu.add(mm.addKey("Editor.ModuleEditor.expressions"));
      helpMenu.add(mm.addKey("Editor.properties"));
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
    saveAction.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, mask));
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
    saveAsAction.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_A, mask));
    mm.addAction("Editor.save_as", saveAsAction);
    toolBar.add(saveAsAction);

    mm.addAction("General.quit", new ShutDownAction());
// FXIME: mnemonics should be language-dependant
//    mm.getAction("General.quit").setMnemonic('Q');

    mm.addAction("Editor.UnusedImages.remove_unused_images", new AbstractAction("Remove Unused Images") {  //NON-NLS
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        new RemoveUnusedImagesDialog(EditorWindow.this).setVisible(true);
      }
    });

    mm.addAction("Editor.ModuleEditor.refresh_predefined", new AbstractAction(Resources.getString(
      "Editor.ModuleEditor.refresh_predefined")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        final GameState gs = GameModule.getGameModule().getGameState();
        if (gs.isGameStarted()) {
          //If a game is started log warning and stop
          final Command ac = new AlertCommand(Resources.getString("GameRefresher.game_started_in_editor"));
          ac.execute();
        }
        else {
          new RefreshPredefinedSetupsDialog(EditorWindow.this).setVisible(true);
        }
      }
    });


    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/index.html").toURI().toURL();
      mm.addAction("Editor.ModuleEditor.table_of_contents", new ShowHelpAction("Editor.ModuleEditor.table_of_contents", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/editor.html").toURI().toURL();
      mm.addAction("Editor.ModuleEditor.using_the_editor", new ShowHelpAction("Editor.ModuleEditor.using_the_editor", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "designerguide/designerguide.pdf").toURI()
        .toURL();
      mm.addAction("Editor.ModuleEditor.designer_guide", new ShowHelpAction("Editor.ModuleEditor.designer_guide", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/GameModule.html").toURI().toURL();
      mm.addAction("Editor.ModuleEditor.module_components", new ShowHelpAction("Editor.ModuleEditor.module_components", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/Map.html").toURI().toURL();
      mm.addAction("Editor.ModuleEditor.map_components", new ShowHelpAction("Editor.ModuleEditor.map_components", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/Toolbar.html").toURI().toURL();
      mm.addAction("Editor.ModuleEditor.toolbar_help", new ShowHelpAction("Editor.ModuleEditor.toolbar_help", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/GamePiece.html").toURI()
        .toURL();
      mm.addAction("Editor.ModuleEditor.game_pieces_and_traits", new ShowHelpAction("Editor.ModuleEditor.game_pieces_and_traits", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/Expression.html").toURI()
        .toURL();
      mm.addAction("Editor.ModuleEditor.expressions", new ShowHelpAction("Editor.ModuleEditor.expressions", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(), "ReferenceManual/Properties.html").toURI()
        .toURL();

      //BR// I dunno why this different pattern was used here. But "thangs warn't right" in general (this was the buggiest entry in the 3.2.17 version of this, and the gif thing had no noticeable effect) so I've just made it work like the others.

      //final ShowHelpAction helpAction = new ShowHelpAction(url,
      //    helpWindow.getClass().getResource("/images/Help16.gif"));
      //helpAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("Editor.ModuleEditor.properties")); //$NON-NLS-1$
      //toolBar.add(helpAction);

      mm.addAction("Editor.properties", new ShowHelpAction("Editor.properties", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    mm.addAction("AboutScreen.about_vassal", new AboutVASSALAction(this));

    setJMenuBar(mm.getMenuBarFor(this));

    // the presence of the panel prevents a NullPointerException on packing
    final JPanel panel = new JPanel();
    panel.setPreferredSize(new Dimension(250, 400));

    scrollPane = new JScrollPane(panel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    add(scrollPane, BorderLayout.CENTER);
    pack();
  }

  /**
   * @param name Filename to check
   * @return true if this is a "temporary file" (according to the temp-file-making scheme of {@link VASSAL.tools.io.ZipArchive})
   */
  boolean isTempFile(String name) {
    return name == null ||
      name.isEmpty() ||
      ("tmp".equals(name.substring(0, 3)) && name.contains(".zip"));  //NON-NLS
  }

  void setModuleName(String name) {
    if (isTempFile(name)) {
      moduleName = Resources.getString("Editor.ModuleEditor.creating_new_module");
    }
    else {
      moduleName = name;
    }
    updateWindowTitle();
  }

  void setExtensionName(String name) {
    if (isTempFile(name)) {
      extensionName = Resources.getString("Editor.ExtensionEditor.creating_new_extension");
    }
    else {
      extensionName = name;
    }

    extensionName = name;
    updateWindowTitle();
  }

  void updateWindowTitle() {
  }

  protected MenuProxy findMenuProxy(String name, MenuBarProxy mb) {
    for (final ChildProxy<?> c : mb.getChildren()) {
      if (c instanceof MenuProxy) {
        final MenuProxy m = (MenuProxy) c;
        if (name.equals(m.getText()))
          return m;
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
      new ValidationReportDialog(report, new ValidationReportDialog.CallBack() {
        @Override
        public void ok() {
          save.run();
        }

        @Override
        public void cancel() {
        }
      }).setVisible(true);
    }
  }

  /**
   * Called by the enclosed ConfigureTree or ExtensionTree when it's dirty state
   * is changed. The implementing class should override this if they need to take
   * action like changing menu availability.
   *
   * @param changed true if the tree is in a changed (dirty) state
   */
  public void treeStateChanged(boolean changed) {
  }
}
