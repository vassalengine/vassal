/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
import java.net.MalformedURLException;

import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.ui.ShowServerStatusAction;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.ShowHelpAction;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.FileChooser;
import VASSAL.tools.imports.adc2.ImportADC2Action;

/**
 * The starting controls from which the user may load or edit any module
 * 
 * @author rodneykinney
 * 
 */
public class ConsoleControls {
  private static final long serialVersionUID = 1L;
  protected JPanel controls;
  protected FileChooser fc;
  protected ConsoleWindow console;
  protected javax.swing.JButton openButton;
  protected javax.swing.JButton editButton;
  protected javax.swing.JButton importButton;
  protected javax.swing.JButton newButton;
  protected javax.swing.JLabel splash;
  protected java.awt.Color fgColor = java.awt.Color.black;
  protected Runnable closeConsoleWindow;
  protected Runnable translateModule;
  protected Runnable showWelcomeWizard;
  protected Runnable showModuleControls;
  protected Runnable loadExtensions;

  public ConsoleControls(ConsoleWindow c) {
    this.console = c;
    closeConsoleWindow = new Runnable() {
      public void run() {
        console.getFrame().dispose();
      }
    };
    translateModule = new Runnable() {
      public void run() {
        try {
          Localization.getInstance().translate();
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    };
    showWelcomeWizard = new Runnable() {
      public void run() {
        GameModule.getGameModule().getWizardSupport().showWelcomeWizard();
      }
    };
    showModuleControls = new Runnable() {
      public void run() {
        GameModule.getGameModule().getFrame().setVisible(true);
      }
    };
    loadExtensions = new Runnable() {
      public void run() {
        new ExtensionsLoader().addTo(GameModule.getGameModule());
      }
    };
    initComponents();
    fc = FileChooser.createFileChooser(controls, (DirectoryConfigurer) Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));
  }

  protected void initComponents() {
    controls = new JPanel();
    controls.setLayout(new javax.swing.BoxLayout(controls, BoxLayout.Y_AXIS));
    splash = new javax.swing.JLabel();
    splash.setAlignmentX(0.5F);
    splash.setText(Resources.getString(Resources.VASSAL));
    splash.setFont(new java.awt.Font("SansSerif", 1, 48)); //$NON-NLS-1$
    splash.setName("splash"); //$NON-NLS-1$
    splash.setForeground(fgColor);
    controls.add(splash);
    JLabel l2 = new JLabel(Resources.getString("Main.version", VASSAL.Info.getVersion())); //$NON-NLS-1$
    l2.setAlignmentX(0.5F);
    l2.setFont(new java.awt.Font("SansSerif", 1, 12)); //$NON-NLS-1$
    l2.setForeground(fgColor);
    controls.add(l2);
    Box box = Box.createHorizontalBox();
    LoadModuleAction loadModuleAction = new LoadModuleAction(controls);
    loadModuleAction.addAction(closeConsoleWindow);
    loadModuleAction.addAction(translateModule);
    loadModuleAction.addAction(loadExtensions);
    loadModuleAction.addAction(showWelcomeWizard);
    openButton = new javax.swing.JButton(loadModuleAction);
    box.add(openButton);
    Action showStatusAction = new ShowServerStatusAction(new CgiServerStatus(), null);
    JButton statusButton = new JButton(showStatusAction);
    box.add(statusButton);
    JButton help;
    try {
      File readme = new File (Documentation.getDocumentationBaseDir(),"README.html");
      help = new JButton(new ShowHelpAction(readme.toURI().toURL(), null));
      box.add(help);
    }
    catch (MalformedURLException e) {
      e.printStackTrace();
    }
    controls.add(box);
    box = Box.createHorizontalBox();
    EditModuleAction editModuleAction = new EditModuleAction(controls);
    editModuleAction.addAction(closeConsoleWindow);
    editButton = new javax.swing.JButton(editModuleAction);
    box.add(editButton);
    ImportADC2Action importModuleAction = new ImportADC2Action(controls);
    importModuleAction.addAction(closeConsoleWindow);
    importButton = new javax.swing.JButton(importModuleAction);
    box.add(importButton);
    CreateModuleAction createModuleAction = new CreateModuleAction(controls);
    createModuleAction.addAction(closeConsoleWindow);
    newButton = new javax.swing.JButton(createModuleAction);
    box.add(newButton);
    box.setBorder(new TitledBorder(Resources.getString("Main.module_design"))); //$NON-NLS-1$
    controls.add(box);
    JPanel p = new JPanel();
    p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
    loadModuleAction = new LoadModuleAction(controls);
    loadModuleAction.putValue(Action.NAME, Resources.getString("Main.load_module"));
    loadModuleAction.addAction(showModuleControls);
    EditExtensionAction editExtensionAction = new EditExtensionAction(controls);
    editExtensionAction.addAction(closeConsoleWindow);
    editExtensionAction.setEnabled(false);
    NewExtensionAction newExtensionAction = new NewExtensionAction(controls);
    newExtensionAction.addAction(closeConsoleWindow);
    newExtensionAction.setEnabled(false);
    final JButton loadModule = new JButton(loadModuleAction); //$NON-NLS-1$
    final JButton loadExtension = new JButton(editExtensionAction);
    final JButton newExtension = new JButton(newExtensionAction);
    loadModuleAction.addAction(new Runnable() {
      public void run() {
        openButton.setEnabled(false);
        editButton.setEnabled(false);
        newButton.setEnabled(false);
        loadModule.setEnabled(false);
        loadExtension.setEnabled(true);
        newExtension.setEnabled(true);
      }
    });
    p.add(loadModule);
    p.add(loadExtension);
    p.add(newExtension);
    p.setBorder(new TitledBorder(Resources.getString("Main.extension_design"))); //$NON-NLS-1$
    controls.add(p);
  }

  public JComponent getControls() {
    return controls;
  }
}
