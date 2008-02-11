/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.configure;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateVassalWindow;
import VASSAL.tools.ScrollPane;

/**
 * The editing window for a module.
 * @deprecated This functionality has been moved to {@link EditorWindow}.
 */
@Deprecated
public class ModuleEditWindow extends JFrame implements WindowListener {
  private static final long serialVersionUID = 1L;

  protected HelpWindow helpWindow;
  protected JToolBar toolbar;
  protected ConfigureTree tree;

  public ModuleEditWindow() {
    helpWindow = new HelpWindow(Resources.getString("Editor.ModuleEditor.reference_manual"), null); //$NON-NLS-1$
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    addWindowListener(this);
    refreshTitle();
    GameModule.getGameModule().addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
          refreshTitle();
        }
      }
    });
    tree = new ConfigureTree(GameModule.getGameModule(), helpWindow);
    initComponents(new ScrollPane(tree));
  }

  protected void initComponents(Component view) {
    add(view);
    toolbar = new JToolBar();
    toolbar.setFloatable(false);
    SaveAction saveAction = new SaveAction() {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        ModuleEditWindow.this.save();
      }
    };
    toolbar.add(saveAction);
    SaveAsAction saveAsAction = new SaveAsAction() {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        ModuleEditWindow.this.saveAs();
      }
    };
    toolbar.add(saveAsAction);
    Action helpAction = null;
    try {
      File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
      dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
      helpAction = new ShowHelpAction(HelpFile.toURL(new File(dir, "index.htm")), helpWindow.getClass().getResource("/images/Help16.gif")); //$NON-NLS-1$ //$NON-NLS-2$
      helpAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("Editor.ModuleEditor.reference_manual")); //$NON-NLS-1$
      toolbar.add(helpAction);
    }
    catch (MalformedURLException e) {
      e.printStackTrace();
    }
    add(toolbar, BorderLayout.NORTH);
    JMenuBar mb = new JMenuBar();
    JMenuItem mi;
    JMenu fileMenu = new JMenu(Resources.getString(Resources.FILE));
    mi = new JMenuItem(Resources.getString("Editor.save")); //$NON-NLS-1$
    mi.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    mi.addActionListener(saveAction);
    fileMenu.add(mi);
    mi = new JMenuItem(Resources.getString("Editor.save_as")); //$NON-NLS-1$
    mi.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    mi.addActionListener(saveAsAction);
    fileMenu.add(mi);
    fileMenu.add(new CreateInstallerAction(this));
    mb.add(fileMenu);    
    mb.add(tree.getEditMenu());    
    if (helpAction != null) {
      JMenu helpMenu = new JMenu(Resources.getString(Resources.HELP));
      mi = new JMenuItem(Resources.getString("Editor.ModuleEditor.reference_manual")); //$NON-NLS-1$
      mi.addActionListener(helpAction);     
      helpMenu.add(mi);
      mi = new JMenuItem(tree.getHelpAction());
      helpMenu.add(mi);
      mb.add(helpMenu);
    }
    mb.add(createUpdateMenu());
    mb.add(createTranslateMenu());
    setJMenuBar(mb);
    pack();
  }

  protected JMenu createUpdateMenu() {
    JMenu updaterMenu = new JMenu(Resources.getString("Editor.ModuleEditor.updaters")); //$NON-NLS-1$
    JMenuItem mi = new JMenuItem(Resources.getString("Editor.ModuleEditor.create_module_updater")); //$NON-NLS-1$
    mi.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new ModuleUpdaterDialog(ModuleEditWindow.this).setVisible(true);
      }
    });
    updaterMenu.add(mi);
    mi = new JMenuItem(Resources.getString("Editor.ModuleEditor.update_saved")); //$NON-NLS-1$
    mi.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new SavedGameUpdaterDialog(ModuleEditWindow.this).setVisible(true);
      }
    });
    updaterMenu.add(mi);
    return updaterMenu;
  }
  
  protected JMenu createTranslateMenu() {
    JMenu translateMenu = new JMenu(Resources.getString("Editor.ModuleEditor.translate")); //$NON-NLS-1$
    JMenuItem mi = new JMenuItem(Resources.getString("Editor.ModuleEditor.translate_vassal")); //$NON-NLS-1$
    mi.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new TranslateVassalWindow(ModuleEditWindow.this).setVisible(true);
      }
    });
    translateMenu.add(mi);
    return translateMenu;
  }

  protected void saveAs() {
    ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(), report);
    if (report.getWarnings().size() == 0) {
      GameModule.getGameModule().saveAs();
    }
    else {
      new ValidationReportDialog(report, new ValidationReportDialog.CallBack() {
        public void ok() {
          GameModule.getGameModule().saveAs();
        }

        public void cancel() {
        }
      }).setVisible(true);
    }
  }

  protected void save() {
    ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(), report);
    if (report.getWarnings().size() == 0) {
      GameModule.getGameModule().save();
    }
    else {
      new ValidationReportDialog(report, new ValidationReportDialog.CallBack() {
        public void ok() {
          GameModule.getGameModule().save();
        }

        public void cancel() {
        }
      }).setVisible(true);
    }
  }

  protected void refreshTitle() {
    String configureName = GameModule.getGameModule().getConfigureName();
    if (configureName == null) {
      configureName = Resources.getString("Editor.ModuleEditor.coponent_type"); //$NON-NLS-1$
    }
    setTitle(Resources.getString("Editor.ModuleEditor.edit", configureName)); //$NON-NLS-1$
  }

  public void windowActivated(WindowEvent e) {
  }

  public void windowClosed(WindowEvent e) {
  }

  public void windowClosing(WindowEvent e) {
    GameModule.getGameModule().quit();
  }

  public void windowDeactivated(WindowEvent e) {
  }

  public void windowDeiconified(WindowEvent e) {
  }

  public void windowIconified(WindowEvent e) {
  }

  public void windowOpened(WindowEvent e) {
  }
}
